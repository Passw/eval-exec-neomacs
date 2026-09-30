//! Row-wise decode, for sources whose decoder can hand back part of a frame.
//!
//! A large image takes long enough to decode that the honest thing to do with
//! it is to show it arriving: rows become available from the top while the
//! bottom is still being read. [`BandSource`] is the route decision — a
//! row-wise decoder, or the all-or-nothing decode this renderer did before —
//! and [`BandedSource::next_band`] is the progress itself.
//!
//! Three properties this module is built to keep:
//!
//! - **Bands tile the source.** A source derives every band from the cursor it
//!   advances itself, and nothing can ask a source for a named range, so its
//!   bands cannot overlap, skip or arrive out of order. A band assembled
//!   elsewhere through [`BandChunk::from_rows`] is checked against the
//!   rectangle it claims, which is as much as a value can know on its own.
//! - **A banded decode agrees with the whole one.** Both decode through the
//!   same transforms and both land in the same RGBA, which is what lets a
//!   decode move between the two paths without the picture changing.
//! - **Failure is not truncation.** A row-wise decode that errors mid-stream
//!   stops at [`BandStep::Failed`]; its bands are abandoned and the caller
//!   decodes the whole image again. Only [`BandedSource::into_image`] on a
//!   completed source yields pixels.

use std::io::Cursor;
use std::num::NonZeroU32;
use std::sync::Arc;

use neomacs_display_protocol::{
    ImageIntrinsicExtent, ImageNativeExtent, ImageRealization, ImageRotation, ImageSizeSpec,
};

/// Smallest source, in pixels, that banding engages for.
///
/// Below this one decode is not visibly slow — the measured 36-megapixel PNG
/// takes 1.4 s here, so a four-megapixel source is tens of milliseconds — and
/// the whole-image path is both simpler and no slower. The threshold only has
/// to be the point where the first band visibly beats the last one.
pub(crate) const BANDING_MIN_PIXELS: u64 = 4_000_000;

/// Largest RGBA payload one band carries.
///
/// This is what bounds the number of bands from below: with no width to worry
/// about, a source arrives in at most `total_bytes / BAND_MAX_BYTES` bands, so
/// even a hundred-megapixel image bands into about a hundred pieces instead of
/// following its height into thousands.
pub(crate) const BAND_MAX_BYTES: usize = 4 * 1024 * 1024;

/// Most bands one source is planned to produce.
///
/// One band per scanline would be progressive in name only: the first band
/// would arrive almost as late as the last, because a decoder still has to
/// produce the rows above it.
pub(crate) const BAND_TARGET_COUNT: u32 = 32;

/// Receives each band as the decode produces it.
///
/// A sink rather than a returned collection: a band is worth having before the
/// next one exists, which is the whole point of decoding this way. `None`
/// decodes the same pixels without reporting progress, for the callers that
/// only want the image.
pub(crate) type BandSink<'sink> = Option<&'sink mut dyn FnMut(BandChunk)>;

/// A run of whole source rows, in decode order.
///
/// Built only by the source that hands it out, from the cursor that source
/// advances; `len` is non-zero so an empty band cannot be published.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RowRange {
    start: u32,
    len: NonZeroU32,
}

impl RowRange {
    fn new(start: u32, len: NonZeroU32) -> Self {
        Self { start, len }
    }

    /// First source row this range covers.
    #[must_use]
    pub const fn start(self) -> u32 {
        self.start
    }

    /// How many source rows it covers; never zero.
    #[must_use]
    pub const fn len(self) -> NonZeroU32 {
        self.len
    }

    /// One past the last source row it covers.
    #[must_use]
    pub const fn end(self) -> u32 {
        self.start + self.len.get()
    }
}

/// One band of a banded decode: the rows it covers and the pixels for them.
///
/// The pixels are a copy of those rows rather than a window into the source's
/// own buffer, so a consumer may hold, upload or drop a band while the decode
/// that produced it keeps filling the rest of the image.
#[derive(Clone, Eq, PartialEq)]
pub struct BandChunk {
    rows: RowRange,
    width: u32,
    pixels: Arc<[u8]>,
}

impl BandChunk {
    /// Assemble a band from rows and their pixels.
    ///
    /// The decoding path does not need this — a source builds its own bands
    /// from the cursor it advances, which is what keeps them disjoint and in
    /// order. This is for a band that arrives from somewhere else: another
    /// decoder, or a test that needs a band without one. `None` when the
    /// pixels are not exactly `rows` of `width` RGBA texels, so a band always
    /// describes the rectangle it claims to.
    #[must_use]
    pub fn from_rows(
        start: u32,
        rows: NonZeroU32,
        width: u32,
        pixels: impl Into<Arc<[u8]>>,
    ) -> Option<Self> {
        let pixels = pixels.into();
        let expected = width as usize * rows.get() as usize * 4;
        (pixels.len() == expected).then(|| Self {
            rows: RowRange::new(start, rows),
            width,
            pixels,
        })
    }

    /// The source rows this band carries.
    #[must_use]
    pub const fn rows(&self) -> RowRange {
        self.rows
    }

    /// Source width. Every band spans the full width; bands are rows only.
    #[must_use]
    pub const fn width(&self) -> u32 {
        self.width
    }

    /// RGBA pixels, `width * rows.len() * 4` bytes, row-major from
    /// `rows.start()`.
    #[must_use]
    pub fn pixels(&self) -> &[u8] {
        &self.pixels
    }
}

/// The pixels are deliberately not in the printed form: a band is megabytes of
/// RGBA, and one `{:?}` of a published terminal — the failure message of an
/// `assert_eq!` on one, say — would carry all of them into the log.
impl std::fmt::Debug for BandChunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BandChunk")
            .field("rows", &self.rows)
            .field("width", &self.width)
            .field("pixels", &format_args!("{} bytes", self.pixels.len()))
            .finish()
    }
}

/// What one turn of a banded source produced.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum BandStep {
    /// The next band, in order, once.
    Band(BandChunk),
    /// Every source row has been handed out.
    Done,
    /// The decoder failed part-way through. The bands already handed out are
    /// still what they said they were, but this source can never finish: a
    /// caller that wants a whole image must abandon it and decode the source
    /// again on the whole-image path.
    Failed,
}

/// Where an encoded source's rows come from.
///
/// Matched exhaustively wherever a source is consumed: a third route has to be
/// a deliberate addition rather than something a wildcard swallows.
pub(crate) enum BandSource<'a> {
    /// Whole source rows, in order, one band at a time.
    Banded(BandedSource<'a>),
    /// One all-or-nothing decode of the whole image — this renderer's
    /// behaviour before banding existed. Every format whose decoder has no
    /// row-wise entry point lands here, and so does every source too small for
    /// the decode to be visibly slow.
    Whole,
}

/// The row-wise decoders, one variant per format that has one.
///
/// This variant set is the compile-time answer to "which formats can band?":
/// a format added here fails to build in every match over it until its
/// decoder is written. That property cannot be had from `image::ImageFormat`,
/// which is `#[non_exhaustive]` and so needs a wildcard arm; the format match
/// in [`BandSource::open`] names every format `image` reports and falls back
/// for formats `image` might add, and this enum is where a *banded* addition
/// is caught.
pub(crate) enum BandedSource<'a> {
    /// PNG, whose reader yields one transformed row at a time
    /// (`png-0.18.1` `src/decoder/mod.rs:513` `next_row`).
    Png(PngRows<'a>),
}

impl<'a> BandedSource<'a> {
    /// The source's native extent, in source pixels.
    #[must_use]
    pub(crate) fn dimensions(&self) -> (u32, u32) {
        match self {
            Self::Png(rows) => rows.dimensions(),
        }
    }

    /// Take the next band, or learn that there are no more.
    pub(crate) fn next_band(&mut self) -> BandStep {
        match self {
            Self::Png(rows) => rows.next_band(),
        }
    }

    /// The whole image, once every row has been handed out.
    ///
    /// `None` before then, so a caller cannot publish a prefix by accident.
    #[must_use]
    pub(crate) fn into_image(self) -> Option<(u32, u32, Vec<u8>)> {
        match self {
            Self::Png(rows) => rows.into_image(),
        }
    }
}

impl<'a> BandSource<'a> {
    /// Classify an encoded source and, when banding applies, open its row-wise
    /// decoder.
    ///
    /// `size` and `realization` are the realization the image will be drawn
    /// through, and they only size the bands: how many source rows one display
    /// row is worth. Rotation is deliberately not an input — GNU turns the
    /// image after sizing (`src/image.c:3169-3201`), so the unrotated geometry
    /// is the one whose vertical scale maps source rows onto display rows.
    pub(crate) fn open(data: &'a [u8], size: ImageSizeSpec, realization: ImageRealization) -> Self {
        Self::open_at_least(data, size, realization, BANDING_MIN_PIXELS)
    }

    /// The same, with the size threshold forced to zero.
    ///
    /// The threshold is a policy about when banding is *worth it*, not about
    /// what banding does, so the tests that are about what it does read a
    /// small source rather than encode a four-megapixel one.
    #[cfg(test)]
    pub(crate) fn open_forced(
        data: &'a [u8],
        size: ImageSizeSpec,
        realization: ImageRealization,
    ) -> Self {
        Self::open_at_least(data, size, realization, 0)
    }

    fn open_at_least(
        data: &'a [u8],
        size: ImageSizeSpec,
        realization: ImageRealization,
        min_pixels: u64,
    ) -> Self {
        match image::guess_format(data) {
            Ok(image::ImageFormat::Png) => {
                PngRows::open(data, BandPlan::new(size, realization), min_pixels)
                    .map_or(Self::Whole, |rows| Self::Banded(BandedSource::Png(rows)))
            }
            // Every other format `image` reports, named so the route per format
            // is a line of code rather than a default. None of them can band:
            // `image` decodes JPEG with `zune-jpeg`, whose public surface is
            // `decode`/`decode_into` — a whole frame, optionally into a
            // caller-provided buffer (`zune-jpeg-0.5.15` `src/decoder.rs:215`,
            // `:827`) — with no row-wise entry point at all. Baseline JPEG is
            // row-wise decodable as a format; the pinned decoder does not
            // expose it. GIF, WebP, TIFF, BMP, ICO, TGA, DDS, PNM, HDR,
            // OpenEXR, farbfeld, AVIF and QOI are whole-image decoders in
            // `image` likewise.
            Ok(
                image::ImageFormat::Jpeg
                | image::ImageFormat::Gif
                | image::ImageFormat::WebP
                | image::ImageFormat::Pnm
                | image::ImageFormat::Tiff
                | image::ImageFormat::Tga
                | image::ImageFormat::Dds
                | image::ImageFormat::Bmp
                | image::ImageFormat::Ico
                | image::ImageFormat::Hdr
                | image::ImageFormat::OpenExr
                | image::ImageFormat::Farbfeld
                | image::ImageFormat::Avif
                | image::ImageFormat::Qoi,
            ) => Self::Whole,
            // `ImageFormat` is `#[non_exhaustive]`, so this arm is mandatory
            // whether or not a format is left to name. Both the formats hid
            // behind it and the sources `image` cannot identify at all (XPM,
            // XBM and SVG, which the caller's own fallbacks decode) route
            // whole, which is the right answer for every one of them.
            Ok(_) | Err(_) => Self::Whole,
        }
    }
}

/// How the bands of one source are sized.
///
/// The realized geometry, kept as the spec it was resolved from: the source's
/// native extent is only known once its header has been read, and the display
/// row a band has to cover is derived from both.
#[derive(Clone, Copy, Debug)]
struct BandPlan {
    size: ImageSizeSpec,
    realization: ImageRealization,
}

impl BandPlan {
    const fn new(size: ImageSizeSpec, realization: ImageRealization) -> Self {
        Self { size, realization }
    }

    /// Rows one band of a `width` x `height` source covers.
    ///
    /// Three bounds meet, and the largest floor under the smallest ceiling
    /// wins:
    ///
    /// - A display row's worth of source pixels, because a band is drawn as a
    ///   sub-rect of the image and one that does not cover a display row
    ///   cannot advance it.
    /// - A [`BAND_TARGET_COUNT`]th of the height, because the first band
    ///   should arrive early rather than after most of the decode.
    /// - [`BAND_MAX_BYTES`] of RGBA, because a very wide source would
    ///   otherwise band into tens of megabytes at a time.
    ///
    /// The height floor and the byte ceiling govern in practice: a source
    /// large enough to band is usually shown at or below its own size, where
    /// one display row is a single source row.
    fn band_rows(self, width: u32, height: u32) -> NonZeroU32 {
        let display = self.rows_per_display_row(width, height).get();
        let counted = height.div_ceil(BAND_TARGET_COUNT).max(1);
        let by_bytes = (BAND_MAX_BYTES / (width.max(1) as usize * 4)).max(1);
        let by_bytes = u32::try_from(by_bytes).unwrap_or(u32::MAX);
        let rows = display.max(counted).min(by_bytes).max(1).min(height.max(1));
        NonZeroU32::new(rows).unwrap_or(NonZeroU32::MIN)
    }

    /// Source rows behind one display row at this realization: one at native
    /// size or magnified, more when the source is minified onto fewer rows.
    fn rows_per_display_row(self, width: u32, height: u32) -> NonZeroU32 {
        let layout = self
            .realization
            .resolve_geometry(
                self.size,
                ImageIntrinsicExtent::from(ImageNativeExtent::new(width, height)),
                ImageRotation::None,
            )
            .layout();
        NonZeroU32::new(height.div_ceil(layout.height().max(1))).unwrap_or(NonZeroU32::MIN)
    }
}

#[cfg(test)]
#[path = "image_bands/tests.rs"]
mod tests;

/// A PNG being read one row at a time.
pub(crate) struct PngRows<'a> {
    reader: png::Reader<Cursor<&'a [u8]>>,
    format: PngRowFormat,
    width: u32,
    height: u32,
    /// Rows one band carries. The last band of a source is shorter.
    band: NonZeroU32,
    /// The whole image, filled a band at a time.
    rgba: Vec<u8>,
    /// Rows filled so far. Every band is derived from this cursor.
    decoded: u32,
    /// Set once the reader has failed, so later calls cannot report progress a
    /// caller might read as a successful continuation.
    failed: bool,
}

impl<'a> PngRows<'a> {
    /// Open `data` for row-wise reading, or decline.
    ///
    /// `None` hands the source back to the whole-image path, and means one of:
    /// the header would not parse, the output is not one of the 8-bit colour
    /// types below, the source is interlaced, or it is too small for banding to
    /// pay ([`BANDING_MIN_PIXELS`]).
    fn open(data: &'a [u8], plan: BandPlan, min_pixels: u64) -> Option<Self> {
        let mut decoder = png::Decoder::new(Cursor::new(data));
        // The transform `image`'s own PNG decoder sets before reading
        // (`image-0.25.10` `src/codecs/png.rs:60`). Both paths decoding through
        // it is what makes a banded decode and a whole decode of the same file
        // agree pixel for pixel.
        decoder.set_transformations(png::Transformations::EXPAND);
        let reader = decoder.read_info().ok()?;
        let (width, height) = (reader.info().width, reader.info().height);
        // Adam7 rows are partial rows, and the crate keeps the pass and line
        // geometry that would reassemble them private
        // (`InterlaceInfo::line_number`), so an interlaced source is read
        // whole rather than guessed at.
        if reader.info().interlaced {
            return None;
        }
        let format = PngRowFormat::of(reader.output_color_type())?;
        if u64::from(width) * u64::from(height) < min_pixels {
            return None;
        }
        Some(Self {
            rgba: vec![0; width as usize * height as usize * 4],
            width,
            height,
            band: plan.band_rows(width, height),
            format,
            reader,
            decoded: 0,
            failed: false,
        })
    }

    #[must_use]
    pub(crate) fn dimensions(&self) -> (u32, u32) {
        (self.width, self.height)
    }

    fn next_band(&mut self) -> BandStep {
        if self.failed {
            return BandStep::Failed;
        }
        if self.decoded >= self.height {
            return BandStep::Done;
        }
        let rows = self.band.get().min(self.height - self.decoded);
        let start = self.decoded;
        for row in start..start + rows {
            if self.read_row(row).is_err() {
                self.failed = true;
                return BandStep::Failed;
            }
        }
        self.decoded = start + rows;
        let rows = RowRange::new(start, NonZeroU32::new(rows).unwrap_or(NonZeroU32::MIN));
        let stride = self.width as usize * 4;
        let pixels: Arc<[u8]> =
            self.rgba[rows.start() as usize * stride..rows.end() as usize * stride].into();
        BandStep::Band(BandChunk {
            rows,
            width: self.width,
            pixels,
        })
    }

    /// Read one source row into its place in the image.
    fn read_row(&mut self, row: u32) -> Result<(), ()> {
        let Self {
            reader,
            format,
            width,
            rgba,
            ..
        } = self;
        let format = *format;
        let Some(source) = reader.next_row().map_err(|_| ())? else {
            // The reader ended before the header's height did: a truncated
            // stream, which is exactly the mid-stream failure the caller has to
            // be able to recover from.
            return Err(());
        };
        let stride = *width as usize * 4;
        let target = &mut rgba[row as usize * stride..(row as usize + 1) * stride];
        format.expand_row(source.data(), target).ok_or(())
    }

    fn into_image(self) -> Option<(u32, u32, Vec<u8>)> {
        if self.failed || self.decoded != self.height {
            return None;
        }
        Some((self.width, self.height, self.rgba))
    }
}

/// The 8-bit output colour types a PNG row can arrive in.
///
/// `Transformations::EXPAND` lifts grayscale below 8 bits and palettes (with
/// or without `tRNS`) into these, leaving 16-bit output as the one thing to
/// decline — its rows are big-endian and `image` reorders them only inside its
/// own decoder, so a row-wise path would have to reimplement that to stay
/// byte-exact. 16-bit PNGs are rare enough to read whole.
#[derive(Clone, Copy, Debug)]
enum PngRowFormat {
    Gray8,
    GrayAlpha8,
    Rgb8,
    Rgba8,
}

impl PngRowFormat {
    fn of((color, depth): (png::ColorType, png::BitDepth)) -> Option<Self> {
        match (color, depth) {
            (png::ColorType::Grayscale, png::BitDepth::Eight) => Some(Self::Gray8),
            (png::ColorType::GrayscaleAlpha, png::BitDepth::Eight) => Some(Self::GrayAlpha8),
            (png::ColorType::Rgb, png::BitDepth::Eight) => Some(Self::Rgb8),
            (png::ColorType::Rgba, png::BitDepth::Eight) => Some(Self::Rgba8),
            _ => None,
        }
    }

    /// Widen one row to RGBA, the conversion `image`'s `to_rgba8` applies to
    /// the same colour type. `None` means the row was not the length the
    /// colour type promised.
    fn expand_row(self, source: &[u8], target: &mut [u8]) -> Option<()> {
        let pixels = target.len() / 4;
        let sized = |bytes: usize| (source.len() == pixels * bytes).then_some(());
        match self {
            Self::Rgba8 => {
                sized(4)?;
                target.copy_from_slice(source);
            }
            Self::Rgb8 => {
                sized(3)?;
                for (target, source) in target.chunks_exact_mut(4).zip(source.chunks_exact(3)) {
                    target.copy_from_slice(&[source[0], source[1], source[2], 0xff]);
                }
            }
            Self::Gray8 => {
                sized(1)?;
                for (target, source) in target.chunks_exact_mut(4).zip(source) {
                    target.copy_from_slice(&[*source, *source, *source, 0xff]);
                }
            }
            Self::GrayAlpha8 => {
                sized(2)?;
                for (target, source) in target.chunks_exact_mut(4).zip(source.chunks_exact(2)) {
                    target.copy_from_slice(&[source[0], source[0], source[0], source[1]]);
                }
            }
        }
        Some(())
    }
}
