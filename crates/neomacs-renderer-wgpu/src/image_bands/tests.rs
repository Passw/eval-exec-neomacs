//! What a banded decode promises its consumer.
//!
//! Three contracts, in the order a consumer meets them: the bands tile the
//! source, the pixels they carry are the pixels the whole-image path would
//! produce for the same file, and a decode that cannot finish says so instead
//! of stopping quietly.

use super::*;
use std::io::Cursor;

use neomacs_display_protocol::{
    ImageHeuristicMask, ImageMaskPolicy, ImageRealization, ImageSizeSpec,
};

/// A source banded regardless of size, so these tests can use small images.
fn open_banded(data: &[u8]) -> BandSource<'_> {
    BandSource::open_forced(data, ImageSizeSpec::default(), ImageRealization::default())
}

/// Drive a banded source to completion, collecting what it published.
///
/// Returns the bands and the image, and panics on a failure — the tests that
/// expect one drive the source themselves.
fn drain(mut source: BandedSource<'_>) -> (Vec<BandChunk>, Option<(u32, u32, Vec<u8>)>) {
    let mut bands = Vec::new();
    loop {
        match source.next_band() {
            BandStep::Band(band) => bands.push(band),
            BandStep::Done => return (bands, source.into_image()),
            BandStep::Failed => panic!("a valid source must not fail"),
        }
    }
}

/// The pixels the whole-image path produces for `data`, through `image`'s own
/// decode and colour conversion — the reference a banded decode has to match.
fn whole_image_pixels(data: &[u8]) -> (u32, u32, Vec<u8>) {
    let image = image::load_from_memory(data).expect("fixture decodes whole");
    let rgba = image.to_rgba8();
    (rgba.width(), rgba.height(), rgba.into_raw())
}

/// One pixel per `(x, y)`, so a band placed at the wrong offset is visible
/// rather than hidden by identical rows.
fn varying_pixels(width: u32, height: u32) -> Vec<u8> {
    (0..height)
        .flat_map(|y| {
            (0..width).flat_map(move |x| {
                [
                    (x % 251) as u8,
                    (y % 253) as u8,
                    ((x + y) % 241) as u8,
                    0xff,
                ]
            })
        })
        .collect()
}

fn png_of(width: u32, height: u32, pixels: Vec<u8>) -> Vec<u8> {
    let image = image::RgbaImage::from_raw(width, height, pixels).expect("pixel buffer");
    let mut bytes = Cursor::new(Vec::new());
    image::DynamicImage::ImageRgba8(image)
        .write_to(&mut bytes, image::ImageFormat::Png)
        .expect("PNG is encodable");
    bytes.into_inner()
}

/// A PNG of `width` x `height` with a distinct colour per pixel.
fn varying_png(width: u32, height: u32) -> Vec<u8> {
    png_of(width, height, varying_pixels(width, height))
}

#[test]
fn bands_tile_the_source_in_order_and_cover_every_row_once() {
    let (width, height) = (120, 500);
    let data = varying_png(width, height);
    let BandSource::Banded(source) = open_banded(&data) else {
        panic!("a PNG has a row-wise decoder");
    };
    assert_eq!(source.dimensions(), (width, height));

    let (bands, image) = drain(source);

    assert!(
        bands.len() > 1,
        "a 500-row source must band into more than one piece, got {}",
        bands.len()
    );
    let mut expected_start = 0;
    for band in &bands {
        assert_eq!(
            band.rows().start(),
            expected_start,
            "bands arrive in order, each where the last one ended"
        );
        assert_eq!(band.width(), width);
        assert_eq!(
            band.pixels().len(),
            width as usize * band.rows().len().get() as usize * 4,
            "a band carries exactly the rows it claims"
        );
        expected_start = band.rows().end();
    }
    assert_eq!(expected_start, height, "the bands cover every row once");

    let Some((image_width, image_height, rgba)) = image else {
        panic!("a source that reached Done yields the image");
    };
    assert_eq!((image_width, image_height), (width, height));
    assert_eq!(
        rgba,
        varying_pixels(width, height),
        "the assembled image is the source's pixels"
    );
}

#[test]
fn the_pixels_a_band_carries_are_where_it_says_they_are() {
    let (width, height) = (32, 200);
    let data = varying_png(width, height);
    let BandSource::Banded(source) = open_banded(&data) else {
        panic!("a PNG has a row-wise decoder");
    };
    let (bands, _) = drain(source);

    let stride = width as usize * 4;
    for band in &bands {
        let expected = &varying_pixels(width, height)
            [band.rows().start() as usize * stride..band.rows().end() as usize * stride];
        assert_eq!(
            band.pixels(),
            expected,
            "band at row {} carries those rows, not others",
            band.rows().start()
        );
    }
}

#[test]
fn a_banded_decode_produces_the_whole_image_paths_pixels_for_every_row_format() {
    // One fixture per colour type that reaches the decoder as 8-bit output:
    // `Transformations::EXPAND` widens the sub-8-bit and paletted ones.
    let width = 23;
    let height = 61;
    let cases: Vec<(&str, Vec<u8>)> = vec![
        ("gray8", {
            let raw: Vec<u8> = (0..width * height).map(|i| (i % 251) as u8).collect();
            png_from(
                image::GrayImage::from_raw(width, height, raw)
                    .unwrap()
                    .into(),
            )
        }),
        ("gray-alpha8", {
            let raw: Vec<u8> = (0..width * height * 2).map(|i| (i % 249) as u8).collect();
            png_from(
                image::ImageBuffer::<image::LumaA<u8>, _>::from_raw(width, height, raw)
                    .unwrap()
                    .into(),
            )
        }),
        ("rgb8", {
            let raw: Vec<u8> = (0..width * height * 3).map(|i| (i % 247) as u8).collect();
            png_from(
                image::ImageBuffer::<image::Rgb<u8>, _>::from_raw(width, height, raw)
                    .unwrap()
                    .into(),
            )
        }),
        ("rgba8", varying_png(width, height)),
        // 1-bit and 4-bit grayscale, and a palette with transparency, are the
        // shapes `EXPAND` exists to lift into 8-bit output.
        (
            "gray1",
            png_with_depth(
                width,
                height,
                png::BitDepth::One,
                png::ColorType::Grayscale,
                |x| u8::from(x % 2 == 0),
            ),
        ),
        (
            "gray4",
            png_with_depth(
                width,
                height,
                png::BitDepth::Four,
                png::ColorType::Grayscale,
                |x| (x % 16) as u8,
            ),
        ),
        (
            "palette",
            png_paletted(
                width,
                height,
                vec![0x10, 0x20, 0x30, 0x40, 0x50, 0x60],
                None,
            ),
        ),
        // A palette with tRNS is translated to RGBA, not RGB, by EXPAND.
        (
            "palette+trns",
            png_paletted(
                width,
                height,
                vec![0x10, 0x20, 0x30, 0x40, 0x50, 0x60],
                Some(vec![0x00, 0x80]),
            ),
        ),
    ];

    for (name, data) in cases {
        let BandSource::Banded(source) = open_banded(&data) else {
            panic!("{name}: an 8-bit PNG has a row-wise decoder");
        };
        let (bands, image) = drain(source);
        assert!(!bands.is_empty(), "{name}: bands were produced");
        let Some(banded) = image else {
            panic!("{name}: a completed source yields the image");
        };
        assert_eq!(banded.0, width, "{name}: the decoded width");
        assert_eq!(banded.1, height, "{name}: the decoded height");
        assert_eq!(
            banded,
            whole_image_pixels(&data),
            "{name}: a banded decode must produce the whole decode's pixels"
        );
    }
}

#[test]
fn a_sixteen_bit_png_declines_banding_rather_than_re_deriving_its_endianness() {
    let width = 9;
    let height = 4;
    let raw: Vec<u16> = (0..width * height * 4).map(|i| (i * 997) as u16).collect();
    let data = png_from(
        image::ImageBuffer::<image::Rgba<u16>, _>::from_raw(width, height, raw)
            .unwrap()
            .into(),
    );

    assert!(
        matches!(open_banded(&data), BandSource::Whole),
        "16-bit output is read whole"
    );
    // The control: the same source without the extra bit depth bands.
    assert!(matches!(
        open_banded(&varying_png(width, height)),
        BandSource::Banded(_)
    ));
}

/// Adam7 rows are partial rows, so an interlaced source has no bands to take.
/// The `png` encoder cannot write one, so the flag is set on an otherwise
/// valid file with a corrected IHDR checksum — enough for the rule this pins,
/// which is decided from the header alone.
#[test]
fn an_interlaced_png_declines_banding() {
    let mut data = varying_png(8, 8);
    // IHDR: 8-byte signature, 4-byte length, 4-byte type, then the data whose
    // thirteenth byte is the interlace method.
    let interlace = 8 + 4 + 4 + 12;
    assert_eq!(data[interlace], 0, "the fixture is not interlaced to begin");
    data[interlace] = 1;
    let crc = crc32(&data[8 + 4..8 + 4 + 4 + 13]);
    data[8 + 4 + 4 + 13..8 + 4 + 4 + 17].copy_from_slice(&crc.to_be_bytes());

    assert!(
        matches!(open_banded(&data), BandSource::Whole),
        "interlaced rows are read whole"
    );
}

/// A source that runs out mid-stream fails rather than reporting the rows it
/// managed to read as the image.
#[test]
fn a_truncated_png_fails_mid_stream_and_yields_no_image() {
    let (width, height) = (64, 600);
    let data = varying_png(width, height);
    // Keep the header and part of the image data: enough rows to publish
    // bands, not enough to finish.
    let truncated = &data[..data.len() / 2];

    let BandSource::Banded(mut source) = open_banded(truncated) else {
        panic!("the header of a truncated PNG still parses");
    };
    let mut bands = Vec::new();
    loop {
        match source.next_band() {
            BandStep::Band(band) => bands.push(band),
            BandStep::Done => panic!("a truncated source cannot complete"),
            BandStep::Failed => break,
        }
    }

    let covered = bands.last().map_or(0, |band| band.rows().end());
    assert!(
        covered > 0 && covered < height,
        "the failure is mid-stream: {covered} of {height} rows"
    );
    assert!(
        source.into_image().is_none(),
        "an unfinished decode must not hand back a prefix as the image"
    );
}

/// A band has a destination only where the texture can be built up from the
/// top: an unrotated realization whose mask policy leaves the pixels alone.
/// Both exceptions are about the realization rather than the band, and both
/// leave the band what step 2 made it — progress, with nowhere to go.
#[test]
fn a_band_has_a_destination_only_where_the_texture_can_be_filled_from_the_top() {
    assert_eq!(
        BandFilling::of(ImageRotation::None, ImageMaskPolicy::Preserve),
        BandFilling::TopDown,
    );
    for rotation in [
        ImageRotation::Quarter,
        ImageRotation::Half,
        ImageRotation::ThreeQuarter,
    ] {
        assert_eq!(
            BandFilling::of(rotation, ImageMaskPolicy::Preserve),
            BandFilling::Deferred,
            "a {rotation:?} turn moves a band's rows out of the raster's rows",
        );
    }
    for mask in [
        ImageMaskPolicy::Suppress,
        ImageMaskPolicy::Heuristic(ImageHeuristicMask::FourCorners),
        ImageMaskPolicy::Heuristic(ImageHeuristicMask::Rgb16([0x12, 0x34, 0x56])),
    ] {
        assert_eq!(
            BandFilling::of(ImageRotation::None, mask),
            BandFilling::Deferred,
            "a {mask:?} mask rewrites pixels and needs all of them first",
        );
    }
}

/// A band of `rows` rows of `width` pixels, for the mapping tests: the pixels
/// are all zero, because where they land is the question and what they contain
/// is not.
fn band_of_rows(start: u32, rows: u32, width: u32) -> BandChunk {
    BandChunk::from_rows(
        start,
        NonZeroU32::new(rows).expect("a test band has rows"),
        width,
        vec![0u8; width as usize * rows as usize * 4],
    )
    .expect("a band of exactly the rows it claims")
}

/// The bands of one decode have to fill its texture from the top: contiguous,
/// in order, and reaching the raster's last row. A source row boundary rarely
/// lands on a texture row, so the boundaries are the case that decides the
/// rule — and covering the raster with no hole is what lets the display side
/// describe how far the image has come with one number.
#[test]
fn the_bands_of_a_source_tile_the_raster_from_row_zero() {
    // 700 source rows onto 238 raster rows (a 12000x700 image clamped to the
    // texture limit): 0.34 raster rows per source row, so nearly every band
    // boundary falls between two of them.
    let raster = ImageRasterExtent::new(4096, 238);
    let map = BandMap::new(700, raster).expect("a source with rows has a map");
    assert_eq!(map.raster(), raster);

    let mut expected_start = 0;
    for start in (0..700).step_by(22) {
        let rows = (700 - start).min(22);
        let placed = map
            .place(&band_of_rows(start, rows, 8))
            .expect("a 22-row band covers several texture rows");
        let placement = placed.placement();
        assert_eq!(
            placement.rows().start(),
            expected_start,
            "a band starts where the last one ended"
        );
        assert_eq!(placement.raster(), raster);
        assert_eq!(
            placed.pixels().len(),
            raster.width() as usize * placement.rows().len().get() as usize * 4,
            "a placed band carries exactly the rows it fills"
        );
        expected_start = placement.rows().end();
    }
    assert_eq!(
        expected_start,
        raster.height(),
        "the bands reach the last row of the raster"
    );
}

/// A boundary between two texture rows belongs to the band that starts there:
/// rounding it to the nearest row is what keeps consecutive bands adjacent
/// instead of leaving the boundary row to neither of them.
#[test]
fn a_band_boundary_between_two_texture_rows_belongs_to_the_band_that_starts_there() {
    let map = BandMap::new(700, ImageRasterExtent::new(4096, 238)).expect("map");
    // 33 * 238 / 700 = 11.22: nearer row 11 than row 12.
    let second = map
        .place(&band_of_rows(33, 11, 1))
        .expect("the second band has rows");
    assert_eq!(second.placement().rows().start(), 11);

    let first = map
        .place(&band_of_rows(0, 33, 1))
        .expect("the first band has rows");
    assert_eq!(
        first.placement().rows().end(),
        11,
        "the band before the boundary ends exactly where the next begins"
    );
}

/// A source whose rows map one to one onto the raster — the case of an image
/// shown at its own size — places every band on exactly its own rows.
#[test]
fn a_band_of_a_native_size_image_fills_the_rows_it_covered() {
    let map = BandMap::new(600, ImageRasterExtent::new(600, 600)).expect("map");
    let placed = map
        .place(&band_of_rows(120, 30, 4))
        .expect("a 30-row band of 600");
    assert_eq!(placed.placement().rows().start(), 120);
    assert_eq!(placed.placement().rows().len().get(), 30);
}

/// A source minified so far that a band covers less than one texture row has
/// nowhere to write, which is a band that places nowhere rather than a band
/// written at the wrong place.
#[test]
fn a_band_that_covers_no_texture_row_places_nothing() {
    // 40000 source rows onto 10 raster rows: one raster row per 4000 source
    // rows, so a 100-row band can round to a single row... and a 1-row band to
    // the row it rounds to, which is the floor below.
    let map = BandMap::new(40000, ImageRasterExtent::new(10, 10)).expect("map");
    let placed = map.place(&band_of_rows(0, 100, 1));
    // Rounding sends rows 0..100 to 0..0, so there is nothing to write.
    assert!(
        placed.is_none(),
        "a band worth less than a texture row places nothing"
    );
    // The control: the same source with wider bands does place rows.
    assert!(map.place(&band_of_rows(0, 4000, 1)).is_some());
}

/// Whatever the source, a band says which rows it is: the constructor refuses
/// pixels that are not that rectangle.
#[test]
fn a_band_refuses_pixels_that_are_not_the_rows_it_claims() {
    let rows = NonZeroU32::new(2).expect("non-zero");
    assert!(BandChunk::from_rows(0, rows, 4, vec![0u8; 4 * 2 * 4]).is_some());
    assert!(BandChunk::from_rows(0, rows, 4, vec![0u8; 4 * 2 * 4 - 1]).is_none());
    assert!(BandChunk::from_rows(0, rows, 4, vec![0u8; 4 * 2 * 4 + 1]).is_none());
}

/// The size a band covers follows the source and the byte cap, not the
/// scanline: a tall source bands into tens of pieces, a wide one into bigger
/// pieces rather than megabytes of them.
#[test]
fn band_rows_are_bounded_by_the_display_row_the_count_and_the_bytes() {
    let plan = BandPlan::new(ImageSizeSpec::default(), ImageRealization::default());
    let display_row = NonZeroU32::new(1).unwrap();

    // A tall, narrow source: a thirtieth of its height is far more than one
    // display row, and its rows are small enough for the byte cap not to bind.
    assert_eq!(plan.band_rows(100, 2000), NonZeroU32::new(63).unwrap());

    // The byte cap binds on a wide source: 4 MiB of RGBA at 20000 pixels
    // across is 52 rows, fewer than a thirtieth of 20000.
    assert_eq!(plan.band_rows(20_000, 20_000), NonZeroU32::new(52).unwrap());

    // A source below the count floor bands per row, which is what the floor
    // means at that size; banding itself never engages this small.
    assert_eq!(plan.band_rows(10, 4), NonZeroU32::new(1).unwrap());

    // Magnified or at native size: the display row is a single source row, so
    // the count floor decides, and the plan never asks for zero rows.
    assert_eq!(
        plan.rows_per_display_row(10, 4).get(),
        display_row.get(),
        "a source shown at its own size has one source row per display row"
    );
}

/// The source rows behind a display row: more when the source is shown
/// smaller than it is, one when it is shown at its own size or larger.
#[test]
fn the_band_plan_scales_source_rows_to_display_rows() {
    let at_native = BandPlan::new(ImageSizeSpec::default(), ImageRealization::default());
    assert_eq!(at_native.rows_per_display_row(1000, 1000).get(), 1);

    let minified = BandPlan::new(
        ImageSizeSpec::new(
            neomacs_display_protocol::AxisSize::Exact(100),
            neomacs_display_protocol::AxisSize::Exact(100),
        ),
        ImageRealization::default(),
    );
    assert_eq!(
        minified.rows_per_display_row(1000, 1000).get(),
        10,
        "a source shown at a tenth of its size has ten source rows per display row"
    );
}

/// A paletted PNG of `index(x, y) = x + y` over a two-colour palette, with an
/// optional per-entry alpha channel.
fn png_paletted(width: u32, height: u32, palette: Vec<u8>, trns: Option<Vec<u8>>) -> Vec<u8> {
    let mut bytes = Vec::new();
    {
        let mut encoder = png::Encoder::new(&mut bytes, width, height);
        encoder.set_color(png::ColorType::Indexed);
        encoder.set_depth(png::BitDepth::Eight);
        encoder.set_palette(palette);
        if let Some(trns) = trns {
            encoder.set_trns(trns);
        }
        let mut writer = encoder.write_header().expect("header");
        let pixels: Vec<u8> = (0..height)
            .flat_map(|y| (0..width).map(move |x| ((x + y) % 2) as u8))
            .collect();
        writer.write_image_data(&pixels).expect("image data");
        writer.finish().expect("finish");
    }
    bytes
}

/// A sub-8-bit grayscale PNG, bit-packed the way the format stores it.
fn png_with_depth(
    width: u32,
    height: u32,
    depth: png::BitDepth,
    color: png::ColorType,
    sample: impl Fn(u32) -> u8,
) -> Vec<u8> {
    let per_byte = 8 / depth as usize;
    let row_bytes = (width as usize).div_ceil(per_byte);
    let mut bytes = Vec::new();
    {
        let mut encoder = png::Encoder::new(&mut bytes, width, height);
        encoder.set_color(color);
        encoder.set_depth(depth);
        let mut writer = encoder.write_header().expect("header");
        let mut pixels = Vec::with_capacity(row_bytes * height as usize);
        for _ in 0..height {
            let mut row = vec![0u8; row_bytes];
            for x in 0..width {
                let value = sample(x);
                let shift = 8 - depth as usize * ((x as usize % per_byte) + 1);
                row[x as usize / per_byte] |= value << shift;
            }
            pixels.extend_from_slice(&row);
        }
        writer.write_image_data(&pixels).expect("image data");
        writer.finish().expect("finish");
    }
    bytes
}

fn png_from(image: image::DynamicImage) -> Vec<u8> {
    let mut bytes = Cursor::new(Vec::new());
    image
        .write_to(&mut bytes, image::ImageFormat::Png)
        .expect("PNG is encodable");
    bytes.into_inner()
}

/// CRC-32/ISO-HDLC, for the one hand-edited chunk above.
fn crc32(bytes: &[u8]) -> u32 {
    let mut crc = 0xffff_ffffu32;
    for byte in bytes {
        crc ^= u32::from(*byte);
        for _ in 0..8 {
            let mask = 0u32.wrapping_sub(crc & 1);
            crc = (crc >> 1) ^ (0xedb8_8320 & mask);
        }
    }
    !crc
}
