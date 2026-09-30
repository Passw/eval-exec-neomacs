//! Streaming area-average resampling: source rows in, raster rows out.
//!
//! A banded decode used to build the whole native image and only then scale it
//! onto the texture's raster. Four buffers that size coexisted while it did —
//! the encoded bytes, the decoded image, the conversion to RGBA and the resized
//! output — which for a 12000x700 PNG is about 50 MB of memory to look at one
//! 4096x238 texture. [`RasterTarget`] is the replacement: the decode pushes its
//! rows in as it reads them, each row is resampled as it arrives, and what the
//! target holds is the raster itself plus the few output rows still being
//! averaged. No buffer is ever sized by the native height.
//!
//! **The filter is the area average.** One output sample is the mean of the
//! input samples it covers, each weighted by how much of it the output covers.
//! That is a box filter, and it is what a reduction wants: every input sample
//! contributes exactly once, so nothing is dropped and nothing is counted
//! twice. It is not the same picture as the Lanczos3 this path used to resample
//! with — box is cheaper and does not ring, and it is softer on fine detail —
//! but it is the picture the texture ends up holding, since a band's pixels are
//! cut from the same target the finished upload writes: the preview during a
//! decode and the finished image are the same bytes.
//!
//! **A target only ever reduces.** An area average that had to *invent* samples
//! would be reading a value at a position no sample sits at, and the honest
//! answer there is an interpolating filter rather than a box: a source shown
//! larger than it is declines the target ([`RasterTarget::new`] returns `None`)
//! and takes the whole-image path, which is where the interpolating filter
//! lives and which is what such a source did before banding existed. Every
//! axis this type admits has at least as many inputs as outputs, and an axis
//! with exactly as many is the identity — the case of an image shown at its own
//! size, where the preview is not an approximation of the finished pixels but
//! those pixels.
//!
//! The weights are integers. Output `o` covers the input interval
//! `[o * n / m, (o + 1) * n / m)` for `n` inputs and `m` outputs; measured in
//! `1/m`ths of an input sample that interval is `[o * n, (o + 1) * n)`, and
//! input `i` covers `[i * m, (i + 1) * m)`. Their overlap is the weight, and
//! each output's weights sum to exactly `n`, so the result is `round(sum / n)`
//! with no accumulated error.

use std::collections::VecDeque;
use std::num::NonZeroU32;
use std::sync::Arc;

use neomacs_display_protocol::{ImageNativeExtent, ImageRasterExtent};

use crate::image_bands::{BandPlacement, RasterBand, TextureRows};

/// Largest dimension either axis of a resample is defined for.
///
/// The products below are `input * output` in `u64`, and a PNG's own header
/// caps a dimension at `2^31 - 1`; anything at or beyond that is declined so a
/// dimension no decoder can produce cannot overflow the arithmetic.
const MAX_AXIS: u32 = 1 << 31;

/// One axis of a resample: `output` samples, each an area average of `input`.
///
/// `output <= input` is what makes the average an average of samples that
/// exist; a longer axis has no definition here, and `None` is how that is said.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Axis {
    input: u32,
    output: u32,
}

impl Axis {
    fn new(input: u32, output: u32) -> Option<Self> {
        (input > 0 && output > 0 && output <= input && input < MAX_AXIS)
            .then_some(Self { input, output })
    }

    /// Whether every output is one input, which the area average reproduces
    /// exactly and the target therefore skips.
    fn is_identity(self) -> bool {
        self.input == self.output
    }

    /// The weight one output distributes over the inputs it covers.
    fn total(self) -> u64 {
        u64::from(self.input)
    }

    /// Every output input `i` contributes to, with the weight of that
    /// contribution. Outputs are visited in increasing order.
    ///
    /// The range is the outputs `i`'s own interval could touch, and a zero
    /// overlap is what says an output in that range is not one of them: the
    /// first output an input reaches is not simply `i * m / n` rounded up (an
    /// input straddling a boundary reaches the output before its own start),
    /// and the arithmetic that says so exactly is the overlap itself.
    fn contributions(self, i: u32, mut visit: impl FnMut(u32, u64)) {
        let (n, m) = (u64::from(self.input), u64::from(self.output));
        let i = u64::from(i);
        let first = (i * m) / n;
        let last = ((i + 1) * m).div_ceil(n);
        for o in first..last {
            let low = (o * n).max(i * m);
            let high = ((o + 1) * n).min((i + 1) * m);
            if high > low {
                visit(u32::try_from(o).unwrap_or(u32::MAX), high - low);
            }
        }
    }

    /// The last input that contributes anything to output `o`.
    fn last_input(self, o: u32) -> u32 {
        let (n, m) = (u64::from(self.input), u64::from(self.output));
        u32::try_from(((u64::from(o) + 1) * n).div_ceil(m).saturating_sub(1)).unwrap_or(self.input)
    }
}

/// One output row's running weighted total, in flight.
///
/// The sums are `u64`: a weight is at most the input count and a sample is at
/// most 255, so the largest sum an output can hold is `255 * n`, which fits
/// `u64` for every `n` this type admits.
struct Accum {
    output: u32,
    sums: Vec<u64>,
}

impl Accum {
    fn new(output: u32, sums: Vec<u64>) -> Self {
        Self { output, sums }
    }

    /// Add one input row, `weight` times over.
    fn add(&mut self, row: &[u8], weight: u64) {
        for (sum, sample) in self.sums.iter_mut().zip(row) {
            *sum += weight * u64::from(*sample);
        }
    }
}

/// A raster-sized buffer that source rows are written into as they arrive.
///
/// Built for one native extent and one raster, it is filled from the top: each
/// [`Self::push_row`] takes one source row, and the output rows that row
/// completes are written where they belong. [`Self::band_since`] hands back
/// the rows built since a mark, in order and without gaps, which is exactly
/// what one band of the decode is allowed to fill.
pub(crate) struct RasterTarget {
    native: ImageNativeExtent,
    raster: ImageRasterExtent,
    columns: Axis,
    rows: Axis,
    /// The raster, row-major from row zero.
    pixels: Vec<u8>,
    /// Source rows pushed so far.
    fed: u32,
    /// Output rows written so far.
    built: u32,
    /// Output rows still being averaged, lowest first.
    pending: VecDeque<Accum>,
    /// Accumulator buffers to reuse, so a long raster does not allocate one
    /// per output row.
    spare: Vec<Vec<u64>>,
    /// One source row, resampled to the raster's width.
    line: Vec<u8>,
}

impl RasterTarget {
    /// A target realizing `native` onto `raster`.
    ///
    /// `None` for an empty extent on either side, which is a source or a raster
    /// with no pixels rather than a target that cannot be built, and for a
    /// raster larger than the source on either axis — a target that would have
    /// to invent pixels is not one this filter can be.
    pub(crate) fn new(native: ImageNativeExtent, raster: ImageRasterExtent) -> Option<Self> {
        let columns = Axis::new(native.width(), raster.width())?;
        let rows = Axis::new(native.height(), raster.height())?;
        Some(Self {
            native,
            raster,
            columns,
            rows,
            pixels: vec![0; raster.width() as usize * raster.height() as usize * 4],
            fed: 0,
            built: 0,
            pending: VecDeque::new(),
            spare: Vec::new(),
            line: vec![0; raster.width() as usize * 4],
        })
    }

    /// The raster this target fills.
    pub(crate) fn raster(&self) -> ImageRasterExtent {
        self.raster
    }

    /// How many output rows have been written; the fill level of the raster.
    pub(crate) fn built(&self) -> u32 {
        self.built
    }

    /// Take one source row, in order.
    ///
    /// The row must be the native width in RGBA, which is what the decode
    /// expands each row to; anything else is a decode whose rows are not the
    /// extent its header named, and the raster it would produce would not be
    /// the raster its texture holds.
    pub(crate) fn push_row(&mut self, row: &[u8]) {
        assert_eq!(
            row.len(),
            self.native.width() as usize * 4,
            "a source row is the native width in RGBA"
        );
        assert!(
            self.fed < self.native.height(),
            "no more rows than the height"
        );
        match self.columns {
            axis if axis.is_identity() => self.line.copy_from_slice(row),
            axis => resample_row(axis, row, &mut self.line),
        }
        let index = self.fed;
        self.fed += 1;
        if self.rows.is_identity() {
            let stride = self.raster.width() as usize * 4;
            let at = index as usize * stride;
            self.pixels[at..at + stride].copy_from_slice(&self.line);
            self.built = self.fed;
            return;
        }
        let rows = self.rows;
        {
            let (line, pending, spare) = (&self.line, &mut self.pending, &mut self.spare);
            rows.contributions(index, |output, weight| {
                let slot = match pending.iter().position(|accum| accum.output == output) {
                    Some(slot) => slot,
                    None => {
                        // Outputs are visited in increasing order, so a new one
                        // is always the last.
                        let sums = spare.pop().unwrap_or_else(|| vec![0; line.len()]);
                        pending.push_back(Accum::new(output, sums));
                        pending.len() - 1
                    }
                };
                // Indexed rather than `get_mut`: the slot is one this loop just
                // produced, so an invalid one is a bug in this function, and a
                // contribution that vanished silently would be a wrong picture
                // rather than a loud failure. A panic here is caught by the
                // decoder thread and turns into a failed decode.
                pending[slot].add(line, weight);
            });
        }
        // An output row is complete once every input that contributes to it
        // has been pushed, which is a property of the axis, not of the band.
        while let Some(front) = self.pending.front() {
            if rows.last_input(front.output) > index {
                break;
            }
            let done = self
                .pending
                .pop_front()
                .expect("the front was just checked");
            let total = rows.total();
            let stride = self.raster.width() as usize * 4;
            let at = done.output as usize * stride;
            for (channel, sum) in self.pixels[at..at + stride].iter_mut().zip(&done.sums) {
                *channel = scale_channel(*sum, total);
            }
            let mut sums = done.sums;
            sums.iter_mut().for_each(|sum| *sum = 0);
            self.spare.push(sums);
            self.built = done.output + 1;
        }
    }

    /// The output rows built since `from`, with their pixels.
    ///
    /// `None` when no row was built since then, which a decode paused between
    /// two of them reports; the band that follows starts where this one would
    /// have. `from` is a [`Self::built`] the caller read earlier, so the range
    /// is inside the raster by construction.
    pub(crate) fn band_since(&self, from: u32) -> Option<RasterBand> {
        let len = NonZeroU32::new(self.built.checked_sub(from)?)?;
        let rows = TextureRows::new(from, len);
        let stride = self.raster.width() as usize * 4;
        let pixels: Arc<[u8]> =
            self.pixels[from as usize * stride..self.built as usize * stride].into();
        Some(RasterBand::new(
            BandPlacement::new(self.raster, rows),
            pixels,
        ))
    }

    /// The raster, once every source row has been pushed.
    ///
    /// `None` before then, so a caller cannot publish a partial target as a
    /// finished image.
    pub(crate) fn into_pixels(self) -> Option<Vec<u8>> {
        (self.fed == self.native.height() && self.built == self.raster.height())
            .then_some(self.pixels)
    }
}

/// Resample one row of `axis.input` RGBA texels into `axis.output` of them.
fn resample_row(axis: Axis, source: &[u8], target: &mut [u8]) {
    let (n, m) = (u64::from(axis.input), u64::from(axis.output));
    let total = axis.total();
    for (output, texel) in target.chunks_exact_mut(4).enumerate() {
        let (low, high) = (output as u64 * n, (output as u64 + 1) * n);
        let mut sums = [0_u64; 4];
        for index in low / m..high.div_ceil(m) {
            let (input_low, input_high) = (index * m, (index + 1) * m);
            let overlap = high.min(input_high) - low.max(input_low);
            let sample = &source[index as usize * 4..index as usize * 4 + 4];
            sums[0] += overlap * u64::from(sample[0]);
            sums[1] += overlap * u64::from(sample[1]);
            sums[2] += overlap * u64::from(sample[2]);
            sums[3] += overlap * u64::from(sample[3]);
        }
        for (channel, sum) in texel.iter_mut().zip(sums) {
            *channel = scale_channel(sum, total);
        }
    }
}

/// One weighted average, rounded to the nearest 8-bit sample.
fn scale_channel(sum: u64, total: u64) -> u8 {
    // The weights of one output sum to `total`, so the quotient is at most
    // `255`; the rounding cannot carry it past that either.
    u8::try_from((sum + total / 2) / total).unwrap_or(u8::MAX)
}

#[cfg(test)]
#[path = "image_scale/tests.rs"]
mod tests;
