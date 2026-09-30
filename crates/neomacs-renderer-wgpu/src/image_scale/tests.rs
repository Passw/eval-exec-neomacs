//! What the streaming scaler promises.
//!
//! Three contracts. The pixels are the area average of the source — checked
//! against the definition written out the obvious way, which shares no code
//! with the streaming target. The rows come out in order, once each, and the
//! bands cut from the target tile it. And a target that has not been fed every
//! row is not a finished image.

use super::*;
use neomacs_display_protocol::{ImageNativeExtent, ImageRasterExtent};

/// A source whose every channel varies with both coordinates, so a sample
/// taken from the wrong place cannot pass as the right one.
fn source(width: u32, height: u32) -> Vec<u8> {
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

fn target(native: (u32, u32), raster: (u32, u32)) -> Option<RasterTarget> {
    RasterTarget::new(
        ImageNativeExtent::new(native.0, native.1),
        ImageRasterExtent::new(raster.0, raster.1),
    )
}

/// Every row pushed in order, which is how a decode feeds it.
fn filled(native: (u32, u32), raster: (u32, u32)) -> RasterTarget {
    let pixels = source(native.0, native.1);
    let mut target = target(native, raster).expect("a target for a reducible extent");
    for row in pixels.chunks_exact(native.0 as usize * 4) {
        target.push_row(row);
    }
    target
}

/// The area average, written the obvious way: every output sample walks the
/// inputs it could cover and weights each by the overlap, one axis at a time,
/// holding the resampled rows in between.
///
/// Nothing here streams — no cursor, no accumulator, no buffer reused between
/// rows — so it is the definition rather than a second copy of the
/// implementation, and a target that averages the wrong row, drops a weight or
/// counts one twice disagrees with it.
fn area_average(source: &[u8], (in_w, in_h): (u32, u32), (out_w, out_h): (u32, u32)) -> Vec<u8> {
    fn weights(lo: u64, hi: u64, input: u64) -> impl Iterator<Item = (usize, u64)> {
        let first = lo / input;
        let last = hi.div_ceil(input);
        (first..last).filter_map(move |i| {
            let overlap = hi.min((i + 1) * input) - lo.max(i * input);
            (overlap > 0).then_some((i as usize, overlap))
        })
    }
    let round = |sum: u64, total: u64| ((sum + total / 2) / total) as u8;

    let mut lines = Vec::with_capacity(in_h as usize * out_w as usize * 4);
    for row in source.chunks_exact(in_w as usize * 4) {
        let mut line = vec![0_u8; out_w as usize * 4];
        for (o, texel) in line.chunks_exact_mut(4).enumerate() {
            let (lo, hi) = (o as u64 * in_w as u64, (o as u64 + 1) * in_w as u64);
            let mut sums = [0_u64; 4];
            for (i, weight) in weights(lo, hi, u64::from(out_w)) {
                for c in 0..4 {
                    sums[c] += weight * u64::from(row[i * 4 + c]);
                }
            }
            for (c, channel) in texel.iter_mut().enumerate() {
                *channel = round(sums[c], u64::from(in_w));
            }
        }
        lines.extend_from_slice(&line);
    }

    let mut out = vec![0_u8; out_w as usize * out_h as usize * 4];
    for column in 0..out_w as usize {
        for o in 0..out_h {
            let (lo, hi) = (u64::from(o) * in_h as u64, (u64::from(o) + 1) * in_h as u64);
            let mut sums = [0_u64; 4];
            for (i, weight) in weights(lo, hi, u64::from(out_h)) {
                for c in 0..4 {
                    sums[c] += weight * u64::from(lines[i * out_w as usize * 4 + column * 4 + c]);
                }
            }
            let at = (o as usize * out_w as usize + column) * 4;
            for (c, channel) in out[at..at + 4].iter_mut().enumerate() {
                *channel = round(sums[c], u64::from(in_h));
            }
        }
    }
    out
}

/// The whole point: what comes out is the area average of what went in.
#[test]
fn a_target_holds_the_area_average_of_the_rows_it_was_given() {
    // Ratios that divide evenly, ratios that do not, a reduction on one axis
    // only, and a reduction that is nearly the identity.
    let cases = [
        ((37, 23), (11, 7)),
        ((64, 64), (16, 16)),
        ((100, 3), (33, 3)),
        ((3, 100), (3, 33)),
        ((50, 40), (49, 39)),
        ((17, 11), (1, 1)),
    ];
    for (native, raster) in cases {
        let target = filled(native, raster);
        assert_eq!(target.built(), raster.1, "{native:?} -> {raster:?}");
        let got = target
            .into_pixels()
            .expect("every row was pushed, so the target is whole");
        assert_eq!(
            got,
            area_average(&source(native.0, native.1), native, raster),
            "{native:?} -> {raster:?}: the target must be the area average"
        );
    }
}

/// A floor of one output row per band means a source can be reduced far enough
/// that one output covers all of it: the one output is then the mean of the
/// whole source, which a reduction that dropped the odd row would miss.
///
/// To within a level, not exactly: the two axes round their intermediate
/// results, so the mean of the row means is not bit-for-bit the mean of the
/// pixels. That difference is what `a_target_holds_the_area_average...` pins
/// from the other side, against the separable definition itself.
#[test]
fn a_whole_source_can_reduce_to_a_single_pixel() {
    let native = (8, 8);
    let target = filled(native, (1, 1));
    let got = target.into_pixels().expect("a whole target");
    let pixels = source(native.0, native.1);
    let mean = |channel: usize| {
        let sum: u32 = pixels
            .iter()
            .skip(channel)
            .step_by(4)
            .map(|sample| u32::from(*sample))
            .sum();
        ((sum + 32) / 64) as u8
    };
    for (channel, sample) in got.iter().enumerate() {
        assert!(
            sample.abs_diff(mean(channel)) <= 1,
            "one output is the mean of the source: got {got:?}, \
             expected {channel} to be {}",
            mean(channel)
        );
    }
}

/// A constant source reduces to that constant, whatever the ratio: the weights
/// one output distributes sum to the input count, so a filter that lost or
/// double-counted a weight would drift instead.
#[test]
fn a_constant_source_reduces_to_the_same_constant() {
    let native = (97, 61);
    for raster in [(97, 61), (40, 40), (13, 7), (1, 1)] {
        let pixels: Vec<u8> = (0..native.0 * native.1)
            .flat_map(|_| [7u8, 200, 33, 255])
            .collect();
        let mut target = target(native, raster).expect("a reducible extent");
        for row in pixels.chunks_exact(native.0 as usize * 4) {
            target.push_row(row);
        }
        let got = target.into_pixels().expect("a whole target");
        assert!(
            got.chunks_exact(4).all(|texel| texel == [7, 200, 33, 255]),
            "{native:?} -> {raster:?}: a constant must reduce to itself"
        );
    }
}

/// The case of an image shown at its own size: the raster is the source, so the
/// preview during a decode is not an approximation of the finished pixels but
/// the finished pixels. A reduction on one axis only is the case above; this is
/// the one where neither axis moves.
#[test]
fn a_target_the_size_of_its_source_reproduces_it_exactly() {
    let native = (40, 30);
    let target = filled(native, native);
    assert_eq!(target.built(), native.1);
    assert_eq!(
        target.into_pixels().expect("a whole target"),
        source(native.0, native.1),
        "a target the size of its source is its source"
    );
}

/// An axis with more outputs than inputs would have to invent samples, so it is
/// declined rather than approximated: the source that asked for one takes the
/// whole-image path, where the interpolating filter lives.
#[test]
fn a_target_larger_than_its_source_is_declined() {
    assert!(target((100, 100), (101, 100)).is_none(), "a wider raster");
    assert!(target((100, 100), (100, 101)).is_none(), "a taller raster");
    assert!(target((100, 100), (200, 50)).is_none(), "wider on one axis");
    assert!(target((100, 100), (100, 100)).is_some(), "the same size");
    assert!(target((100, 100), (50, 50)).is_some(), "smaller");
}

/// The rows a band hands over are the rows built since the caller's mark:
/// in order, contiguous, and never the same row twice.
#[test]
fn each_band_takes_the_rows_built_since_the_last_one() {
    let native = (60, 40);
    let raster = (20, 13);
    let pixels = source(native.0, native.1);
    let mut target = target(native, raster).expect("a reducible extent");
    let mut written = 0;
    let mut bands = 0;
    // Feed in bands of three source rows, the way a decode does.
    for rows in pixels.chunks(native.0 as usize * 4 * 3) {
        for row in rows.chunks_exact(native.0 as usize * 4) {
            target.push_row(row);
        }
        if let Some(band) = target.band_since(written) {
            let placement = band.placement();
            assert_eq!(placement.raster(), target.raster());
            assert_eq!(
                placement.rows().start(),
                written,
                "a band starts where the last one ended"
            );
            assert_eq!(
                band.pixels().len(),
                raster.0 as usize * placement.rows().len().get() as usize * 4,
                "a band carries exactly the rows it fills"
            );
            written = placement.rows().end();
            bands += 1;
        }
    }
    assert_eq!(written, raster.1, "the bands cover the whole raster");
    assert!(bands > 1, "a 40-row source bands into more than one piece");
    assert_eq!(target.built(), raster.1);
    assert!(
        target.band_since(written).is_none(),
        "a band with no rows is not a band"
    );
}

/// A target nobody has finished feeding is not an image: the only way to get
/// the pixels out is to have pushed every row.
#[test]
fn a_partially_fed_target_is_not_a_finished_image() {
    let native = (16, 8);
    let pixels = source(native.0, native.1);
    let mut half = target(native, (8, 4)).expect("a reducible extent");
    for row in pixels.chunks_exact(native.0 as usize * 4).take(4) {
        half.push_row(row);
    }
    assert!(
        half.into_pixels().is_none(),
        "half a source is not an image"
    );

    let mut whole = target(native, (8, 4)).expect("a reducible extent");
    for row in pixels.chunks_exact(native.0 as usize * 4) {
        whole.push_row(row);
    }
    assert!(whole.into_pixels().is_some(), "a whole source is");
}

/// The separable average is within one 8-bit level of the two-dimensional area
/// average of the same samples. They are not the same computation — each axis
/// rounds its intermediate result — but they are the same filter, and a
/// resampler that was off by a scale factor or an off-by-one window would be
/// further out than that.
#[test]
fn the_separable_result_is_the_two_dimensional_area_average() {
    let native = (37, 23);
    let raster = (11, 7);
    let pixels = source(native.0, native.1);
    let mut target = target(native, raster).expect("a reducible extent");
    for row in pixels.chunks_exact(native.0 as usize * 4) {
        target.push_row(row);
    }
    let got = target.into_pixels().expect("a whole target");

    // The two-dimensional average: each output covers a rectangle, and an input
    // pixel's weight is the product of the two axis overlaps.
    let mut expected = vec![0_u8; raster.0 as usize * raster.1 as usize * 4];
    for oy in 0..raster.1 {
        for ox in 0..raster.0 {
            let (x_lo, x_hi) = (
                u64::from(ox) * u64::from(native.0),
                u64::from(ox + 1) * u64::from(native.0),
            );
            let (y_lo, y_hi) = (
                u64::from(oy) * u64::from(native.1),
                u64::from(oy + 1) * u64::from(native.1),
            );
            let mut sums = [0_u64; 4];
            for iy in y_lo / u64::from(raster.1)..y_hi.div_ceil(u64::from(raster.1)) {
                for ix in x_lo / u64::from(raster.0)..x_hi.div_ceil(u64::from(raster.0)) {
                    let xw = x_hi.min((ix + 1) * u64::from(raster.0))
                        - x_lo.max(ix * u64::from(raster.0));
                    let yw = y_hi.min((iy + 1) * u64::from(raster.1))
                        - y_lo.max(iy * u64::from(raster.1));
                    let at = (iy as usize * native.0 as usize + ix as usize) * 4;
                    for c in 0..4 {
                        sums[c] += xw * yw * u64::from(pixels[at + c]);
                    }
                }
            }
            let total = u64::from(native.0) * u64::from(native.1);
            let at = (oy as usize * raster.0 as usize + ox as usize) * 4;
            for c in 0..4 {
                expected[at + c] = ((sums[c] + total / 2) / total) as u8;
            }
        }
    }

    for (texel, want) in got.chunks_exact(4).zip(expected.chunks_exact(4)) {
        for (channel, expected) in texel.iter().zip(want) {
            assert!(
                channel.abs_diff(*expected) <= 1,
                "the separable average is the two-dimensional one to within a \
                 level of rounding: got {texel:?}, expected {want:?}"
            );
        }
    }
}
