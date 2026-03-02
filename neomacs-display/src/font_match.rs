//! Shared font matching helpers.
//!
//! Keeps layout and rasterization behavior consistent.

use cosmic_text::FontSystem;
use fontdb::Style as DbStyle;

/// Resolve a requested weight to the closest available weight in the same family.
///
/// For explicit family names, this prevents cross-family jumps when a specific
/// weight instance is missing from the requested family.
pub fn resolve_weight_in_family(
    font_system: &FontSystem,
    family: &str,
    requested_weight: u16,
    italic: bool,
) -> u16 {
    let family_lc = family.to_lowercase();
    if matches!(
        family_lc.as_str(),
        "" | "mono" | "monospace" | "serif" | "sans" | "sansserif" | "sans-serif"
    ) {
        return requested_weight;
    }

    let style = if italic {
        DbStyle::Italic
    } else {
        DbStyle::Normal
    };
    let db = font_system.db();
    let family_weights = family_weights_for_style(db, family, style);
    if family_weights.is_empty() {
        return requested_weight;
    }
    pick_nearest_css_weight(&family_weights, requested_weight)
}

fn family_weights_for_style(db: &fontdb::Database, family: &str, style: DbStyle) -> Vec<u16> {
    let style_pref = match style {
        DbStyle::Italic => [DbStyle::Italic, DbStyle::Oblique, DbStyle::Normal],
        DbStyle::Oblique => [DbStyle::Oblique, DbStyle::Italic, DbStyle::Normal],
        DbStyle::Normal => [DbStyle::Normal, DbStyle::Oblique, DbStyle::Italic],
    };

    for preferred_style in style_pref {
        let mut weights: Vec<u16> = db
            .faces()
            .filter(|face| face.style == preferred_style)
            .filter(|face| {
                face.families
                    .iter()
                    .any(|(name, _)| name.eq_ignore_ascii_case(family))
            })
            .map(|face| face.weight.0)
            .collect();

        if !weights.is_empty() {
            weights.sort_unstable();
            weights.dedup();
            return weights;
        }
    }

    Vec::new()
}

// Generic same-family weight fallback:
// 1) exact match when available
// 2) otherwise prefer nearest lower existing weight
// 3) if none lower exists, use the nearest upper existing weight
fn pick_nearest_css_weight(weights: &[u16], requested_weight: u16) -> u16 {
    if weights.contains(&requested_weight) {
        return requested_weight;
    }
    if let Some(w) = weights
        .iter()
        .copied()
        .filter(|w| *w <= requested_weight)
        .max()
    {
        return w;
    }

    weights
        .iter()
        .copied()
        .filter(|w| *w > requested_weight)
        .min()
        .unwrap_or(requested_weight)
}
