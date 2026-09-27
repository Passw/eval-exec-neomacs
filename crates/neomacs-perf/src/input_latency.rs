//! Reports native confirmed input latency separately from redisplay throughput.
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Deserialize)]
struct Sample {
    input: u64,
    kind: String,
    input_to_present_ns: Option<u64>,
    evicted_inputs: u64,
}

#[derive(Serialize)]
struct Summary {
    samples: usize,
    p50_ms: f64,
    p95_ms: f64,
    p99_ms: f64,
    max_ms: f64,
    over_budget: usize,
}

pub(crate) fn report(text: &str, budget_us: u64) -> Result<serde_json::Value, String> {
    let mut groups: BTreeMap<String, Vec<u64>> = BTreeMap::new();
    let mut identities = BTreeSet::new();
    for (index, line) in text.lines().enumerate() {
        let sample: Sample =
            serde_json::from_str(line).map_err(|error| format!("line {}: {error}", index + 1))?;
        if !identities.insert(sample.input) {
            return Err(format!(
                "duplicate input {}: use one editor session per report",
                sample.input
            ));
        }
        if sample.evicted_inputs != 0 {
            return Err("input tracking overflowed; latency distribution is incomplete".into());
        }
        let latency = sample
            .input_to_present_ns
            .ok_or("input and presentation timestamps are not comparable")?;
        groups.entry(sample.kind).or_default().push(latency);
    }
    if groups.is_empty() {
        return Err("no compositor-confirmed input samples".into());
    }
    let summaries: BTreeMap<_, _> = groups
        .into_iter()
        .map(|(kind, mut values)| {
            values.sort_unstable();
            let quantile = |percent: usize| {
                values[(values.len() * percent).div_ceil(100).saturating_sub(1)] as f64
                    / 1_000_000.0
            };
            let summary = Summary {
                samples: values.len(),
                p50_ms: quantile(50),
                p95_ms: quantile(95),
                p99_ms: quantile(99),
                max_ms: *values.last().unwrap() as f64 / 1_000_000.0,
                over_budget: values
                    .iter()
                    .filter(|&&value| value > budget_us.saturating_mul(1_000))
                    .count(),
            };
            (kind, summary)
        })
        .collect();
    Ok(
        serde_json::json!({ "measurement": "native-enqueue-to-compositor-presentation", "scope": "inputs with a confirmed viewport change", "budget_us": budget_us, "by_input_kind": summaries }),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn report_keeps_input_kinds_separate_and_counts_budget_misses() {
        let text = [
            r#"{"input":1,"kind":"wheel","input_to_present_ns":1000000,"evicted_inputs":0}"#,
            r#"{"input":2,"kind":"wheel","input_to_present_ns":30000000,"evicted_inputs":0}"#,
            r#"{"input":3,"kind":"page","input_to_present_ns":5000000,"evicted_inputs":0}"#,
        ]
        .join("\n");
        let result = report(&text, 16667).unwrap();
        assert_eq!(result["by_input_kind"]["wheel"]["samples"], 2);
        assert_eq!(result["by_input_kind"]["wheel"]["p95_ms"], 30.0);
        assert_eq!(result["by_input_kind"]["wheel"]["over_budget"], 1);
        assert_eq!(result["by_input_kind"]["page"]["over_budget"], 0);
    }
    #[test]
    fn report_rejects_unavailable_or_biased_measurements() {
        for input in [
            "",
            "broken",
            r#"{"input":1,"kind":"page","input_to_present_ns":null,"evicted_inputs":0}"#,
            r#"{"input":1,"kind":"page","input_to_present_ns":100,"evicted_inputs":1}"#,
        ] {
            assert!(report(input, 16667).is_err());
        }
        let sample = r#"{"input":1,"kind":"page","input_to_present_ns":100,"evicted_inputs":0}"#;
        assert!(report(&format!("{sample}\n{sample}"), 16667).is_err());
    }
}
