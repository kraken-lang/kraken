//! Extended tests for the compiler benchmark harness.

#[cfg(test)]
mod tests {
    use crate::bench_harness::*;
    use std::collections::BTreeMap;

    #[test]
    fn test_phase_metrics_conversions() {
        let m = PhaseMetrics {
            phase: "lexer".into(),
            duration_ns: 1_500_000,
            memory_bytes: 2048,
            units_processed: 100,
        };
        assert!((m.duration_ms() - 1.5).abs() < 0.001);
        assert!((m.duration_us() - 1500.0).abs() < 0.1);
        assert!((m.memory_kb() - 2.0).abs() < 0.001);
    }

    #[test]
    fn test_throughput_positive() {
        let m = PhaseMetrics {
            phase: "p".into(),
            duration_ns: 1_000_000_000,
            memory_bytes: 0,
            units_processed: 500,
        };
        assert!((m.throughput() - 500.0).abs() < 0.1);
    }

    #[test]
    fn test_throughput_zero_duration() {
        let m = PhaseMetrics {
            phase: "p".into(),
            duration_ns: 0,
            memory_bytes: 0,
            units_processed: 10,
        };
        assert_eq!(m.throughput(), 0.0);
    }

    #[test]
    fn test_phase_timer_accuracy() {
        let t = PhaseTimer::start("x");
        std::thread::sleep(std::time::Duration::from_millis(5));
        let m = t.stop(1024, 42);
        assert_eq!(m.phase, "x");
        assert!(m.duration_ns >= 4_000_000);
        assert_eq!(m.memory_bytes, 1024);
        assert_eq!(m.units_processed, 42);
    }

    #[test]
    fn test_default_corpus_coverage() {
        let c = default_corpus();
        assert!(c.len() >= 5);
        let cats: Vec<&str> = c.iter().map(|e| e.category.as_str()).collect();
        assert!(cats.contains(&"trivial"));
        assert!(cats.contains(&"small"));
        assert!(cats.contains(&"medium"));
        assert!(cats.contains(&"large"));
        assert!(cats.contains(&"stress"));
    }

    #[test]
    fn test_default_corpus_all_valid_source() {
        let c = default_corpus();
        for e in &c {
            assert!(!e.source.is_empty(), "empty source for {}", e.name);
            assert!(e.line_count > 0, "zero line_count for {}", e.name);
            assert!(!e.name.is_empty());
        }
    }

    #[test]
    fn test_bench_entry_trivial() {
        let e = CorpusEntry {
            name: "t".into(),
            source: "fn main() { let x = 1; }\n".into(),
            category: "trivial".into(),
            line_count: 1,
        };
        let r = bench_entry(&e, 3);
        assert_eq!(r.name, "t");
        assert_eq!(r.iterations, 3);
        assert!(!r.phases.is_empty());
        assert!(r.total.duration_ns > 0);
    }

    #[test]
    fn test_bench_entry_phases_present() {
        let e = CorpusEntry {
            name: "t".into(),
            source: "fn main() { let x = 1; }\n".into(),
            category: "trivial".into(),
            line_count: 1,
        };
        let r = bench_entry(&e, 2);
        let names: Vec<&str> = r.phases.iter().map(|p| p.phase.as_str()).collect();
        assert!(names.contains(&"lexer"));
        assert!(names.contains(&"parser"));
    }

    #[test]
    fn test_bench_entry_min_one_iteration() {
        let e = CorpusEntry {
            name: "t".into(),
            source: "fn main() {}\n".into(),
            category: "trivial".into(),
            line_count: 1,
        };
        let r = bench_entry(&e, 0);
        assert_eq!(r.iterations, 1);
    }

    #[test]
    fn test_harness_default_corpus() {
        let h = BenchHarness::new();
        assert!(h.corpus().len() >= 5);
    }

    #[test]
    fn test_harness_with_iterations() {
        let h = BenchHarness::new().with_iterations(10);
        let r = bench_entry(&h.corpus()[0], 10);
        assert_eq!(r.iterations, 10);
    }

    #[test]
    fn test_harness_add_entry() {
        let mut h = BenchHarness::new();
        let n = h.corpus().len();
        h.add_entry(CorpusEntry {
            name: "custom".into(),
            source: "fn main() {}\n".into(),
            category: "custom".into(),
            line_count: 1,
        });
        assert_eq!(h.corpus().len(), n + 1);
    }

    #[test]
    fn test_compare_stable() {
        let a = PhaseMetrics {
            phase: "x".into(),
            duration_ns: 100,
            memory_bytes: 0,
            units_processed: 0,
        };
        let b = PhaseMetrics {
            phase: "x".into(),
            duration_ns: 100,
            memory_bytes: 0,
            units_processed: 0,
        };
        let c = compare_phase(&a, &b, 10.0);
        assert!(matches!(c.status, RegressionStatus::Stable(_)));
    }

    #[test]
    fn test_compare_regressed() {
        let a = PhaseMetrics {
            phase: "x".into(),
            duration_ns: 200,
            memory_bytes: 0,
            units_processed: 0,
        };
        let b = PhaseMetrics {
            phase: "x".into(),
            duration_ns: 100,
            memory_bytes: 0,
            units_processed: 0,
        };
        let c = compare_phase(&a, &b, 10.0);
        assert!(matches!(c.status, RegressionStatus::Regressed(_)));
        assert!(c.change_pct > 90.0);
    }

    #[test]
    fn test_compare_improved() {
        let a = PhaseMetrics {
            phase: "x".into(),
            duration_ns: 50,
            memory_bytes: 0,
            units_processed: 0,
        };
        let b = PhaseMetrics {
            phase: "x".into(),
            duration_ns: 100,
            memory_bytes: 0,
            units_processed: 0,
        };
        let c = compare_phase(&a, &b, 10.0);
        assert!(matches!(c.status, RegressionStatus::Improved(_)));
    }

    #[test]
    fn test_compare_zero_baseline() {
        let a = PhaseMetrics {
            phase: "x".into(),
            duration_ns: 100,
            memory_bytes: 0,
            units_processed: 0,
        };
        let b = PhaseMetrics {
            phase: "x".into(),
            duration_ns: 0,
            memory_bytes: 0,
            units_processed: 0,
        };
        let c = compare_phase(&a, &b, 10.0);
        assert!(matches!(c.status, RegressionStatus::Stable(_)));
    }

    #[test]
    fn test_regression_detection_triggered() {
        let h = BenchHarness::new().with_threshold(5.0);
        let fast = BenchResult {
            name: "t".into(),
            category: "x".into(),
            iterations: 1,
            phases: vec![],
            total: PhaseMetrics {
                phase: "full_pipeline".into(),
                duration_ns: 100,
                memory_bytes: 0,
                units_processed: 0,
            },
        };
        let slow = BenchResult {
            name: "t".into(),
            category: "x".into(),
            iterations: 1,
            phases: vec![],
            total: PhaseMetrics {
                phase: "full_pipeline".into(),
                duration_ns: 200,
                memory_bytes: 0,
                units_processed: 0,
            },
        };
        let mut map = BTreeMap::new();
        map.insert("t".into(), fast);
        let bl = Baseline {
            captured_at: "0".into(),
            commit: None,
            results: map,
        };
        let reports = h.detect_regressions(&[slow], &bl);
        assert_eq!(reports.len(), 1);
        assert!(reports[0].has_regression);
    }

    #[test]
    fn test_regression_detection_stable() {
        let h = BenchHarness::new().with_threshold(10.0);
        let a = BenchResult {
            name: "t".into(),
            category: "x".into(),
            iterations: 1,
            phases: vec![],
            total: PhaseMetrics {
                phase: "full_pipeline".into(),
                duration_ns: 100,
                memory_bytes: 0,
                units_processed: 0,
            },
        };
        let b = a.clone();
        let mut map = BTreeMap::new();
        map.insert("t".into(), b);
        let bl = Baseline {
            captured_at: "0".into(),
            commit: None,
            results: map,
        };
        let reports = h.detect_regressions(&[a], &bl);
        assert_eq!(reports.len(), 1);
        assert!(!reports[0].has_regression);
    }

    #[test]
    fn test_regression_no_baseline_match() {
        let h = BenchHarness::new();
        let r = BenchResult {
            name: "unknown".into(),
            category: "x".into(),
            iterations: 1,
            phases: vec![],
            total: PhaseMetrics {
                phase: "full_pipeline".into(),
                duration_ns: 100,
                memory_bytes: 0,
                units_processed: 0,
            },
        };
        let bl = Baseline {
            captured_at: "0".into(),
            commit: None,
            results: BTreeMap::new(),
        };
        let reports = h.detect_regressions(&[r], &bl);
        assert!(reports.is_empty());
    }

    #[test]
    fn test_baseline_roundtrip() {
        let dir = std::env::temp_dir().join("kraken_bench_rt");
        let _ = std::fs::create_dir_all(&dir);
        let path = dir.join("test_bl.json");
        let mut map = BTreeMap::new();
        map.insert(
            "x".into(),
            BenchResult {
                name: "x".into(),
                category: "t".into(),
                iterations: 1,
                phases: vec![],
                total: PhaseMetrics {
                    phase: "full_pipeline".into(),
                    duration_ns: 42,
                    memory_bytes: 0,
                    units_processed: 0,
                },
            },
        );
        let bl = Baseline {
            captured_at: "123".into(),
            commit: Some("abc".into()),
            results: map,
        };
        bl.save(&path).unwrap();
        let loaded = Baseline::load(&path).unwrap();
        assert_eq!(loaded.captured_at, "123");
        assert_eq!(loaded.commit, Some("abc".into()));
        assert_eq!(loaded.results.len(), 1);
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn test_baseline_load_nonexistent() {
        assert!(Baseline::load(std::path::Path::new("/tmp/nonexistent_kraken_bl.json")).is_none());
    }

    #[test]
    fn test_format_report_contains_phases() {
        let h = BenchHarness::new();
        let r = BenchResult {
            name: "t".into(),
            category: "trivial".into(),
            iterations: 1,
            phases: vec![PhaseMetrics {
                phase: "lexer".into(),
                duration_ns: 1000,
                memory_bytes: 64,
                units_processed: 5,
            }],
            total: PhaseMetrics {
                phase: "full_pipeline".into(),
                duration_ns: 2000,
                memory_bytes: 64,
                units_processed: 1,
            },
        };
        let rpt = h.format_report(&[r], &[]);
        assert!(rpt.contains("Compiler Benchmark Report"));
        assert!(rpt.contains("lexer"));
        assert!(rpt.contains("full_pipeline"));
    }

    #[test]
    fn test_format_report_with_regression() {
        let h = BenchHarness::new();
        let r = BenchResult {
            name: "t".into(),
            category: "x".into(),
            iterations: 1,
            phases: vec![],
            total: PhaseMetrics {
                phase: "full_pipeline".into(),
                duration_ns: 200,
                memory_bytes: 0,
                units_processed: 0,
            },
        };
        let rep = RegressionReport {
            name: "t".into(),
            comparisons: vec![PhaseComparison {
                phase: "full_pipeline".into(),
                current_ns: 200,
                baseline_ns: 100,
                change_pct: 100.0,
                status: RegressionStatus::Regressed(100.0),
            }],
            has_regression: true,
        };
        let rpt = h.format_report(&[r], &[rep]);
        assert!(rpt.contains("REGRESSED"));
    }

    #[test]
    fn test_bench_result_total_ms() {
        let r = BenchResult {
            name: "t".into(),
            category: "x".into(),
            iterations: 1,
            phases: vec![],
            total: PhaseMetrics {
                phase: "full_pipeline".into(),
                duration_ns: 5_000_000,
                memory_bytes: 0,
                units_processed: 0,
            },
        };
        assert!((r.total_ms() - 5.0).abs() < 0.001);
    }

    #[test]
    fn test_harness_with_threshold() {
        let h = BenchHarness::new().with_threshold(25.0);
        let a = PhaseMetrics {
            phase: "x".into(),
            duration_ns: 120,
            memory_bytes: 0,
            units_processed: 0,
        };
        let b = PhaseMetrics {
            phase: "x".into(),
            duration_ns: 100,
            memory_bytes: 0,
            units_processed: 0,
        };
        let mut map = BTreeMap::new();
        map.insert(
            "t".into(),
            BenchResult {
                name: "t".into(),
                category: "x".into(),
                iterations: 1,
                phases: vec![],
                total: b,
            },
        );
        let bl = Baseline {
            captured_at: "0".into(),
            commit: None,
            results: map,
        };
        let r = BenchResult {
            name: "t".into(),
            category: "x".into(),
            iterations: 1,
            phases: vec![],
            total: a,
        };
        let reports = h.detect_regressions(&[r], &bl);
        assert!(!reports[0].has_regression); // 20% < 25% threshold
    }

    #[test]
    fn test_harness_with_baseline_path() {
        let h = BenchHarness::new().with_baseline_path("/tmp/custom_bl.json".into());
        // Just verify it doesn't panic
        assert!(!h.corpus().is_empty());
    }
}
