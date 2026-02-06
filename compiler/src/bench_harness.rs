//! Compiler Benchmark Harness — tracks compilation speed, memory, and regressions.

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::time::Instant;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PhaseMetrics {
    pub phase: String,
    pub duration_ns: u64,
    pub memory_bytes: usize,
    pub units_processed: usize,
}

impl PhaseMetrics {
    pub fn duration_ms(&self) -> f64 {
        self.duration_ns as f64 / 1e6
    }
    pub fn duration_us(&self) -> f64 {
        self.duration_ns as f64 / 1e3
    }
    pub fn memory_kb(&self) -> f64 {
        self.memory_bytes as f64 / 1024.0
    }
    pub fn throughput(&self) -> f64 {
        let s = self.duration_ns as f64 / 1e9;
        if s > 0.0 {
            self.units_processed as f64 / s
        } else {
            0.0
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CorpusEntry {
    pub name: String,
    pub source: String,
    pub category: String,
    pub line_count: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BenchResult {
    pub name: String,
    pub category: String,
    pub iterations: usize,
    pub phases: Vec<PhaseMetrics>,
    pub total: PhaseMetrics,
}

impl BenchResult {
    pub fn total_ms(&self) -> f64 {
        self.total.duration_ms()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Baseline {
    pub captured_at: String,
    pub commit: Option<String>,
    pub results: BTreeMap<String, BenchResult>,
}

impl Baseline {
    pub fn load(p: &Path) -> Option<Self> {
        serde_json::from_str(&std::fs::read_to_string(p).ok()?).ok()
    }
    pub fn save(&self, p: &Path) -> std::io::Result<()> {
        let j = serde_json::to_string_pretty(self).map_err(std::io::Error::other)?;
        if let Some(d) = p.parent() {
            std::fs::create_dir_all(d)?;
        }
        std::fs::write(p, j)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum RegressionStatus {
    Improved(f64),
    Stable(f64),
    Regressed(f64),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PhaseComparison {
    pub phase: String,
    pub current_ns: u64,
    pub baseline_ns: u64,
    pub change_pct: f64,
    pub status: RegressionStatus,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegressionReport {
    pub name: String,
    pub comparisons: Vec<PhaseComparison>,
    pub has_regression: bool,
}

pub struct PhaseTimer {
    phase: String,
    start: Instant,
}
impl PhaseTimer {
    pub fn start(p: &str) -> Self {
        Self {
            phase: p.into(),
            start: Instant::now(),
        }
    }
    pub fn stop(self, mem: usize, units: usize) -> PhaseMetrics {
        PhaseMetrics {
            phase: self.phase,
            duration_ns: self.start.elapsed().as_nanos() as u64,
            memory_bytes: mem,
            units_processed: units,
        }
    }
}

pub fn compare_phase(c: &PhaseMetrics, b: &PhaseMetrics, th: f64) -> PhaseComparison {
    let pct = if b.duration_ns > 0 {
        ((c.duration_ns as f64 - b.duration_ns as f64) / b.duration_ns as f64) * 100.0
    } else {
        0.0
    };
    let st = if pct < -th {
        RegressionStatus::Improved(pct)
    } else if pct > th {
        RegressionStatus::Regressed(pct)
    } else {
        RegressionStatus::Stable(pct)
    };
    PhaseComparison {
        phase: c.phase.clone(),
        current_ns: c.duration_ns,
        baseline_ns: b.duration_ns,
        change_pct: pct,
        status: st,
    }
}

pub fn bench_entry(entry: &CorpusEntry, iterations: usize) -> BenchResult {
    let n = iterations.max(1);
    let mut all_p: Vec<Vec<PhaseMetrics>> = Vec::new();
    let mut all_t: Vec<PhaseMetrics> = Vec::new();
    for _ in 0..n {
        let mut ph = Vec::new();
        let t0 = Instant::now();
        let t = PhaseTimer::start("lexer");
        let mut tk = crate::lexer::tokenizer::Tokenizer::new(
            entry.source.clone(),
            PathBuf::from("bench.kr"),
        );
        let toks = tk.tokenize().unwrap_or_default();
        ph.push(t.stop(toks.len() * 64, toks.len()));
        let t = PhaseTimer::start("parser");
        let mut pr = crate::parser::parser::Parser::new(toks.clone(), PathBuf::from("bench.kr"));
        let ast = pr.parse();
        let nc = ast.as_ref().map(|p| p.statements.len()).unwrap_or(0);
        ph.push(t.stop(nc * 256, nc));
        if let Ok(ref prog) = ast {
            let t = PhaseTimer::start("type_checker");
            let mut tc = crate::analyzer::type_checker::TypeChecker::new(PathBuf::from("bench.kr"));
            let _ = tc.check_program(prog);
            ph.push(t.stop(0, prog.statements.len()));
            let t = PhaseTimer::start("ir_lower");
            let mut lowering = crate::ir::lower::IrLowering::new();
            let ir = lowering.lower_program(prog);
            let fc = ir.as_ref().map(|p| p.functions.len()).unwrap_or(0);
            ph.push(t.stop(fc * 512, fc));
        }
        let mem: usize = ph.iter().map(|p| p.memory_bytes).sum();
        all_t.push(PhaseMetrics {
            phase: "full_pipeline".into(),
            duration_ns: t0.elapsed().as_nanos() as u64,
            memory_bytes: mem,
            units_processed: entry.line_count,
        });
        all_p.push(ph);
    }
    all_t.sort_by_key(|m| m.duration_ns);
    let mid = all_t.len() / 2;
    BenchResult {
        name: entry.name.clone(),
        category: entry.category.clone(),
        iterations: n,
        phases: all_p[mid].clone(),
        total: all_t[mid].clone(),
    }
}

pub struct BenchHarness {
    corpus: Vec<CorpusEntry>,
    iterations: usize,
    threshold_pct: f64,
    baseline_path: PathBuf,
}
impl Default for BenchHarness {
    fn default() -> Self {
        Self::new()
    }
}

impl BenchHarness {
    pub fn new() -> Self {
        Self {
            corpus: default_corpus(),
            iterations: 50,
            threshold_pct: 10.0,
            baseline_path: ".compiler_bench_baseline.json".into(),
        }
    }
    pub fn with_iterations(mut self, n: usize) -> Self {
        self.iterations = n.max(1);
        self
    }
    pub fn with_threshold(mut self, p: f64) -> Self {
        self.threshold_pct = p.abs();
        self
    }
    pub fn with_baseline_path(mut self, p: PathBuf) -> Self {
        self.baseline_path = p;
        self
    }
    pub fn add_entry(&mut self, e: CorpusEntry) {
        self.corpus.push(e);
    }
    pub fn corpus(&self) -> &[CorpusEntry] {
        &self.corpus
    }
    pub fn run(&self) -> Vec<BenchResult> {
        self.corpus
            .iter()
            .map(|e| bench_entry(e, self.iterations))
            .collect()
    }

    pub fn save_baseline(&self, results: &[BenchResult]) -> std::io::Result<()> {
        let ts = format!(
            "{}",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs()
        );
        let commit = std::process::Command::new("git")
            .args(["rev-parse", "--short", "HEAD"])
            .output()
            .ok()
            .and_then(|o| {
                if o.status.success() {
                    Some(String::from_utf8_lossy(&o.stdout).trim().to_string())
                } else {
                    None
                }
            });
        let mut map = BTreeMap::new();
        for r in results {
            map.insert(r.name.clone(), r.clone());
        }
        Baseline {
            captured_at: ts,
            commit,
            results: map,
        }
        .save(&self.baseline_path)
    }

    pub fn detect_regressions(
        &self,
        results: &[BenchResult],
        bl: &Baseline,
    ) -> Vec<RegressionReport> {
        results
            .iter()
            .filter_map(|r| {
                bl.results.get(&r.name).map(|blr| {
                    let mut comps = vec![compare_phase(&r.total, &blr.total, self.threshold_pct)];
                    for ph in &r.phases {
                        if let Some(bp) = blr.phases.iter().find(|p| p.phase == ph.phase) {
                            comps.push(compare_phase(ph, bp, self.threshold_pct));
                        }
                    }
                    let has = comps
                        .iter()
                        .any(|c| matches!(c.status, RegressionStatus::Regressed(_)));
                    RegressionReport {
                        name: r.name.clone(),
                        comparisons: comps,
                        has_regression: has,
                    }
                })
            })
            .collect()
    }

    pub fn format_report(&self, results: &[BenchResult], reports: &[RegressionReport]) -> String {
        let mut o = format!("Compiler Benchmark Report\n{}\n", "=".repeat(72));
        for r in results {
            o.push_str(&format!(
                "\n[{}] ({}) {} iters\n",
                r.name, r.category, r.iterations
            ));
            for ph in &r.phases {
                o.push_str(&format!(
                    "  {:16} {:>10.2} us {:>8.1} KB {:>6} units\n",
                    ph.phase,
                    ph.duration_us(),
                    ph.memory_kb(),
                    ph.units_processed
                ));
            }
            o.push_str(&format!(
                "  {:16} {:>10.2} us {:>8.1} KB\n",
                "full_pipeline",
                r.total.duration_us(),
                r.total.memory_kb()
            ));
            if let Some(rep) = reports.iter().find(|x| x.name == r.name) {
                for c in &rep.comparisons {
                    let tag = match &c.status {
                        RegressionStatus::Improved(_) => "IMPROVED",
                        RegressionStatus::Stable(_) => "stable",
                        RegressionStatus::Regressed(_) => "REGRESSED",
                    };
                    o.push_str(&format!(
                        "    {} {:+.1}% ({})\n",
                        c.phase, c.change_pct, tag
                    ));
                }
            }
        }
        o
    }
}

fn gen_many_fns(n: usize) -> String {
    let mut s = String::new();
    for i in 0..n {
        s.push_str(&format!("fn f{i}(x: int) -> int {{ return x + {i}; }}\n"));
    }
    s.push_str("fn main() {\n");
    for i in 0..n {
        s.push_str(&format!("    let v{i} = f{i}({i});\n"));
    }
    s.push_str("}\n");
    s
}

fn gen_deep(d: usize) -> String {
    let mut s = String::from("fn main() {\n    let x = 0;\n");
    for i in 0..d {
        let ind = "    ".repeat(i + 1);
        s.push_str(&format!(
            "{ind}if (x == {i}) {{\n{ind}    let y{i} = x + {i};\n"
        ));
    }
    for i in (0..d).rev() {
        s.push_str(&format!("{}}}\n", "    ".repeat(i + 1)));
    }
    s.push_str("}\n");
    s
}

pub fn default_corpus() -> Vec<CorpusEntry> {
    vec![
        CorpusEntry { name: "trivial_hello".into(), source: "fn main() {\n    let x = 42;\n}\n".into(), category: "trivial".into(), line_count: 3 },
        CorpusEntry { name: "small_arith".into(), source: "fn add(a: int, b: int) -> int { return a + b; }\nfn main() { let x = add(1,2); }\n".into(), category: "small".into(), line_count: 2 },
        CorpusEntry { name: "medium_fib".into(), source: "fn fib(n: int) -> int {\n    if (n <= 1) { return n; }\n    return fib(n-1)+fib(n-2);\n}\nfn main() { let r = fib(10); }\n".into(), category: "medium".into(), line_count: 5 },
        CorpusEntry { name: "medium_structs".into(), source: "struct Point { x: int; y: int; }\nfn dist(p: Point) -> int { return p.x*p.x+p.y*p.y; }\nfn main() { let p = Point { x: 3, y: 4 }; let d = dist(p); }\n".into(), category: "medium".into(), line_count: 3 },
        CorpusEntry { name: "medium_ctrl".into(), source: "fn cls(n: int) -> int {\n    if (n<0) { return 0; }\n    let r=0; let i=0;\n    while (i<n) { if (i%2==0) { r=r+i; } i=i+1; }\n    return r;\n}\nfn main() { let a=cls(50); }\n".into(), category: "medium".into(), line_count: 7 },
        CorpusEntry { name: "large_many_fns".into(), source: gen_many_fns(50), category: "large".into(), line_count: 200 },
        CorpusEntry { name: "stress_nesting".into(), source: gen_deep(20), category: "stress".into(), line_count: 60 },
    ]
}
