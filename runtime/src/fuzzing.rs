//! Fuzzing infrastructure for testing compiler robustness.
//!
//! Provides tools for generating and testing random inputs.

use std::collections::HashSet;

/// Fuzzing input generator.
#[derive(Debug, Clone)]
pub struct FuzzGenerator {
    seed: u64,
    max_depth: usize,
    max_length: usize,
}

impl FuzzGenerator {
    /// Create a new fuzz generator with a seed.
    pub fn new(seed: u64) -> Self {
        Self {
            seed,
            max_depth: 10,
            max_length: 1000,
        }
    }

    /// Set maximum recursion depth.
    pub fn with_max_depth(mut self, depth: usize) -> Self {
        self.max_depth = depth;
        self
    }

    /// Set maximum string length.
    pub fn with_max_length(mut self, length: usize) -> Self {
        self.max_length = length;
        self
    }

    /// Generate a random identifier.
    pub fn generate_identifier(&mut self) -> String {
        let chars = "abcdefghijklmnopqrstuvwxyz_";
        let len = (self.next_u64() % 20) + 1;

        (0..len)
            .map(|_| {
                let idx = (self.next_u64() % chars.len() as u64) as usize;
                chars.chars().nth(idx).unwrap()
            })
            .collect()
    }

    /// Generate a random integer.
    pub fn generate_int(&mut self) -> i64 {
        self.next_u64() as i64
    }

    /// Generate a random float.
    pub fn generate_float(&mut self) -> f64 {
        (self.next_u64() as f64) / (u64::MAX as f64)
    }

    /// Generate a random boolean.
    pub fn generate_bool(&mut self) -> bool {
        (self.next_u64() % 2) == 0
    }

    /// Generate a random string.
    pub fn generate_string(&mut self) -> String {
        let len = (self.next_u64() % self.max_length as u64) as usize;
        let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 !@#$%^&*()";

        (0..len)
            .map(|_| {
                let idx = (self.next_u64() % chars.len() as u64) as usize;
                chars.chars().nth(idx).unwrap()
            })
            .collect()
    }

    /// Generate a random token sequence.
    pub fn generate_tokens(&mut self, count: usize) -> Vec<String> {
        let tokens = vec![
            "fn", "let", "if", "else", "return", "struct", "enum", "impl", "trait", "type",
            "const", "static", "pub", "priv", "mut", "ref", "unsafe", "(", ")", "{", "}", "[", "]",
            ";", ",", ":", "::", "->", "=>", "+", "-", "*", "/", "%", "==", "!=", "<", ">", "<=",
            ">=", "&&", "||", "!", "&", "|", "^", "<<", ">>",
        ];

        (0..count)
            .map(|_| {
                let idx = (self.next_u64() % tokens.len() as u64) as usize;
                tokens[idx].to_string()
            })
            .collect()
    }

    /// Simple LCG random number generator.
    fn next_u64(&mut self) -> u64 {
        self.seed = self.seed.wrapping_mul(6364136223846793005).wrapping_add(1);
        self.seed
    }
}

/// Fuzzing corpus for storing test cases.
#[derive(Debug, Clone)]
pub struct FuzzCorpus {
    inputs: HashSet<Vec<u8>>,
    max_size: usize,
}

impl FuzzCorpus {
    /// Create a new fuzzing corpus.
    pub fn new(max_size: usize) -> Self {
        Self {
            inputs: HashSet::new(),
            max_size,
        }
    }

    /// Add an input to the corpus.
    pub fn add(&mut self, input: Vec<u8>) -> bool {
        if self.inputs.len() >= self.max_size {
            return false;
        }
        self.inputs.insert(input)
    }

    /// Get the number of inputs in the corpus.
    pub fn len(&self) -> usize {
        self.inputs.len()
    }

    /// Check if the corpus is empty.
    pub fn is_empty(&self) -> bool {
        self.inputs.is_empty()
    }

    /// Get all inputs.
    pub fn inputs(&self) -> Vec<Vec<u8>> {
        self.inputs.iter().cloned().collect()
    }

    /// Clear the corpus.
    pub fn clear(&mut self) {
        self.inputs.clear();
    }
}

/// Fuzzing statistics.
#[derive(Debug, Clone, Default)]
pub struct FuzzStats {
    pub total_runs: usize,
    pub crashes: usize,
    pub timeouts: usize,
    pub unique_crashes: HashSet<String>,
}

impl FuzzStats {
    /// Create new fuzzing statistics.
    pub fn new() -> Self {
        Self::default()
    }

    /// Record a successful run.
    pub fn record_success(&mut self) {
        self.total_runs += 1;
    }

    /// Record a crash.
    pub fn record_crash(&mut self, crash_info: String) {
        self.total_runs += 1;
        self.crashes += 1;
        self.unique_crashes.insert(crash_info);
    }

    /// Record a timeout.
    pub fn record_timeout(&mut self) {
        self.total_runs += 1;
        self.timeouts += 1;
    }

    /// Get crash rate as a percentage.
    pub fn crash_rate(&self) -> f64 {
        if self.total_runs == 0 {
            0.0
        } else {
            (self.crashes as f64 / self.total_runs as f64) * 100.0
        }
    }

    /// Get timeout rate as a percentage.
    pub fn timeout_rate(&self) -> f64 {
        if self.total_runs == 0 {
            0.0
        } else {
            (self.timeouts as f64 / self.total_runs as f64) * 100.0
        }
    }

    /// Generate a statistics report.
    pub fn report(&self) -> String {
        format!(
            "Fuzzing Statistics:\n\
             Total Runs: {}\n\
             Crashes: {} ({:.2}%)\n\
             Unique Crashes: {}\n\
             Timeouts: {} ({:.2}%)\n",
            self.total_runs,
            self.crashes,
            self.crash_rate(),
            self.unique_crashes.len(),
            self.timeouts,
            self.timeout_rate()
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fuzz_generator() {
        let mut gen = FuzzGenerator::new(12345);

        let id = gen.generate_identifier();
        assert!(!id.is_empty());
        assert!(id.len() <= 21);

        let _int = gen.generate_int();
        let _float = gen.generate_float();
        let _bool = gen.generate_bool();
    }

    #[test]
    fn test_fuzz_generator_string() {
        let mut gen = FuzzGenerator::new(54321).with_max_length(100);
        let s = gen.generate_string();
        assert!(s.len() <= 100);
    }

    #[test]
    fn test_fuzz_generator_tokens() {
        let mut gen = FuzzGenerator::new(99999);
        let tokens = gen.generate_tokens(10);
        assert_eq!(tokens.len(), 10);
    }

    #[test]
    fn test_fuzz_corpus() {
        let mut corpus = FuzzCorpus::new(100);

        assert!(corpus.add(vec![1, 2, 3]));
        assert_eq!(corpus.len(), 1);

        assert!(!corpus.add(vec![1, 2, 3])); // Duplicate
        assert_eq!(corpus.len(), 1);

        assert!(corpus.add(vec![4, 5, 6]));
        assert_eq!(corpus.len(), 2);
    }

    #[test]
    fn test_fuzz_corpus_max_size() {
        let mut corpus = FuzzCorpus::new(2);

        assert!(corpus.add(vec![1]));
        assert!(corpus.add(vec![2]));
        assert!(!corpus.add(vec![3])); // Exceeds max size

        assert_eq!(corpus.len(), 2);
    }

    #[test]
    fn test_fuzz_stats() {
        let mut stats = FuzzStats::new();

        stats.record_success();
        stats.record_success();
        stats.record_crash("crash1".to_string());
        stats.record_timeout();

        assert_eq!(stats.total_runs, 4);
        assert_eq!(stats.crashes, 1);
        assert_eq!(stats.timeouts, 1);
        assert_eq!(stats.unique_crashes.len(), 1);
    }

    #[test]
    fn test_fuzz_stats_rates() {
        let mut stats = FuzzStats::new();

        for _ in 0..8 {
            stats.record_success();
        }
        stats.record_crash("crash".to_string());
        stats.record_timeout();

        assert_eq!(stats.total_runs, 10);
        assert_eq!(stats.crash_rate(), 10.0);
        assert_eq!(stats.timeout_rate(), 10.0);
    }

    #[test]
    fn test_fuzz_stats_report() {
        let mut stats = FuzzStats::new();
        stats.record_success();
        stats.record_crash("test crash".to_string());

        let report = stats.report();
        assert!(report.contains("Fuzzing Statistics"));
        assert!(report.contains("Total Runs: 2"));
    }

    #[test]
    fn test_deterministic_generation() {
        let mut gen1 = FuzzGenerator::new(12345);
        let mut gen2 = FuzzGenerator::new(12345);

        assert_eq!(gen1.generate_identifier(), gen2.generate_identifier());
        assert_eq!(gen1.generate_int(), gen2.generate_int());
    }
}
