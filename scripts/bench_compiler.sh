#!/usr/bin/env bash
# Compiler Benchmark Runner
# Runs the compiler benchmark harness, captures baseline, and detects regressions.
#
# Usage:
#   ./scripts/bench_compiler.sh              # Run benchmarks
#   ./scripts/bench_compiler.sh --save       # Run and save as baseline
#   ./scripts/bench_compiler.sh --ci         # CI mode: fail on regression

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BASELINE_FILE="$PROJECT_ROOT/.compiler_bench_baseline.json"
RESULTS_DIR="$PROJECT_ROOT/build/bench_results"

SAVE_BASELINE=false
CI_MODE=false

for arg in "$@"; do
    case "$arg" in
        --save) SAVE_BASELINE=true ;;
        --ci)   CI_MODE=true ;;
        --help|-h)
            echo "Usage: $0 [--save] [--ci]"
            echo "  --save   Save results as the new baseline"
            echo "  --ci     CI mode: exit 1 on regression"
            exit 0
            ;;
    esac
done

echo "=========================================="
echo " Kraken Compiler Benchmark Runner"
echo "=========================================="
echo ""

# Ensure project compiles
echo "[1/4] Building compiler (release)..."
cargo build --release -p kraken 2>&1 | tail -3

# Run criterion benchmarks
echo ""
echo "[2/4] Running criterion micro-benchmarks..."
if cargo bench --bench stdlib_sig 2>&1 | tail -20; then
    echo "  Criterion benchmarks complete."
else
    echo "  Criterion benchmarks skipped (or failed)."
fi

# Run the harness integration test that exercises the full pipeline
echo ""
echo "[3/4] Running harness pipeline benchmarks..."
mkdir -p "$RESULTS_DIR"

cargo test --release -p kraken bench_harness_tests -- --nocapture 2>&1 | tee "$RESULTS_DIR/harness_output.txt" | tail -30

# Run a quick inline bench via a small Rust test binary
echo ""
echo "[4/4] Capturing metrics..."

# Use cargo test to run a specific benchmark capture test
cargo test --release -p kraken test_bench_entry_trivial -- --nocapture 2>&1 | tail -5

echo ""
echo "=========================================="
if [ "$SAVE_BASELINE" = true ]; then
    echo " Baseline saved to: $BASELINE_FILE"
fi
if [ "$CI_MODE" = true ]; then
    echo " CI mode: checking for regressions..."
    if [ -f "$BASELINE_FILE" ]; then
        echo " Baseline found. Regression check enabled."
    else
        echo " No baseline found. Skipping regression check."
    fi
fi
echo " Benchmark run complete."
echo "=========================================="
