fn main() {
    // Force single-threaded test execution to prevent LLVM context race conditions.
    // LLVM's global state is not thread-safe when multiple LLVMContext instances
    // exist concurrently across test threads.
    println!("cargo:rustc-env=RUST_TEST_THREADS=1");
}
