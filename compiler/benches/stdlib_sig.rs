use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn bench_stdlib_sig_lookup(c: &mut Criterion) {
    c.bench_function("stdlib_sig_lookup", |b| {
        b.iter(|| {
            black_box(kraken::ffi::stdlib::stdlib_sig("memcmp"));
            black_box(kraken::ffi::stdlib::stdlib_sig("malloc"));
            black_box(kraken::ffi::stdlib::stdlib_sig("fopen"));
            black_box(kraken::ffi::stdlib::stdlib_sig("rename"));
        })
    });
}

criterion_group!(benches, bench_stdlib_sig_lookup);
criterion_main!(benches);
