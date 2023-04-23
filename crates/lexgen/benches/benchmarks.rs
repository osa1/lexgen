use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn compile_bench(c: &mut Criterion) {
    let bench_data = std::fs::read_to_string("/home/omer/rust/lexgen/bench_data").unwrap();
    c.bench_function("compile", |b| {
        b.iter(|| lexgen::lexer(black_box(&bench_data)))
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(10);
    targets = compile_bench
}

criterion_main!(benches);
