use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn compile_bench(c: &mut Criterion) {
    let bench_data = std::fs::read_to_string("bench_data").unwrap();
    let lexer_ast = syn::parse_str::<lexgen::ast::Lexer>(&bench_data).unwrap();
    c.bench_function("compile", |b| {
        b.iter(|| lexgen::lexer(black_box(lexer_ast.clone())))
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(10);
    targets = compile_bench
}
criterion_main!(benches);
