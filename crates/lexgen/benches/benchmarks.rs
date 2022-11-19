// Hacky, but this is the only way I could find to share the lexer in both tests and benchmarks
include!("../tests/lua_5_1.rs");

use criterion::{black_box, criterion_group, criterion_main, Criterion};

#[inline(never)]
fn lex_lua(s: &str) {
    let lexer = Lexer::new(s);
    for _ in lexer {}
}

fn lexer_bench(c: &mut Criterion) {
    let mut str = String::new();
    str.push_str(&std::fs::read_to_string("tests/test_data").unwrap());

    for _ in 0..5 {
        let str_ = str.clone();
        str.push_str(&str_);
    }

    c.bench_function("Lex Lua files", |b| b.iter(|| lex_lua(black_box(&str))));
}

criterion_group!(benches, lexer_bench);
criterion_main!(benches);
