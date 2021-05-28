use lexgen::ast::Lexer;
use lexgen::lexer;

fn main() {
    let file = std::env::args().nth(1).unwrap();
    let contents = std::fs::read_to_string(file).unwrap();
    let lexer_ast = syn::parse_str::<Lexer>(&contents).unwrap();
    lexer(lexer_ast);
}
