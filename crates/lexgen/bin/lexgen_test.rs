use lexgen::lexer;

fn main() {
    let file = std::env::args().nth(1).unwrap();
    let contents = std::fs::read_to_string(file).unwrap();
    lexer(&contents);
}
