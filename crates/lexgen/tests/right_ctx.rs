mod test_utils;

use lexgen::lexer;
use lexgen_util::{LexerError, LexerErrorKind, Loc};
use test_utils::{loc, next};

#[test]
fn right_ctx_1() {
    lexer! {
        Lexer -> u32;

        'a' > _ = 1,
    }

    let mut lexer = Lexer::new("aa");
    assert_eq!(next(&mut lexer), Some(Ok(1)));
    assert_eq!(next(&mut lexer), None);
}
