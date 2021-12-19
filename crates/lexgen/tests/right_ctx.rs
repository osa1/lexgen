mod test_utils;

use lexgen::lexer;
use lexgen_util::{LexerError, LexerErrorKind, Loc};
use test_utils::{loc, next};

#[test]
fn right_ctx_1() {
    lexer! {
        Lexer -> u32;

        'a' > 'a' = 1,
    }

    let mut lexer = Lexer::new("aa");
    assert_eq!(next(&mut lexer), Some(Ok(1)));
    assert_eq!(
        next(&mut lexer),
        Some(Err(LexerError {
            location: loc(0, 1, 1),
            kind: LexerErrorKind::InvalidToken,
        }))
    );

    let mut lexer = Lexer::new("ab");
    assert_eq!(
        next(&mut lexer),
        Some(Err(LexerError {
            location: loc(0, 0, 0),
            kind: LexerErrorKind::InvalidToken,
        }))
    );
}

#[test]
fn right_ctx_2() {
    lexer! {
        Lexer -> u32;

        'a' > _ = 1,
    }

    let mut lexer = Lexer::new("aa");
    assert_eq!(next(&mut lexer), Some(Ok(1)));
    assert_eq!(
        next(&mut lexer),
        Some(Err(LexerError {
            location: loc(0, 1, 1),
            kind: LexerErrorKind::InvalidToken,
        }))
    );

    let mut lexer = Lexer::new("ab");
    assert_eq!(next(&mut lexer), Some(Ok(1)));
    assert_eq!(
        next(&mut lexer),
        Some(Err(LexerError {
            location: loc(0, 1, 1),
            kind: LexerErrorKind::InvalidToken,
        }))
    );

    let mut lexer = Lexer::new("a");
    assert_eq!(
        next(&mut lexer),
        Some(Err(LexerError {
            location: loc(0, 0, 0),
            kind: LexerErrorKind::InvalidToken,
        }))
    );
}

#[test]
fn right_ctx_3() {
    lexer! {
        Lexer -> u32;

        'a' > $ = 1,
    }

    let mut lexer = Lexer::new("a");
    assert_eq!(next(&mut lexer), Some(Ok(1)));
    assert_eq!(next(&mut lexer), None);

    let mut lexer = Lexer::new("ab");
    assert_eq!(
        next(&mut lexer),
        Some(Err(LexerError {
            location: loc(0, 0, 0),
            kind: LexerErrorKind::InvalidToken,
        }))
    );
}

#[test]
fn right_ctx_4() {
    lexer! {
        Lexer -> u32;

        'a' > 'a' = 1,
        'a' > $ = 2,
    }

    let mut lexer = Lexer::new("a");
    assert_eq!(next(&mut lexer), Some(Ok(2)));
    assert_eq!(next(&mut lexer), None);

    let mut lexer = Lexer::new("aa");
    assert_eq!(next(&mut lexer), Some(Ok(1)));
    assert_eq!(next(&mut lexer), Some(Ok(2)));
    assert_eq!(next(&mut lexer), None);
}

// TODO: Implement this test in simulation
#[test]
fn right_ctx_5() {
    lexer! {
        Lexer -> u32;

        // Per longest match we "a" should return 2, not 1
        'a' = 1,
        'a' > $ = 2,
        'a' > 'a' = 3,
    }

    let mut lexer = Lexer::new("a");
    assert_eq!(next(&mut lexer), Some(Ok(2)));
    assert_eq!(next(&mut lexer), None);

    let mut lexer = Lexer::new("aa");
    assert_eq!(next(&mut lexer), Some(Ok(3)));
    assert_eq!(next(&mut lexer), Some(Ok(2)));
    assert_eq!(next(&mut lexer), None);
}
