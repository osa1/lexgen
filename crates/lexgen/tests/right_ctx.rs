mod test_utils;

use lexgen::lexer;
use lexgen_util::{LexerError, LexerErrorKind};
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

#[test]
fn rust_single_line_comment() {
    lexer! {
        Lexer -> &'input str;

        rule Init {
            $$ascii_whitespace,

            "//" => |lexer| lexer.switch(LexerRule::SinglelineComment),
        }

        rule SinglelineComment {
            (_ # '\n')* > ('\n' | $) => |lexer| {
                let comment = lexer.match_();
                lexer.switch_and_return(LexerRule::Init, comment)
            },
        }
    }

    // Terminated at the end of input (no newline)
    let input = "//  /  ";
    let mut lexer = Lexer::new(input);
    assert_eq!(next(&mut lexer), Some(Ok(input)));
    assert_eq!(next(&mut lexer), None);

    // Terminated with newlines
    let input = "//  /  \n";
    let mut lexer = Lexer::new(input);
    assert_eq!(next(&mut lexer), Some(Ok("//  /  ")));
    assert_eq!(next(&mut lexer), None);

    // Empty comment, terminated with eof
    let input = "//";
    let mut lexer = Lexer::new(input);
    assert_eq!(next(&mut lexer), Some(Ok("//")));
    assert_eq!(next(&mut lexer), None);

    // Empty comment, terminated with eol
    let input = "//\n";
    let mut lexer = Lexer::new(input);
    assert_eq!(next(&mut lexer), Some(Ok("//")));
    assert_eq!(next(&mut lexer), None);
}

#[test]
fn rust_float() {
    enum Token<'input> {
        Float(&'input str),
        Int(&'input str),
        Range,
    }

    lexer! {
        Lexer -> Token<'input>;

        ['0'-'9']+ '.' > (_ # ('.' | '_' | $$XID_Start) | $) => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(Token::Float(match_))
        },

        ['0'-'9']+ => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(Token::Int(match_))
        },

        ".." = Token::Range,
    }
}
