mod test_utils;

use lexgen::lexer;
use test_utils::next;

use std::convert::TryFrom;

#[test]
fn simple() {
    mod lexer {
        #[derive(Debug, PartialEq, Eq)]
        pub enum Token {
            Id(String),
        }

        lexgen::lexer! {
            pub Lexer -> Token;

            let init = ['a'-'z'];
            let subseq = $init | ['A'-'Z' '0'-'9' '-' '_'];

            [' ' '\t' '\n']+,

            $init $subseq* =>
                |lexer| {
                    let token = Token::Id(lexer.match_().to_owned());
                    lexer.return_(token)
                },
        }
    }

    use lexer::{Lexer, Token};

    let mut lexer = Lexer::new(" abc123Q-t  z9_9");
    assert_eq!(
        lexer.next(),
        Some(Ok((1, Token::Id("abc123Q-t".to_owned()), 10)))
    );
    assert_eq!(
        lexer.next(),
        Some(Ok((12, Token::Id("z9_9".to_owned()), 16)))
    );
    assert_eq!(lexer.next(), None);
}

// Tests user state and named rules
#[test]
fn switch_user_state() {
    #[derive(Debug, PartialEq, Eq)]
    enum Token {
        Comment,
    }

    type CommentDepth = usize;

    lexer! {
        Lexer(CommentDepth) -> Token;

        let whitespace = [' ' '\t' '\n']+;

        rule Init {
            $whitespace,

            "/*" =>
                |mut lexer| {
                    *lexer.state() = 1;
                    lexer.switch(LexerRule::Comment)
                },
        }

        rule Comment {
            "/*" =>
                |mut lexer| {
                    let state = lexer.state();
                    *state = *state + 1;
                    lexer.continue_()
                },

            "*/" =>
                |mut lexer| {
                    let state = lexer.state();
                    if *state == 1 {
                        lexer.switch_and_return(LexerRule::Init, Token::Comment)
                    } else {
                        *state = *state - 1;
                        lexer.continue_()
                    }
                },

            _ =>
                |lexer|
                    lexer.continue_(),
        }
    }

    let mut lexer = Lexer::new("  /* test  */  /* /* nested comments!! */ */");
    assert_eq!(lexer.next(), Some(Ok((2, Token::Comment, 13))));
    assert_eq!(lexer.next(), Some(Ok((15, Token::Comment, 44))));
    assert_eq!(lexer.next(), None);
}

#[test]
fn counting() {
    lexer! {
        Lexer(usize) -> usize;

        rule Init {
            ' ',

            '[' =>
                |mut lexer| {
                    *lexer.state() = 0;
                    lexer.switch(LexerRule::Count)
                },
        }

        rule Count {
            '=' =>
                |mut lexer| {
                    let n = *lexer.state();
                    *lexer.state() = n + 1;
                    lexer.continue_()
                },

            '[' =>
                |mut lexer| {
                    let n = *lexer.state();
                    lexer.switch_and_return(LexerRule::Init, n)
                },
        }
    }

    let mut lexer = Lexer::new("[[ [=[ [==[");
    assert_eq!(lexer.next(), Some(Ok((0, 0, 2))));
    assert_eq!(lexer.next(), Some(Ok((3, 1, 6))));
    assert_eq!(lexer.next(), Some(Ok((7, 2, 11))));
    assert_eq!(lexer.next(), None);
}

#[test]
fn lua_long_strings() {
    #[derive(Default, Clone, Copy)]
    struct StringBracketSize {
        left_size: usize,
        right_size: usize,
    }

    lexer! {
        LuaLongStringLexer(StringBracketSize) -> String;

        rule Init {
            ' ',

            '[' =>
                |mut lexer| {
                    *lexer.state() = Default::default();
                    lexer.switch(LuaLongStringLexerRule::LeftBracket)
                },
        }

        rule LeftBracket {
            '=' =>
                |mut lexer| {
                    lexer.state().left_size += 1;
                    lexer.continue_()
                },

            '[' =>
                |lexer|
                    lexer.switch(LuaLongStringLexerRule::String),
        }

        rule String {
            ']' =>
                |mut lexer| {
                    lexer.state().right_size = 0;
                    lexer.switch(LuaLongStringLexerRule::RightBracket)
                },

            _ =>
                |lexer|
                    lexer.continue_(),
        }

        rule RightBracket {
            '=' =>
                |mut lexer| {
                    lexer.state().right_size += 1;
                    lexer.continue_()
                },

            ']' =>
                |mut lexer| {
                    let state = *lexer.state();
                    if state.left_size == state.right_size {
                        let match_ = lexer.match_[state.left_size+2..lexer.match_.len() - state.right_size - 2].to_owned();
                        lexer.switch_and_return(LuaLongStringLexerRule::Init, match_)
                    } else {
                        lexer.switch(LuaLongStringLexerRule::String)
                    }
                },

            _ =>
                |lexer|
                    lexer.switch(LuaLongStringLexerRule::String),
        }
    }

    let mut lexer = LuaLongStringLexer::new("[[ ]] [=[test]=] [=[ ]]");
    assert_eq!(lexer.next(), Some(Ok((0, " ".to_owned(), 5))));
    assert_eq!(lexer.next(), Some(Ok((6, "test".to_owned(), 16))));
    assert!(matches!(lexer.next(), Some(Err(_))));
}

#[test]
fn simple_lifetime() {
    #[derive(Debug, PartialEq, Eq)]
    enum Token<'input> {
        Id(&'input str),
    }

    lexer! {
        Lexer -> Token<'input>;

        ' ',

        ['a'-'z']+ => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(Token::Id(match_))
        },
    }

    let mut lexer = Lexer::new("good times");
    assert_eq!(lexer.next(), Some(Ok((0, Token::Id("good"), 4))));
    assert_eq!(lexer.next(), Some(Ok((5, Token::Id("times"), 10))));
    assert_eq!(lexer.next(), None);
}

#[test]
fn rule_kind_simple() {
    #[derive(Debug, PartialEq, Eq)]
    enum Token {
        LParen,
        RParen,
    }

    lexer! {
        Lexer -> Token;

        '(' = Token::LParen,
        ')' = Token::RParen,
    }

    let mut lexer = Lexer::new("(())");
    assert_eq!(lexer.next(), Some(Ok((0, Token::LParen, 1))));
    assert_eq!(lexer.next(), Some(Ok((1, Token::LParen, 2))));
    assert_eq!(lexer.next(), Some(Ok((2, Token::RParen, 3))));
    assert_eq!(lexer.next(), Some(Ok((3, Token::RParen, 4))));
    assert_eq!(lexer.next(), None);
}

#[test]
fn rule_kind_fallible_no_lifetimes() {
    #[derive(Debug, PartialEq, Eq)]
    enum Token {
        Int(i64),
    }

    #[derive(Debug, PartialEq, Eq)]
    struct UserError(String);

    lexer! {
        Lexer -> Token;

        type Error = UserError;

        [' ' '\t' '\n'],
        ['a'-'z' '0'-'9']+ =? |lexer| {
            let match_ = lexer.match_();
            match str::parse(match_) {
                Ok(i) => lexer.return_(Ok(Token::Int(i))),
                Err(err) => lexer.return_(Err(UserError(err.to_string()))),
            }
        },
    }

    let mut lexer = Lexer::new("123 blah");
    assert_eq!(lexer.next(), Some(Ok((0, Token::Int(123), 3))));
    assert!(matches!(lexer.next(), Some(Err(LexerError::UserError(_)))));
    assert_eq!(lexer.next(), None);
}

#[test]
fn rule_kind_fallible_with_lifetimes() {
    #[derive(Debug, PartialEq, Eq)]
    enum Token {
        Int(i64),
    }

    #[derive(Debug, PartialEq, Eq)]
    struct UserError<'input>(&'input str);

    lexer! {
        Lexer -> Token;

        type Error<'input> = UserError<'input>;

        [' ' '\t' '\n'],
        ['a'-'z' '0'-'9']+ =? |lexer| {
            let match_ = lexer.match_();
            match str::parse(match_) {
                Ok(i) => lexer.return_(Ok(Token::Int(i))),
                Err(_) => lexer.return_(Err(UserError(match_))),
            }
        },
    }

    let mut lexer = Lexer::new("123 blah");
    assert_eq!(lexer.next(), Some(Ok((0, Token::Int(123), 3))));
    assert!(matches!(
        lexer.next(),
        Some(Err(LexerError::UserError(UserError("blah"))))
    ));
    assert_eq!(lexer.next(), None);
}

#[test]
fn rule_kind_mix() {
    #[derive(Debug, PartialEq, Eq)]
    enum Token {
        Int(i64),
        A,
        Other,
    }

    #[derive(Debug, PartialEq, Eq)]
    struct UserError<'input>(&'input str);

    lexer! {
        Lexer -> Token;

        type Error<'input> = UserError<'input>;

        // simple with skip
        [' ' '\t' '\n'],

        // simple with token
        "A" = Token::A,

        // fallible
        ['a'-'z' '0'-'9']+ =? |lexer| {
            let match_ = lexer.match_();
            match str::parse(match_) {
                Ok(i) => lexer.return_(Ok(Token::Int(i))),
                Err(_) => lexer.return_(Err(UserError(match_))),
            }
        },

        // infallible
        ['-' '_'] => |lexer| {
            lexer.return_(Token::Other)
        },
    }

    let mut lexer = Lexer::new("123 blah");
    assert_eq!(lexer.next(), Some(Ok((0, Token::Int(123), 3))));
    assert!(matches!(
        lexer.next(),
        Some(Err(LexerError::UserError(UserError("blah"))))
    ));
    assert_eq!(lexer.next(), None);

    let mut lexer = Lexer::new("A -");
    assert_eq!(lexer.next(), Some(Ok((0, Token::A, 1))));
    assert_eq!(lexer.next(), Some(Ok((2, Token::Other, 3))));
    assert_eq!(lexer.next(), None);
}

#[test]
fn overlapping_ranges() {
    lexer! {
        Lexer -> usize;

        ' ',
        ['a'-'b'] '1' = 1,
        ['a'-'c'] '2' = 2,
        ['b'-'c'] '3' = 3,
        'a' '4' = 4,
        'b' '5' = 5,
        'c' '6' = 6,
    }

    let mut lexer = Lexer::new("a1 b1 a2 b2 b3 c3 a4 b5 c6");
    assert_eq!(next(&mut lexer), Some(Ok(1))); // a1
    assert_eq!(next(&mut lexer), Some(Ok(1))); // b1
    assert_eq!(next(&mut lexer), Some(Ok(2))); // a2
    assert_eq!(next(&mut lexer), Some(Ok(2))); // b2
    assert_eq!(next(&mut lexer), Some(Ok(3))); // b3
    assert_eq!(next(&mut lexer), Some(Ok(3))); // c3
    assert_eq!(next(&mut lexer), Some(Ok(4))); // a4
    assert_eq!(next(&mut lexer), Some(Ok(5))); // b5
    assert_eq!(next(&mut lexer), Some(Ok(6))); // c6
    assert_eq!(lexer.next(), None);
}

#[test]
fn builtin_alphabetic() {
    lexer! {
        Lexer -> ();

        ' ',
        $$alphabetic = (),
    }

    // Test characters copied from Rust std documentation

    let mut lexer = Lexer::new("a 京 💝");
    assert_eq!(next(&mut lexer), Some(Ok(())));
    assert_eq!(next(&mut lexer), Some(Ok(())));
    assert!(matches!(next(&mut lexer), Some(Err(_))));
}

#[test]
fn builtin_alphanumeric() {
    lexer! {
        Lexer -> ();

        ' ',
        $$alphanumeric = (),
    }

    let mut lexer = Lexer::new("٣ 7 ৬ ¾ ① K و 藏");
    assert_eq!(next(&mut lexer), Some(Ok(())));
    assert_eq!(next(&mut lexer), Some(Ok(())));
    assert_eq!(next(&mut lexer), Some(Ok(())));
    assert_eq!(next(&mut lexer), Some(Ok(())));
    assert_eq!(next(&mut lexer), Some(Ok(())));
    assert_eq!(next(&mut lexer), Some(Ok(())));
    assert_eq!(next(&mut lexer), Some(Ok(())));
    assert_eq!(next(&mut lexer), Some(Ok(())));
    assert_eq!(next(&mut lexer), None);
}

#[test]
fn builtin_ascii() {
    lexer! {
        Lexer -> ();

        $$ascii = (),
    }

    for i in 0u32..128 {
        let c = char::try_from(i).unwrap();
        let mut str = String::new();
        str.push(c);

        let mut lexer = Lexer::new(&str);
        assert_eq!(next(&mut lexer), Some(Ok(())));
    }
}

#[test]
fn regex_syntax_precedence() {
    lexer! {
        Lexer -> &'input str;

        // Alternation should have less binding power than concatenation
        'a' 'b' | 'c'+ => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(match_)
        },
    }

    let mut lexer = Lexer::new("abab");
    // `+` should not cover RHS of alternation
    assert_eq!(next(&mut lexer), Some(Ok("ab")));
    assert_eq!(next(&mut lexer), Some(Ok("ab")));
    assert_eq!(next(&mut lexer), None);

    let mut lexer = Lexer::new("ccc");
    assert_eq!(next(&mut lexer), Some(Ok("ccc")));
    assert_eq!(next(&mut lexer), None);
}

#[test]
fn any_transitions() {
    lexer! {
        Lexer -> (usize, &'input str);

        "ab" => |lexer| {
            let match_ = lexer.match_();
            lexer.return_((1, match_))
        },

        _ => |lexer| {
            let match_ = lexer.match_();
            lexer.return_((2, match_))
        },
    }

    let mut lexer = Lexer::new("a");
    assert_eq!(next(&mut lexer), Some(Ok((2, "a"))));
    assert_eq!(next(&mut lexer), None);

    let mut lexer = Lexer::new("ab");
    assert_eq!(next(&mut lexer), Some(Ok((1, "ab"))));
    assert_eq!(next(&mut lexer), None);

    let mut lexer = Lexer::new("abc");
    assert_eq!(next(&mut lexer), Some(Ok((1, "ab"))));
    assert_eq!(next(&mut lexer), Some(Ok((2, "c"))));
    assert_eq!(next(&mut lexer), None);
}

#[test]
fn end_of_input_transition_1() {
    lexer! {
        Lexer -> usize;

        $ = 1,
        _ = 2,

        // Rule above should have precedence, so this should never match
        'a' = 3,
    }

    let mut lexer = Lexer::new("");
    assert_eq!(next(&mut lexer), Some(Ok(1)));
    assert_eq!(next(&mut lexer), None);

    let mut lexer = Lexer::new("a");
    assert_eq!(next(&mut lexer), Some(Ok(2)));
    assert_eq!(next(&mut lexer), Some(Ok(1)));
    assert_eq!(next(&mut lexer), None);
}

#[test]
fn end_of_input_transition_2() {
    lexer! {
        Lexer -> (usize, &'input str);

        $ => |lexer| {
            let match_ = lexer.match_();
            lexer.return_((1, match_))
        },

        _* => |lexer| {
            let match_ = lexer.match_();
            lexer.return_((2, match_))
        },
    }

    let mut lexer = Lexer::new("a");
    assert_eq!(next(&mut lexer), Some(Ok((2, "a")))); // longest match
    assert_eq!(next(&mut lexer), Some(Ok((1, ""))));
    assert_eq!(next(&mut lexer), None);
}

#[test]
fn end_of_input_transition_3() {
    lexer! {
        Lexer -> (usize, &'input str);

        "test" => |lexer| {
            let match_ = lexer.match_();
            lexer.return_((1, match_))
        },

        // TODO: need syntax for excluding characters
        "//" (['a'-'z'] | ['A'-'Z'] | ' ')* ('\n' | $) => |lexer| {
            let match_ = lexer.match_();
            lexer.return_((2, match_))
        },
    }

    let mut lexer = Lexer::new("");
    assert_eq!(next(&mut lexer), None);

    let mut lexer = Lexer::new("//");
    assert_eq!(next(&mut lexer), Some(Ok((2, "//"))));
    assert_eq!(next(&mut lexer), None);

    let mut lexer = Lexer::new("// a");
    assert_eq!(next(&mut lexer), Some(Ok((2, "// a"))));
    assert_eq!(next(&mut lexer), None);

    let mut lexer = Lexer::new("// a\n");
    assert_eq!(next(&mut lexer), Some(Ok((2, "// a\n"))));
    assert_eq!(next(&mut lexer), None);

    let mut lexer = Lexer::new("// a\ntest");
    assert_eq!(next(&mut lexer), Some(Ok((2, "// a\n"))));
    assert_eq!(next(&mut lexer), Some(Ok((1, "test"))));
    assert_eq!(next(&mut lexer), None);
}

#[test]
fn end_of_input_multiple_states() {
    // End-of-input should be matched once
    lexer! {
        Lexer -> usize;

        rule Init {
            $ = 1,

            'a' => |lexer| {
                lexer.switch(LexerRule::Rule1)
            },
        }

        rule Rule1 {
            $ = 2,
        }
    }

    let mut lexer = Lexer::new("");
    assert_eq!(next(&mut lexer), Some(Ok(1)));
    assert_eq!(next(&mut lexer), None);

    let mut lexer = Lexer::new("a");
    assert_eq!(next(&mut lexer), Some(Ok(2)));
    assert_eq!(next(&mut lexer), None);
}
