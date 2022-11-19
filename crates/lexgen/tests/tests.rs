mod test_utils;

use lexgen::lexer;
use lexgen_util::{LexerError, LexerErrorKind, Loc};
use test_utils::{loc, next};

use std::convert::TryFrom;

#[test]
fn readme_1() {
    lexer! {
        // First line specifies name of the lexer and the token type returned by
        // semantic actions
        Lexer -> Token;

        // Regular expressions can be named with `let` syntax
        let init = ['a'-'z'];
        let subseq = $init | ['A'-'Z' '0'-'9' '-' '_'];

        // Rule sets have names. Each rule set is compiled to a separate DFA.
        // Switching between rule sets is done explicitly in semantic actions.
        rule Init {
            // Rules without a right-hand side for skipping whitespace,
            // comments, etc.
            [' ' '\t' '\n']+,

            // Rule for matching identifiers
            $init $subseq* => |lexer| {
                let token = Token::Id(lexer.match_().to_owned());
                lexer.return_(token)
            },
        }
    }

    // The token type
    #[derive(Debug, PartialEq, Eq)]
    enum Token {
        // An identifier
        Id(String),
    }

    // Generated lexers are initialized with a `&str` for the input
    let mut lexer = Lexer::new(" abc123Q-t  z9_9");

    // Lexers implement `Iterator<Item=Result<(Loc, T, Loc), LexerError>>`,
    // where `T` is the token type specified in the lexer definition (`Token` in
    // this case), and `Loc`s indicate line, column, and byte indices of
    // beginning and end of the lexemes.
    assert_eq!(
        lexer.next(),
        Some(Ok((
            Loc {
                line: 0,
                col: 1,
                byte_idx: 1,
            },
            Token::Id("abc123Q-t".to_owned()),
            Loc {
                line: 0,
                col: 10,
                byte_idx: 10,
            }
        )))
    );
    assert_eq!(
        lexer.next(),
        Some(Ok((
            Loc {
                line: 0,
                col: 12,
                byte_idx: 12,
            },
            Token::Id("z9_9".to_owned()),
            Loc {
                line: 0,
                col: 16,
                byte_idx: 16,
            }
        )))
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn readme_2() {
    lexer! {
        Lexer(usize) -> usize;

        rule Init {
            ' ',                                            // line 5

            '[' => |lexer| {
                *lexer.state() = 0;                         // line 8
                lexer.switch(LexerRule::Count)              // line 9
            },
        }

        rule Count {
            '=' => |lexer| {
                *lexer.state() += 1;                        // line 15
                lexer.continue_()                           // line 16
            },

            '[' => |lexer| {
                let n = *lexer.state();
                lexer.switch_and_return(LexerRule::Init, n) // line 21
            },
        }
    }

    let mut lexer = Lexer::new("[[ [=[ [==[");
    assert_eq!(
        lexer.next(),
        Some(Ok((
            Loc {
                line: 0,
                col: 0,
                byte_idx: 0
            },
            0,
            Loc {
                line: 0,
                col: 2,
                byte_idx: 2
            }
        )))
    );
    assert_eq!(
        lexer.next(),
        Some(Ok((
            Loc {
                line: 0,
                col: 3,
                byte_idx: 3
            },
            1,
            Loc {
                line: 0,
                col: 6,
                byte_idx: 6
            }
        )))
    );
    assert_eq!(
        lexer.next(),
        Some(Ok((
            Loc {
                line: 0,
                col: 7,
                byte_idx: 7
            },
            2,
            Loc {
                line: 0,
                col: 11,
                byte_idx: 11
            }
        )))
    );
    assert_eq!(lexer.next(), None);
}

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

            $init $subseq* => |lexer| {
                let token = Token::Id(lexer.match_().to_owned());
                lexer.return_(token)
            },
        }
    }

    use lexer::{Lexer, Token};

    let mut lexer = Lexer::new(" abc123Q-t  z9_9");
    assert_eq!(
        lexer.next(),
        Some(Ok((
            loc(0, 1, 1),
            Token::Id("abc123Q-t".to_owned()),
            loc(0, 10, 10)
        )))
    );
    assert_eq!(
        lexer.next(),
        Some(Ok((
            loc(0, 12, 12),
            Token::Id("z9_9".to_owned()),
            loc(0, 16, 16)
        )))
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

            "/*" => |lexer| {
                *lexer.state() = 1;
                lexer.switch(LexerRule::Comment)
            },
        }

        rule Comment {
            "/*" => |lexer| {
                let state = lexer.state();
                *state += 1;
                lexer.continue_()
            },

            "*/" => |lexer| {
                let state = lexer.state();
                if *state == 1 {
                    lexer.switch_and_return(LexerRule::Init, Token::Comment)
                } else {
                    *state -= 1;
                    lexer.continue_()
                }
            },

            _ => |lexer| lexer.continue_(),
        }
    }

    let mut lexer = Lexer::new("  /* test  */  /* /* nested comments!! */ */");
    assert_eq!(
        lexer.next(),
        Some(Ok((loc(0, 2, 2), Token::Comment, loc(0, 13, 13))))
    );
    assert_eq!(
        lexer.next(),
        Some(Ok((loc(0, 15, 15), Token::Comment, loc(0, 44, 44))))
    );
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

            '[' => |lexer| {
                *lexer.state() = Default::default();
                lexer.switch(LuaLongStringLexerRule::LeftBracket)
            },
        }

        rule LeftBracket {
            '=' => |lexer| {
                lexer.state().left_size += 1;
                lexer.continue_()
            },

            '[' => |lexer| lexer.switch(LuaLongStringLexerRule::String),
        }

        rule String {
            ']' => |lexer| {
                lexer.state().right_size = 0;
                lexer.switch(LuaLongStringLexerRule::RightBracket)
            },

            _ => |lexer| lexer.continue_(),
        }

        rule RightBracket {
            '=' => |lexer| {
                lexer.state().right_size += 1;
                lexer.continue_()
            },

            ']' => |lexer| {
                let state = *lexer.state();
                if state.left_size == state.right_size {
                    let match_ = lexer.match_()[state.left_size+2..lexer.match_().len() - state.right_size - 2].to_owned();
                    lexer.switch_and_return(LuaLongStringLexerRule::Init, match_)
                } else {
                    lexer.switch(LuaLongStringLexerRule::String)
                }
            },

            _ => |lexer| lexer.switch(LuaLongStringLexerRule::String),
        }
    }

    let mut lexer = LuaLongStringLexer::new("[[ ]] [=[test]=] [=[ ]]");
    assert_eq!(
        lexer.next(),
        Some(Ok((loc(0, 0, 0), " ".to_owned(), loc(0, 5, 5))))
    );
    assert_eq!(
        lexer.next(),
        Some(Ok((loc(0, 6, 6), "test".to_owned(), loc(0, 16, 16))))
    );
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
    assert_eq!(
        lexer.next(),
        Some(Ok((loc(0, 0, 0), Token::Id("good"), loc(0, 4, 4))))
    );
    assert_eq!(
        lexer.next(),
        Some(Ok((loc(0, 5, 5), Token::Id("times"), loc(0, 10, 10))))
    );
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
    assert_eq!(
        lexer.next(),
        Some(Ok((loc(0, 0, 0), Token::LParen, loc(0, 1, 1))))
    );
    assert_eq!(
        lexer.next(),
        Some(Ok((loc(0, 1, 1), Token::LParen, loc(0, 2, 2))))
    );
    assert_eq!(
        lexer.next(),
        Some(Ok((loc(0, 2, 2), Token::RParen, loc(0, 3, 3))))
    );
    assert_eq!(
        lexer.next(),
        Some(Ok((loc(0, 3, 3), Token::RParen, loc(0, 4, 4))))
    );
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
    assert_eq!(
        lexer.next(),
        Some(Ok((loc(0, 0, 0), Token::Int(123), loc(0, 3, 3))))
    );
    assert!(matches!(
        lexer.next(),
        Some(Err(LexerError {
            kind: LexerErrorKind::Custom(_),
            ..
        }))
    ));
    assert_eq!(lexer.next(), None);
}

#[test]
fn rule_kind_fallible_with_lifetimes() {
    #[derive(Debug, PartialEq, Eq)]
    enum Token<'input> {
        Int(&'input str),
    }

    #[derive(Debug, PartialEq, Eq)]
    struct UserError<'input>(&'input str);

    lexer! {
        Lexer -> Token<'input>;

        type Error = UserError<'input>;

        [' ' '\t' '\n'],
        ['a'-'z' '0'-'9']+ =? |lexer| {
            let match_ = lexer.match_();
            match str::parse::<i64>(match_) {
                Ok(_) => lexer.return_(Ok(Token::Int(match_))),
                Err(_) => lexer.return_(Err(UserError(match_))),
            }
        },
    }

    let mut lexer = Lexer::new("123 blah");
    assert_eq!(
        lexer.next(),
        Some(Ok((loc(0, 0, 0), Token::Int("123"), loc(0, 3, 3))))
    );
    assert!(matches!(
        lexer.next(),
        Some(Err(LexerError {
            kind: LexerErrorKind::Custom(UserError("blah")),
            ..
        }))
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

        type Error = UserError<'input>;

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
    assert_eq!(
        lexer.next(),
        Some(Ok((loc(0, 0, 0), Token::Int(123), loc(0, 3, 3))))
    );
    assert!(matches!(
        lexer.next(),
        Some(Err(LexerError {
            kind: LexerErrorKind::Custom(UserError("blah")),
            ..
        }))
    ));
    assert_eq!(lexer.next(), None);

    let mut lexer = Lexer::new("A -");
    assert_eq!(
        lexer.next(),
        Some(Ok((loc(0, 0, 0), Token::A, loc(0, 1, 1))))
    );
    assert_eq!(
        lexer.next(),
        Some(Ok((loc(0, 2, 2), Token::Other, loc(0, 3, 3))))
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn overlapping_ranges_1() {
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
fn overlapping_ranges_2() {
    lexer! {
        Lexer -> usize;

        ' ',
        'a' = 1,
        ['a'-'b'] = 2,
        ['a'-'c'] = 3,
        ['b'-'c'] = 4,
        'b' = 5,
        'c' = 6,
    }

    let mut lexer = Lexer::new("a b c");
    assert_eq!(next(&mut lexer), Some(Ok(1)));
    assert_eq!(next(&mut lexer), Some(Ok(2)));
    assert_eq!(next(&mut lexer), Some(Ok(3)));
    assert_eq!(lexer.next(), None);
}

#[test]
fn overlapping_ranges_3() {
    lexer! {
        Lexer1 -> usize;

        ' ',
        ['a'-'b'] = 1,
        (['a'-'b'] | ['a'-'b']) = 2,
    }

    let mut lexer = Lexer1::new("a b");
    assert_eq!(next(&mut lexer), Some(Ok(1)));
    assert_eq!(next(&mut lexer), Some(Ok(1)));
    assert_eq!(lexer.next(), None);

    // Compile test (compilation shouldn't panic). Tests the same case as above. Extracted from
    // Rust lexer.
    lexer! {
        Lexer2 -> &'input str;

        let oct_digit = ['0'-'7'];
        let dec_digit = ['0'-'9'];
        let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F'];
        let bin_digit = '0' | '1';
        let digit = $oct_digit | $dec_digit | $hex_digit | $bin_digit;

        let id = $$XID_Start $$XID_Continue*;

        ("0b" | "0o" | "0x")? ($digit | '_')* $id? => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(match_)
        },
    }
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
    assert_eq!(lexer.next(), Some(Ok((loc(0, 0, 0), (), loc(0, 1, 1)))));
    assert_eq!(lexer.next(), Some(Ok((loc(0, 2, 2), (), loc(0, 4, 5)))));
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
    assert_eq!(lexer.next(), Some(Ok((loc(0, 0, 0), (), loc(0, 1, 2))))); // 2 bytes
    assert_eq!(lexer.next(), Some(Ok((loc(0, 2, 3), (), loc(0, 3, 4)))));
    assert_eq!(lexer.next(), Some(Ok((loc(0, 4, 5), (), loc(0, 5, 8))))); // 3 bytes
    assert_eq!(lexer.next(), Some(Ok((loc(0, 6, 9), (), loc(0, 7, 11))))); // 2 bytes
    assert_eq!(lexer.next(), Some(Ok((loc(0, 8, 12), (), loc(0, 9, 15))))); // 3 bytes
    assert_eq!(lexer.next(), Some(Ok((loc(0, 10, 16), (), loc(0, 11, 17)))));
    assert_eq!(lexer.next(), Some(Ok((loc(0, 12, 18), (), loc(0, 13, 20))))); // 2 bytes
    assert_eq!(lexer.next(), Some(Ok((loc(0, 14, 21), (), loc(0, 16, 24))))); // 3 bytes, wide
    assert_eq!(lexer.next(), None);
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

#[test]
fn switch_and_reset_match() {
    #[derive(Default)]
    struct State {
        enable_reset_match: bool,
    }

    lexer! {
        Lexer(State) -> &'input str;

        rule Init {
            $ = "_",

            'a' => |lexer| {
                lexer.switch_and_return(LexerRule::Rule1, "a")
            },
        }

        rule Rule1 {
            'c' => |lexer| {
                if lexer.state().enable_reset_match {
                    lexer.reset_match();
                }
                lexer.continue_()
            },

            "!" => |lexer| {
                lexer.reset_match();
                let enable_reset_match = &mut lexer.state().enable_reset_match;
                *enable_reset_match = !*enable_reset_match;
                lexer.continue_()
            },

            ['d' 'e']+ => |lexer| {
                let s = lexer.match_();
                lexer.return_(s)
            },

            $ = "<>",
        }
    }

    let mut lexer = Lexer::new("accdeed!ccdeed");
    assert_eq!(next(&mut lexer), Some(Ok("a")));
    assert_eq!(next(&mut lexer), Some(Ok("ccdeed")));
    assert_eq!(next(&mut lexer), Some(Ok("deed")));
    assert_eq!(next(&mut lexer), Some(Ok("<>")));
}

#[test]
fn char_lit() {
    lexer! {
        Lexer -> &'input str;

        "'" _ "'" => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(match_)
        },
    }

    let mut lexer = Lexer::new("'a'");
    assert_eq!(next(&mut lexer), Some(Ok("'a'")));
    assert_eq!(next(&mut lexer), None);
}

#[test]
fn multiple_lexers_in_scope() {
    lexer! {
        Lexer1 -> usize;

        'a' = 1,
    }

    lexer! {
        Lexer2 -> usize;

        'a' = 2,
    }

    let mut lexer = Lexer1::new("a");
    assert_eq!(next(&mut lexer), Some(Ok(1)));
    assert_eq!(next(&mut lexer), None);

    let mut lexer = Lexer2::new("a");
    assert_eq!(next(&mut lexer), Some(Ok(2)));
    assert_eq!(next(&mut lexer), None);
}

#[test]
fn loc_tracking() {
    lexer! {
        Lexer -> &'input str;

        rule Init {
            _ => |lexer| lexer.switch(LexerRule::Rule1),
        }

        rule Rule1 {
            '\n' => |lexer| {
                let match_ = lexer.match_();
                lexer.return_(match_)
            },

            _,

            $ => |lexer| {
                let match_ = lexer.match_();
                lexer.return_(match_)
            },
        }
    }

    let mut lexer = Lexer::new("Ｈｅｌｌｏ,\nｗｏｒｌｄ!!!");
    assert_eq!(
        lexer.next(),
        Some(Ok((loc(0, 0, 0), "Ｈｅｌｌｏ,\n", loc(1, 0, 17))))
    );
    assert_eq!(
        lexer.next(),
        Some(Ok((loc(1, 0, 17), "ｗｏｒｌｄ!!!", loc(1, 13, 35))))
    );
}

#[test]
fn diff_1() {
    lexer! {
        Lexer -> &'input str;

        let exclude = ['3'-'7'];

        ['0'-'9'] # $exclude => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(match_)
        },
    }

    let mut lexer = Lexer::new("01289");
    assert_eq!(next(&mut lexer), Some(Ok("0")));
    assert_eq!(next(&mut lexer), Some(Ok("1")));
    assert_eq!(next(&mut lexer), Some(Ok("2")));
    assert_eq!(next(&mut lexer), Some(Ok("8")));
    assert_eq!(next(&mut lexer), Some(Ok("9")));
    assert_eq!(next(&mut lexer), None);

    let mut lexer = Lexer::new("34567");
    assert!(matches!(next(&mut lexer), Some(Err(_))));
    assert!(matches!(next(&mut lexer), Some(Err(_))));
    assert!(matches!(next(&mut lexer), Some(Err(_))));
    assert!(matches!(next(&mut lexer), Some(Err(_))));
    assert!(matches!(next(&mut lexer), Some(Err(_))));
    assert!(matches!(next(&mut lexer), None));
}

#[test]
fn diff_2() {
    lexer! {
        Lexer -> &'input str;

        _ # 'a' => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(match_)
        },
    }

    let mut lexer = Lexer::new("b");
    assert_eq!(next(&mut lexer), Some(Ok("b")));
    assert!(matches!(next(&mut lexer), None));

    let mut lexer = Lexer::new("a");
    assert!(matches!(next(&mut lexer), Some(Err(_))));
    assert!(matches!(next(&mut lexer), None));
}

#[test]
fn diff_3() {
    lexer! {
        Lexer -> &'input str;

        "'" (_ # ('\t' | '\n' | '\\' | '\'')) "'" => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(match_)
        },
    }

    let mut lexer = Lexer::new("''");
    assert!(matches!(next(&mut lexer), Some(Err(_))));

    let mut lexer = Lexer::new("'''");
    assert!(matches!(next(&mut lexer), Some(Err(_))));

    let mut lexer = Lexer::new("'\t'");
    assert!(matches!(next(&mut lexer), Some(Err(_))));

    let mut lexer = Lexer::new("'a'");
    assert_eq!(next(&mut lexer), Some(Ok("'a'")));
    assert_eq!(next(&mut lexer), None);
}

#[test]
fn diff_4() {
    lexer! {
        Lexer -> &'input str;

        "//" (_ # '\n')* $? => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(match_)
        },
    }

    let mut lexer = Lexer::new("// asdf");
    assert_eq!(next(&mut lexer), Some(Ok("// asdf")));
    assert_eq!(next(&mut lexer), None);

    let mut lexer = Lexer::new("// asdf\n");
    assert_eq!(next(&mut lexer), Some(Ok("// asdf")));
    assert_eq!(
        next(&mut lexer),
        Some(Err(LexerError {
            location: loc(0, 7, 7),
            kind: LexerErrorKind::InvalidToken,
        }))
    );
}

#[test]
fn iter_interface_simple() {
    // Tests `new_from_iter` with simple rules
    lexer! {
        Lexer -> usize;

        'a' = 1,
        'b' = 2,
    }

    let mut lexer = Lexer::new_from_iter("ab".chars());
    assert_eq!(next(&mut lexer), Some(Ok(1)));
    assert_eq!(next(&mut lexer), Some(Ok(2)));
    assert_eq!(next(&mut lexer), None);
}

#[test]
fn iter_interface_infallible() {
    // Tests `new_from_iter` with infallible rules
    lexer! {
        Lexer(String) -> u32;

        ['0'-'9']+ => |lexer| {
            let (start, end) = lexer.match_loc();
            let str = lexer.state();
            let val = str::parse::<u32>(&str[start.byte_idx..end.byte_idx]).unwrap();
            lexer.return_(val)
        },
    }

    let input = "123";
    let mut lexer = Lexer::new_from_iter_with_state(input.chars(), input.to_owned());
    assert_eq!(next(&mut lexer), Some(Ok(123)));
    assert_eq!(next(&mut lexer), None);
}

#[test]
fn iter_interface_fallible() {
    // Tests `new_from_iter` with fallible rules
    lexer! {
        Lexer(String) -> u32;

        type Error = std::num::ParseIntError;

        $$ascii_alphanumeric+ =? |lexer| {
            let (start, end) = lexer.match_loc();
            let str = lexer.state();
            match str::parse::<u32>(&str[start.byte_idx..end.byte_idx]) {
                Ok(i) => lexer.return_(Ok(i)),
                Err(err) => lexer.return_(Err(err)),
            }
        },
    }

    let input = "123";
    let mut lexer = Lexer::new_from_iter_with_state(input.chars(), input.to_owned());
    assert_eq!(next(&mut lexer), Some(Ok(123)));
    assert_eq!(next(&mut lexer), None);

    let input = "a";
    let mut lexer = Lexer::new_from_iter_with_state(input.chars(), input.to_owned());
    assert!(matches!(
        next(&mut lexer),
        Some(Err(LexerError {
            kind: LexerErrorKind::Custom(_),
            ..
        }))
    ));
}

#[test]
fn user_state_lifetimes() {
    struct State<'a> {
        buffer: &'a mut String,
    }

    lexer! {
        Lexer(State<'a>) -> ();

        rule Init {
            $$ascii_whitespace,
            '"' => |lexer| {
                lexer.reset_match();
                lexer.switch(LexerRule::String)
            },
        }

        rule String {
            '"' => |lexer| {
                lexer.switch_and_return(LexerRule::Init, ())
            },
            _ => |lexer| {
                let match_ = lexer.match_();
                lexer.state().buffer.push_str(match_);
                lexer.reset_match();
                lexer.continue_()
            },
        }
    }

    let mut buffer = String::new();
    let mut lexer = Lexer::new_with_state(
        "\"ab\" \"cd\"",
        State {
            buffer: &mut buffer,
        },
    );
    assert_eq!(next(&mut lexer), Some(Ok(())));
    assert_eq!(next(&mut lexer), Some(Ok(())));
    assert_eq!(next(&mut lexer), None);
    assert_eq!(buffer, "abcd");
}

#[test]
fn lifetime_named_input() {
    struct State<'input> {
        vec: Vec<&'input str>,
    }

    lexer! {
        Lexer(State<'input>) -> ();

        rule Init {
            $$ascii_whitespace,
            '"' => |lexer| {
                lexer.reset_match();
                lexer.switch(LexerRule::String)
            },
        }

        rule String {
            '"' => |lexer| {
                let match_ = lexer.match_();
                lexer.state().vec.push(&match_[..match_.len()-1]);
                lexer.switch_and_return(LexerRule::Init, ())
            },
            _,
        }
    }

    let mut lexer = Lexer::new_with_state("\"a\" \"b\"", State { vec: Vec::new() });
    assert_eq!(next(&mut lexer), Some(Ok(())));
    assert_eq!(next(&mut lexer), Some(Ok(())));
    assert_eq!(next(&mut lexer), None);
    assert_eq!(lexer.state().vec, vec!["a", "b"]);
}

#[test]
fn static_and_input() {
    struct State<'a, 'b, 'c> {
        words: Vec<&'a str>,
        word: &'b str,
        counter: &'c mut i32,
    }

    lexer! {
        Lexer(State<'input, 'static, 'c>) -> ();

        rule Init {
            $$ascii_whitespace,
            '"' => |lexer| {
                lexer.reset_match();
                lexer.switch(LexerRule::String)
            },
        }

        rule String {
            '"' => |lexer| {
                let match_ = lexer.match_();
                let s = &match_[..match_.len()-1];
                if s != lexer.state().word {
                    lexer.state().words.push(s)
                } else {
                    *lexer.state().counter += 1
                }
                lexer.switch_and_return(LexerRule::Init, ())
            },
            _,
        }
    }

    let mut counter = 0;
    let state = State {
        words: Vec::new(),
        word: "Hello",
        counter: &mut counter,
    };
    let test = "\"Hello\" \"world\"".to_owned(); // try non-static input
    let mut lexer: Lexer<'_, '_, _> = Lexer::new_with_state(&test, state);
    assert_eq!(next(&mut lexer), Some(Ok(())));
    assert_eq!(next(&mut lexer), Some(Ok(())));
    assert_eq!(next(&mut lexer), None);
    assert_eq!(lexer.state().words, vec!["world"]);
    assert_eq!(*lexer.state().counter, 1);
}
