use lexgen::lexer;

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
                |mut lexer: LuaLongStringLexerHandle| {
                    *lexer.state() = Default::default();
                    lexer.switch(LuaLongStringLexerRule::LeftBracket)
                },
        }

        rule LeftBracket {
            '=' =>
                |mut lexer: LuaLongStringLexerHandle| {
                    lexer.state().left_size += 1;
                    lexer.continue_()
                },

            '[' =>
                |lexer: LuaLongStringLexerHandle|
                    lexer.switch(LuaLongStringLexerRule::String),
        }

        rule String {
            ']' =>
                |mut lexer: LuaLongStringLexerHandle| {
                    lexer.state().right_size = 0;
                    lexer.switch(LuaLongStringLexerRule::RightBracket)
                },

            _ =>
                |lexer: LuaLongStringLexerHandle|
                    lexer.continue_(),
        }

        rule RightBracket {
            '=' =>
                |mut lexer: LuaLongStringLexerHandle| {
                    lexer.state().right_size += 1;
                    lexer.continue_()
                },

            ']' =>
                |mut lexer: LuaLongStringLexerHandle| {
                    let state = *lexer.state();
                    if state.left_size == state.right_size {
                        let match_ = lexer.match_[state.left_size+2..lexer.match_.len() - state.right_size - 2].to_owned();
                        lexer.switch_and_return(LuaLongStringLexerRule::Init, match_)
                    } else {
                        lexer.switch(LuaLongStringLexerRule::String)
                    }
                },

            _ =>
                |lexer: LuaLongStringLexerHandle|
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
