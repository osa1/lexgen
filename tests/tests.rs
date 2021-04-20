use lexer_gen::lexer_gen;

#[test]
fn simple() {
    #[derive(Debug, PartialEq, Eq)]
    enum Token {
        Id(String),
    }

    lexer_gen! {
        Lexer -> Token;

        let init = ['a'-'z'];
        let subseq = $init | ['A'-'Z' '0'-'9' '-' '_'];

        rule Init {
            [' ' '\t' '\n']+,
            $init $subseq* =>
                |handle: LexerHandle| {
                    let token = Token::Id(handle.match_().to_owned());
                    handle.return_(token)
                },
        }
    }

    let mut lexer = Lexer::new(" abc123Q-t  z9_9", ());
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

    lexer_gen! {
        Lexer(CommentDepth) -> Token;

        let whitespace = [' ' '\t' '\n']+;

        rule Init {
            $whitespace,

            "/*" =>
                |mut handle: LexerHandle| {
                    *handle.state() = 1;
                    handle.switch(LexerRules::Comment)
                },
        }

        rule Comment {
            "/*" =>
                |mut handle: LexerHandle| {
                    let state = handle.state();
                    *state = *state + 1;
                    handle.continue_()
                },

            "*/" =>
                |mut handle: LexerHandle| {
                    let state = handle.state();
                    if *state == 1 {
                        handle.switch_and_return(LexerRules::Init, Token::Comment)
                    } else {
                        *state = *state - 1;
                        handle.continue_()
                    }
                },

            _ =>
                |handle: LexerHandle|
                    handle.continue_(),
        }
    }

    let mut lexer = Lexer::new("  /* test  */  /* /* nested comments!! */ */", 0);
    assert_eq!(lexer.next(), Some(Ok((2, Token::Comment, 13))));
    assert_eq!(lexer.next(), Some(Ok((15, Token::Comment, 44))));
    assert_eq!(lexer.next(), None);
}

#[test]
fn counting() {
    lexer_gen! {
        Lexer(usize) -> usize;

        rule Init {
            ' ',

            '[' =>
                |mut handle: LexerHandle| {
                    *handle.state() = 0;
                    handle.switch(LexerRules::Count)
                },
        },

        rule Count {
            '=' =>
                |mut handle: LexerHandle| {
                    let n = *handle.state();
                    *handle.state() = n + 1;
                    handle.continue_()
                },

            '[' =>
                |mut handle: LexerHandle| {
                    let n = *handle.state();
                    handle.switch_and_return(LexerRules::Init, n)
                },
        },
    }

    let mut lexer = Lexer::new("[[ [=[ [==[", 0);
    assert_eq!(lexer.next(), Some(Ok((0, 0, 2))));
    assert_eq!(lexer.next(), Some(Ok((3, 1, 6))));
    assert_eq!(lexer.next(), Some(Ok((7, 2, 11))));
    assert_eq!(lexer.next(), None);
}

#[test]
fn lua_long_strings() {
    #[derive(Default)]
    struct StringBracketSize {
        left_size: usize,
        right_size: usize,
    }

    lexer_gen! {
        LuaLongStringLexer(StringBracketSize) -> String;

        rule Init {
            ' ',

            '[' =>
                |mut handle: LuaLongStringLexerHandle| {
                    *handle.state() = Default::default();
                    handle.switch(LuaLongStringLexerRules::LeftBracket)
                },
        },

        rule LeftBracket {
            '=' =>
                |mut handle: LuaLongStringLexerHandle| {
                    handle.state().left_size += 1;
                    handle.continue_()
                },

            '[' =>
                |handle: LuaLongStringLexerHandle|
                    handle.switch(LuaLongStringLexerRules::String),
        },

        rule String {
            ']' =>
                |mut handle: LuaLongStringLexerHandle| {
                    handle.state().right_size = 0;
                    handle.switch(LuaLongStringLexerRules::RightBracket)
                },

            _ =>
                |handle: LuaLongStringLexerHandle|
                    handle.continue_(),
        },

        rule RightBracket {
            '=' =>
                |mut handle: LuaLongStringLexerHandle| {
                    handle.state().right_size += 1;
                    handle.continue_()
                },

            ']' =>
                |mut handle: LuaLongStringLexerHandle| {
                    let state = handle.state();
                    if state.left_size == state.right_size {
                        let match_ = handle.match_.to_owned();
                        handle.switch_and_return(LuaLongStringLexerRules::Init, match_)
                    } else {
                        handle.switch(LuaLongStringLexerRules::String)
                    }
                },

            _ =>
                |handle: LuaLongStringLexerHandle|
                    handle.switch(LuaLongStringLexerRules::String),
        }
    }

    let mut lexer = LuaLongStringLexer::new("[[]]", Default::default());
    assert_eq!(lexer.next(), Some(Ok((0, "".to_owned(), 4))));
}
