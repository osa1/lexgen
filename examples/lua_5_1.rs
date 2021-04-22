//! Lexes a given Lua 5.1 file and prints the tokens. Runs some tests when run without an argument.

// TODOs:
//
// - Exclude newlines in strings
// - Locale-dependant alphabetic chars in variables (???)

use lexer_gen::lexer_gen;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() == 1 {
        run_tests();
    } else {
        let files = &args[1..];
        for file in files {
            let contents = std::fs::read_to_string(file).unwrap();
            let lexer = Lexer::new(&contents, Default::default());
            let mut tokens: Vec<Token> = vec![];
            for token in lexer {
                match token {
                    Err(err) => {
                        eprintln!("Unable to lex {:?}: {:?}", file, err);
                        std::process::exit(1);
                    }
                    Ok((_, tok, _)) => {
                        tokens.push(tok);
                    }
                }
            }
            println!("{:?}: {:?}", file, tokens);
        }
    }
}

fn run_tests() {
    lex_lua_simple();
    lex_lua_var();
    lex_lua_string();
    lex_lua_long_string();
    lex_lua_number();
    lex_lua_comment();
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                      Lexer definition and tests                            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, PartialEq, Eq, Clone)]
enum Token {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    Hash,
    EqEq,
    TildeEq,
    LtEq,
    GtEq,
    Lt,
    Gt,
    Eq,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Semicolon,
    Colon,
    Comma,
    Dot,
    DotDot,
    DotDotDot,
    Keyword(Keyword),
    String(String),
    Var(String),
    Number(String), // uninterpreted
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Keyword {
    And,
    Break,
    Do,
    Else,
    ElseIf,
    End,
    False,
    For,
    Function,
    If,
    In,
    Local,
    Nil,
    Not,
    Or,
    Repeat,
    Return,
    Then,
    True,
    Until,
    While,
}

#[derive(Debug, Default, Clone)]
struct LexerState {
    /// Number of opening `=`s seen when parsing a long string
    long_string_opening_eqs: usize,
    /// Number of closing `=`s seen when parsing a long string
    long_string_closing_eqs: usize,
    /// When parsing a short string, whether it's started with a double or single quote
    short_string_delim: Quote,
    /// Buffer for strings
    string_buf: String,
    /// When parsing a long string, whether we're inside a comment or not. When inside a comment we
    /// don't return a token. Otherwise we return a string.
    in_comment: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Quote {
    Single,
    Double,
}

impl Default for Quote {
    fn default() -> Self {
        // arbitrary
        Quote::Single
    }
}

lexer_gen! {
    Lexer(LexerState) -> Token;

    let whitespace = [' ' '\t' '\n'];

    // > Names (also called identifiers) in Lua can be any string of letters, digits, and
    // > underscores, not beginning with a digit. This coincides with the definition of names in
    // > most languages. (The definition of letter depends on the current locale: any character
    // > considered alphabetic by the current locale can be used in an identifier.)
    let var_init = ['a'-'z' 'A'-'Z' '_'];
    let var_subseq = $var_init | ['0'-'9'];

    let digit = ['0'-'9'];
    let hex_digit = ['a'-'f' 'A'-'F' '0'-'9'];

    rule Init {
        $whitespace,

        "+" => |handle| handle.return_(Token::Plus),
        "-" => |handle| handle.return_(Token::Minus),
        "*" => |handle| handle.return_(Token::Star),
        "/" => |handle| handle.return_(Token::Slash),
        "%" => |handle| handle.return_(Token::Percent),
        "^" => |handle| handle.return_(Token::Caret),
        "#" => |handle| handle.return_(Token::Hash),
        "==" => |handle| handle.return_(Token::EqEq),
        "~=" => |handle| handle.return_(Token::TildeEq),
        "<=" => |handle| handle.return_(Token::LtEq),
        ">=" => |handle| handle.return_(Token::GtEq),
        "<" => |handle| handle.return_(Token::Lt),
        ">" => |handle| handle.return_(Token::Gt),
        "=" => |handle| handle.return_(Token::Eq),
        "(" => |handle| handle.return_(Token::LParen),
        ")" => |handle| handle.return_(Token::RParen),
        "{" => |handle| handle.return_(Token::LBrace),
        "}" => |handle| handle.return_(Token::RBrace),
        "]" => |handle| handle.return_(Token::RBracket),
        ";" => |handle| handle.return_(Token::Semicolon),
        ":" => |handle| handle.return_(Token::Colon),
        "," => |handle| handle.return_(Token::Comma),
        "." => |handle| handle.return_(Token::Dot),
        ".." => |handle| handle.return_(Token::DotDot),
        "..." => |handle| handle.return_(Token::DotDotDot),
        "and" => |handle| handle.return_(Token::Keyword(Keyword::And)),
        "break" => |handle| handle.return_(Token::Keyword(Keyword::Break)),
        "do" => |handle| handle.return_(Token::Keyword(Keyword::Do)),
        "else" => |handle| handle.return_(Token::Keyword(Keyword::Else)),
        "elseif" => |handle| handle.return_(Token::Keyword(Keyword::ElseIf)),
        "end" => |handle| handle.return_(Token::Keyword(Keyword::End)),
        "false" => |handle| handle.return_(Token::Keyword(Keyword::False)),
        "for" => |handle| handle.return_(Token::Keyword(Keyword::For)),
        "function" => |handle| handle.return_(Token::Keyword(Keyword::Function)),
        "if" => |handle| handle.return_(Token::Keyword(Keyword::If)),
        "in" => |handle| handle.return_(Token::Keyword(Keyword::In)),
        "local" => |handle| handle.return_(Token::Keyword(Keyword::Local)),
        "nil" => |handle| handle.return_(Token::Keyword(Keyword::Nil)),
        "not" => |handle| handle.return_(Token::Keyword(Keyword::Not)),
        "or" => |handle| handle.return_(Token::Keyword(Keyword::Or)),
        "repeat" => |handle| handle.return_(Token::Keyword(Keyword::Repeat)),
        "return" => |handle| handle.return_(Token::Keyword(Keyword::Return)),
        "then" => |handle| handle.return_(Token::Keyword(Keyword::Then)),
        "true" => |handle| handle.return_(Token::Keyword(Keyword::True)),
        "until" => |handle| handle.return_(Token::Keyword(Keyword::Until)),
        "while" => |handle| handle.return_(Token::Keyword(Keyword::While)),

        '"' => |mut handle| {
            handle.state().short_string_delim = Quote::Double;
            handle.state().string_buf.clear();
            handle.switch(LexerRules::String)
        },

        '\'' => |mut handle| {
            handle.state().short_string_delim = Quote::Single;
            handle.state().string_buf.clear();
            handle.switch(LexerRules::String)
        },

        "[" => |mut handle| {
            match handle.peek() {
                Some('[') | Some('=') => {
                    handle.state().long_string_opening_eqs = 0;
                    handle.state().in_comment = false;
                    handle.switch(LexerRules::LongStringBracketLeft)
                }
                _ => handle.return_(Token::LBracket),
            }
        },

        "--" => |handle| {
            handle.switch(LexerRules::EnterComment)
        },

        $var_init $var_subseq* => |handle| {
            let match_ = handle.match_().to_owned();
            handle.return_(Token::Var(match_))
        },

        $digit+ ('.'? $digit+ (('e' | 'E') ('+'|'-')? $digit+)?)? =>
            |handle| {
                let match_ = handle.match_().to_owned();
                handle.return_(Token::Number(match_))
            },

        "0x" $hex_digit+ => |handle| {
            let match_ = handle.match_().to_owned();
            handle.return_(Token::Number(match_))
        },
    }

    rule LongStringBracketLeft {
        '=' =>
            |mut handle| {
                handle.state().long_string_opening_eqs += 1;
                handle.continue_()
            },

        '[' =>
            |handle|
                handle.switch(LexerRules::LongString),
    }

    rule LongString {
        ']' =>
            |mut handle| {
                handle.state().long_string_closing_eqs = 0;
                handle.switch(LexerRules::LongStringBracketRight)
            },

        _ =>
            |handle|
                handle.continue_(),
    }

    rule LongStringBracketRight {
        '=' =>
            |mut handle| {
                handle.state().long_string_closing_eqs += 1;
                handle.continue_()
            },

        ']' =>
            |mut handle| {
                let state = handle.state();
                let in_comment = state.in_comment;
                let left_eqs = state.long_string_opening_eqs;
                let right_eqs = state.long_string_closing_eqs;
                if left_eqs == right_eqs {
                    if in_comment {
                        handle.switch(LexerRules::Init)
                    } else {
                        let match_ = handle.match_[left_eqs + 2..handle.match_.len() - right_eqs - 2].to_owned();
                        handle.switch_and_return(LexerRules::Init, Token::String(match_))
                    }
                } else {
                    handle.switch(LexerRules::String)
                }
            },

        _ =>
            |handle|
                handle.switch(LexerRules::String),
    }

    rule String {
        '"' => |mut handle| {
            if handle.state().short_string_delim == Quote::Double {
                let str = handle.state().string_buf.clone();
                handle.switch_and_return(LexerRules::Init, Token::String(str))
            } else {
                handle.state().string_buf.push('"');
                handle.continue_()
            }
        },

        "'" => |mut handle| {
            if handle.state().short_string_delim == Quote::Single {
                let str = handle.state().string_buf.clone();
                handle.switch_and_return(LexerRules::Init, Token::String(str))
            } else {
                handle.state().string_buf.push('\'');
                handle.continue_()
            }
        },

        "\\a" => |mut handle| {
            handle.state().string_buf.push('\u{7}');
            handle.continue_()
        },

        "\\b" => |mut handle| {
            handle.state().string_buf.push('\u{8}');
            handle.continue_()
        },

        "\\f" => |mut handle| {
            handle.state().string_buf.push('\u{c}');
            handle.continue_()
        },

        "\\n" => |mut handle| {
            handle.state().string_buf.push('\n');
            handle.continue_()
        },

        "\\r" => |mut handle| {
            handle.state().string_buf.push('\r');
            handle.continue_()
        },

        "\\t" => |mut handle| {
            handle.state().string_buf.push('\t');
            handle.continue_()
        },

        "\\v" => |mut handle| {
            handle.state().string_buf.push('\u{b}');
            handle.continue_()
        },

        "\\\\" => |mut handle| {
            handle.state().string_buf.push('\\');
            handle.continue_()
        },

        "\\\"" => |mut handle| {
            handle.state().string_buf.push('"');
            handle.continue_()
        },

        "\\'" => |mut handle| {
            handle.state().string_buf.push('\'');
            handle.continue_()
        },

        "\\\n" => |mut handle| {
            handle.state().string_buf.push('\n');
            handle.continue_()
        },

        _ => |mut handle| {
            let char = handle.match_().chars().next_back().unwrap();
            handle.state().string_buf.push(char);
            handle.continue_()
        },
    }

    rule EnterComment {
        '[' => |mut handle| {
            match handle.peek() {
                Some('[') | Some('=') => {
                    handle.state().long_string_opening_eqs = 0;
                    handle.state().in_comment = true;
                    handle.switch(LexerRules::LongStringBracketLeft)
                }
                _ =>
                    handle.switch(LexerRules::Comment),
            }
        },

        _ => |handle|
            handle.switch(LexerRules::Comment),
    }

    rule Comment {
        '\n' => |handle|
            handle.switch(LexerRules::Init),

        _ => |handle|
            handle.continue_(),
    }
}

fn ignore_pos<A, E>(ret: Option<Result<(usize, A, usize), E>>) -> Option<Result<A, E>> {
    ret.map(|res| res.map(|(_, a, _)| a))
}

fn lex_lua_number() {
    let mut lexer = Lexer::new(
        "3 3.0 3.1416 314.16e-2 0.31416E1 0xff 0x56",
        Default::default(),
    );

    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Number("3".to_owned())))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Number("3.0".to_owned())))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Number("3.1416".to_owned())))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Number("314.16e-2".to_owned())))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Number("0.31416E1".to_owned())))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Number("0xff".to_owned())))
    );

    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Number("0x56".to_owned())))
    );
}

fn lex_lua_string() {
    let str = "
            \"test\"
            \"\\
test'\\\"\"
        ";
    let mut lexer = Lexer::new(str, Default::default());

    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::String("test".to_owned())))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::String("\ntest'\"".to_owned())))
    );
}

fn lex_lua_long_string() {
    let mut lexer = Lexer::new("[[ ]] [=[test]=] [=[ ]]", Default::default());
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::String(" ".to_owned())))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::String("test".to_owned()))),
    );
    assert!(matches!(lexer.next(), Some(Err(_))));
}

fn lex_lua_comment() {
    let mut lexer = Lexer::new(
        "-- test
         +
         --[[test
         test]]+
        ",
        Default::default(),
    );
    assert_eq!(ignore_pos(lexer.next()), Some(Ok(Token::Plus)));
    assert_eq!(ignore_pos(lexer.next()), Some(Ok(Token::Plus)));
    assert_eq!(ignore_pos(lexer.next()), None);
}

fn lex_lua_var() {
    let str = "ab ab1 ab_1_2 Aab";
    let mut lexer = Lexer::new(str, Default::default());

    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Var("ab".to_owned())))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Var("ab1".to_owned())))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Var("ab_1_2".to_owned())))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Var("Aab".to_owned())))
    );
}

fn lex_lua_simple() {
    let lexer = Lexer::new(
        "+ - * / % ^ # == ~= <= >= < > = ( ) { } [ ] \
         ; : , . .. ... and break do else elseif end \
         false for function if in local nil not or repeat \
         return then true until while n",
        Default::default(),
    );

    let mut tokens: Vec<Token> = vec![];
    for token in lexer {
        tokens.push(token.unwrap().1);
    }

    assert_eq!(
        tokens,
        vec![
            Token::Plus,
            Token::Minus,
            Token::Star,
            Token::Slash,
            Token::Percent,
            Token::Caret,
            Token::Hash,
            Token::EqEq,
            Token::TildeEq,
            Token::LtEq,
            Token::GtEq,
            Token::Lt,
            Token::Gt,
            Token::Eq,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::LBracket,
            Token::RBracket,
            Token::Semicolon,
            Token::Colon,
            Token::Comma,
            Token::Dot,
            Token::DotDot,
            Token::DotDotDot,
            Token::Keyword(Keyword::And),
            Token::Keyword(Keyword::Break),
            Token::Keyword(Keyword::Do),
            Token::Keyword(Keyword::Else),
            Token::Keyword(Keyword::ElseIf),
            Token::Keyword(Keyword::End),
            Token::Keyword(Keyword::False),
            Token::Keyword(Keyword::For),
            Token::Keyword(Keyword::Function),
            Token::Keyword(Keyword::If),
            Token::Keyword(Keyword::In),
            Token::Keyword(Keyword::Local),
            Token::Keyword(Keyword::Nil),
            Token::Keyword(Keyword::Not),
            Token::Keyword(Keyword::Or),
            Token::Keyword(Keyword::Repeat),
            Token::Keyword(Keyword::Return),
            Token::Keyword(Keyword::Then),
            Token::Keyword(Keyword::True),
            Token::Keyword(Keyword::Until),
            Token::Keyword(Keyword::While),
            Token::Var("n".to_owned()),
        ]
    );
}
