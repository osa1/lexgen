//! Lexes a given Lua 5.1 file and prints the tokens. Runs some tests when run without an argument.

// TODOs:
//
// - Exclude newlines in strings
// - Locale-dependant alphabetic chars in variables (???)

use lexgen::lexer;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() == 1 {
        run_tests();
    } else {
        let files = &args[1..];
        for file in files {
            let contents = std::fs::read_to_string(file).unwrap();
            let lexer = Lexer::new(&contents);
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
    lex_lua_windows_line_ending();
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

lexer! {
    Lexer(LexerState) -> Token;

    let whitespace = [' ' '\t' '\n'] | "\r\n";

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

        "+" => |lexer| lexer.return_(Token::Plus),
        "-" => |lexer| lexer.return_(Token::Minus),
        "*" => |lexer| lexer.return_(Token::Star),
        "/" => |lexer| lexer.return_(Token::Slash),
        "%" => |lexer| lexer.return_(Token::Percent),
        "^" => |lexer| lexer.return_(Token::Caret),
        "#" => |lexer| lexer.return_(Token::Hash),
        "==" => |lexer| lexer.return_(Token::EqEq),
        "~=" => |lexer| lexer.return_(Token::TildeEq),
        "<=" => |lexer| lexer.return_(Token::LtEq),
        ">=" => |lexer| lexer.return_(Token::GtEq),
        "<" => |lexer| lexer.return_(Token::Lt),
        ">" => |lexer| lexer.return_(Token::Gt),
        "=" => |lexer| lexer.return_(Token::Eq),
        "(" => |lexer| lexer.return_(Token::LParen),
        ")" => |lexer| lexer.return_(Token::RParen),
        "{" => |lexer| lexer.return_(Token::LBrace),
        "}" => |lexer| lexer.return_(Token::RBrace),
        "]" => |lexer| lexer.return_(Token::RBracket),
        ";" => |lexer| lexer.return_(Token::Semicolon),
        ":" => |lexer| lexer.return_(Token::Colon),
        "," => |lexer| lexer.return_(Token::Comma),
        "." => |lexer| lexer.return_(Token::Dot),
        ".." => |lexer| lexer.return_(Token::DotDot),
        "..." => |lexer| lexer.return_(Token::DotDotDot),
        "and" => |lexer| lexer.return_(Token::Keyword(Keyword::And)),
        "break" => |lexer| lexer.return_(Token::Keyword(Keyword::Break)),
        "do" => |lexer| lexer.return_(Token::Keyword(Keyword::Do)),
        "else" => |lexer| lexer.return_(Token::Keyword(Keyword::Else)),
        "elseif" => |lexer| lexer.return_(Token::Keyword(Keyword::ElseIf)),
        "end" => |lexer| lexer.return_(Token::Keyword(Keyword::End)),
        "false" => |lexer| lexer.return_(Token::Keyword(Keyword::False)),
        "for" => |lexer| lexer.return_(Token::Keyword(Keyword::For)),
        "function" => |lexer| lexer.return_(Token::Keyword(Keyword::Function)),
        "if" => |lexer| lexer.return_(Token::Keyword(Keyword::If)),
        "in" => |lexer| lexer.return_(Token::Keyword(Keyword::In)),
        "local" => |lexer| lexer.return_(Token::Keyword(Keyword::Local)),
        "nil" => |lexer| lexer.return_(Token::Keyword(Keyword::Nil)),
        "not" => |lexer| lexer.return_(Token::Keyword(Keyword::Not)),
        "or" => |lexer| lexer.return_(Token::Keyword(Keyword::Or)),
        "repeat" => |lexer| lexer.return_(Token::Keyword(Keyword::Repeat)),
        "return" => |lexer| lexer.return_(Token::Keyword(Keyword::Return)),
        "then" => |lexer| lexer.return_(Token::Keyword(Keyword::Then)),
        "true" => |lexer| lexer.return_(Token::Keyword(Keyword::True)),
        "until" => |lexer| lexer.return_(Token::Keyword(Keyword::Until)),
        "while" => |lexer| lexer.return_(Token::Keyword(Keyword::While)),

        '"' => |mut lexer| {
            lexer.state().short_string_delim = Quote::Double;
            lexer.state().string_buf.clear();
            lexer.switch(LexerRule::String)
        },

        '\'' => |mut lexer| {
            lexer.state().short_string_delim = Quote::Single;
            lexer.state().string_buf.clear();
            lexer.switch(LexerRule::String)
        },

        "[" => |mut lexer| {
            match lexer.peek() {
                Some('[') | Some('=') => {
                    lexer.state().long_string_opening_eqs = 0;
                    lexer.state().in_comment = false;
                    lexer.switch(LexerRule::LongStringBracketLeft)
                }
                _ => lexer.return_(Token::LBracket),
            }
        },

        "--" => |lexer| {
            lexer.switch(LexerRule::EnterComment)
        },

        $var_init $var_subseq* => |lexer| {
            let match_ = lexer.match_().to_owned();
            lexer.return_(Token::Var(match_))
        },

        $digit+ ('.'? $digit+ (('e' | 'E') ('+'|'-')? $digit+)?)? =>
            |lexer| {
                let match_ = lexer.match_().to_owned();
                lexer.return_(Token::Number(match_))
            },

        "0x" $hex_digit+ => |lexer| {
            let match_ = lexer.match_().to_owned();
            lexer.return_(Token::Number(match_))
        },
    }

    rule LongStringBracketLeft {
        '=' =>
            |mut lexer| {
                lexer.state().long_string_opening_eqs += 1;
                lexer.continue_()
            },

        '[' =>
            |lexer|
                lexer.switch(LexerRule::LongString),
    }

    rule LongString {
        ']' =>
            |mut lexer| {
                lexer.state().long_string_closing_eqs = 0;
                lexer.switch(LexerRule::LongStringBracketRight)
            },

        _ =>
            |lexer|
                lexer.continue_(),
    }

    rule LongStringBracketRight {
        '=' =>
            |mut lexer| {
                lexer.state().long_string_closing_eqs += 1;
                lexer.continue_()
            },

        ']' =>
            |mut lexer| {
                let state = lexer.state();
                let in_comment = state.in_comment;
                let left_eqs = state.long_string_opening_eqs;
                let right_eqs = state.long_string_closing_eqs;
                if left_eqs == right_eqs {
                    if in_comment {
                        lexer.switch(LexerRule::Init)
                    } else {
                        let match_ = lexer.match_[left_eqs + 2..lexer.match_.len() - right_eqs - 2].to_owned();
                        lexer.switch_and_return(LexerRule::Init, Token::String(match_))
                    }
                } else {
                    lexer.switch(LexerRule::String)
                }
            },

        _ =>
            |lexer|
                lexer.switch(LexerRule::String),
    }

    rule String {
        '"' => |mut lexer| {
            if lexer.state().short_string_delim == Quote::Double {
                let str = lexer.state().string_buf.clone();
                lexer.switch_and_return(LexerRule::Init, Token::String(str))
            } else {
                lexer.state().string_buf.push('"');
                lexer.continue_()
            }
        },

        "'" => |mut lexer| {
            if lexer.state().short_string_delim == Quote::Single {
                let str = lexer.state().string_buf.clone();
                lexer.switch_and_return(LexerRule::Init, Token::String(str))
            } else {
                lexer.state().string_buf.push('\'');
                lexer.continue_()
            }
        },

        "\\a" => |mut lexer| {
            lexer.state().string_buf.push('\u{7}');
            lexer.continue_()
        },

        "\\b" => |mut lexer| {
            lexer.state().string_buf.push('\u{8}');
            lexer.continue_()
        },

        "\\f" => |mut lexer| {
            lexer.state().string_buf.push('\u{c}');
            lexer.continue_()
        },

        "\\n" => |mut lexer| {
            lexer.state().string_buf.push('\n');
            lexer.continue_()
        },

        "\\r" => |mut lexer| {
            lexer.state().string_buf.push('\r');
            lexer.continue_()
        },

        "\\t" => |mut lexer| {
            lexer.state().string_buf.push('\t');
            lexer.continue_()
        },

        "\\v" => |mut lexer| {
            lexer.state().string_buf.push('\u{b}');
            lexer.continue_()
        },

        "\\\\" => |mut lexer| {
            lexer.state().string_buf.push('\\');
            lexer.continue_()
        },

        "\\\"" => |mut lexer| {
            lexer.state().string_buf.push('"');
            lexer.continue_()
        },

        "\\'" => |mut lexer| {
            lexer.state().string_buf.push('\'');
            lexer.continue_()
        },

        "\\\n" => |mut lexer| {
            lexer.state().string_buf.push('\n');
            lexer.continue_()
        },

        _ => |mut lexer| {
            let char = lexer.match_().chars().next_back().unwrap();
            lexer.state().string_buf.push(char);
            lexer.continue_()
        },
    }

    rule EnterComment {
        '[' => |mut lexer| {
            match lexer.peek() {
                Some('[') | Some('=') => {
                    lexer.state().long_string_opening_eqs = 0;
                    lexer.state().in_comment = true;
                    lexer.switch(LexerRule::LongStringBracketLeft)
                }
                _ =>
                    lexer.switch(LexerRule::Comment),
            }
        },

        _ => |lexer|
            lexer.switch(LexerRule::Comment),
    }

    rule Comment {
        '\n' => |lexer|
            lexer.switch(LexerRule::Init),

        _ => |lexer|
            lexer.continue_(),
    }
}

fn ignore_pos<A, E>(ret: Option<Result<(usize, A, usize), E>>) -> Option<Result<A, E>> {
    ret.map(|res| res.map(|(_, a, _)| a))
}

fn lex_lua_number() {
    let mut lexer = Lexer::new("3 3.0 3.1416 314.16e-2 0.31416E1 0xff 0x56");

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
    let mut lexer = Lexer::new(str);

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
    let mut lexer = Lexer::new("[[ ]] [=[test]=] [=[ ]]");
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
    );
    assert_eq!(ignore_pos(lexer.next()), Some(Ok(Token::Plus)));
    assert_eq!(ignore_pos(lexer.next()), Some(Ok(Token::Plus)));
    assert_eq!(ignore_pos(lexer.next()), None);
}

fn lex_lua_var() {
    let str = "ab ab1 ab_1_2 Aab";
    let mut lexer = Lexer::new(str);

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

fn lex_lua_windows_line_ending() {
    let mut lexer = Lexer::new("+\r\n+");
    assert_eq!(ignore_pos(lexer.next()), Some(Ok(Token::Plus)));
    assert_eq!(ignore_pos(lexer.next()), Some(Ok(Token::Plus)));
    assert_eq!(ignore_pos(lexer.next()), None);
}
