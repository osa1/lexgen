// A Lua 5.1 lexer. We use this as
//
// - An example: this file is linked from README
//
// - A test: `test_data` contains all Lua files in Lua 5.1 source distribution, we lex it using
//   this lexer as a test.
//
// - A benchmark: We also use `test_data` lexing time as a runtime benchmark.

use lexgen::lexer;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                      Lexer definition and tests                            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, PartialEq, Eq, Clone)]
enum Token<'input> {
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
    String(StringToken<'input>),
    Var(&'input str),
    Number(&'input str), // uninterpreted
}

/// Raw string tokens are borrowed from the input string. Interpreted strings are copied and owned.
#[derive(Debug, PartialEq, Eq, Clone)]
enum StringToken<'input> {
    Raw(&'input str),
    Interpreted(String),
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
    Lexer(LexerState) -> Token<'input>;

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

        "+" = Token::Plus,
        "-" = Token::Minus,
        "*" = Token::Star,
        "/" = Token::Slash,
        "%" = Token::Percent,
        "^" = Token::Caret,
        "#" = Token::Hash,
        "==" = Token::EqEq,
        "~=" = Token::TildeEq,
        "<=" = Token::LtEq,
        ">=" = Token::GtEq,
        "<" = Token::Lt,
        ">" = Token::Gt,
        "=" = Token::Eq,
        "(" = Token::LParen,
        ")" = Token::RParen,
        "{" = Token::LBrace,
        "}" = Token::RBrace,
        "]" = Token::RBracket,
        ";" = Token::Semicolon,
        ":" = Token::Colon,
        "," = Token::Comma,
        "." = Token::Dot,
        ".." = Token::DotDot,
        "..." = Token::DotDotDot,
        "and" = Token::Keyword(Keyword::And),
        "break" = Token::Keyword(Keyword::Break),
        "do" = Token::Keyword(Keyword::Do),
        "else" = Token::Keyword(Keyword::Else),
        "elseif" = Token::Keyword(Keyword::ElseIf),
        "end" = Token::Keyword(Keyword::End),
        "false" = Token::Keyword(Keyword::False),
        "for" = Token::Keyword(Keyword::For),
        "function" = Token::Keyword(Keyword::Function),
        "if" = Token::Keyword(Keyword::If),
        "in" = Token::Keyword(Keyword::In),
        "local" = Token::Keyword(Keyword::Local),
        "nil" = Token::Keyword(Keyword::Nil),
        "not" = Token::Keyword(Keyword::Not),
        "or" = Token::Keyword(Keyword::Or),
        "repeat" = Token::Keyword(Keyword::Repeat),
        "return" = Token::Keyword(Keyword::Return),
        "then" = Token::Keyword(Keyword::Then),
        "true" = Token::Keyword(Keyword::True),
        "until" = Token::Keyword(Keyword::Until),
        "while" = Token::Keyword(Keyword::While),

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
            let match_ = lexer.match_();
            lexer.return_(Token::Var(match_))
        },

        $digit+ ('.'? $digit+ (('e' | 'E') ('+'|'-')? $digit+)?)? =>
            |lexer| {
                let match_ = lexer.match_();
                lexer.return_(Token::Number(match_))
            },

        "0x" $hex_digit+ => |lexer| {
            let match_ = lexer.match_();
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
                        let match_ = &lexer.match_[left_eqs + 2..lexer.match_.len() - right_eqs - 2];
                        lexer.switch_and_return(LexerRule::Init, Token::String(StringToken::Raw(match_)))
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
                lexer.switch_and_return(LexerRule::Init, Token::String(StringToken::Interpreted(str)))
            } else {
                lexer.state().string_buf.push('"');
                lexer.continue_()
            }
        },

        "'" => |mut lexer| {
            if lexer.state().short_string_delim == Quote::Single {
                let str = lexer.state().string_buf.clone();
                lexer.switch_and_return(LexerRule::Init, Token::String(StringToken::Interpreted(str)))
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

#[test]
fn lex_lua_number() {
    let mut lexer = Lexer::new("3 3.0 3.1416 314.16e-2 0.31416E1 0xff 0x56");

    assert_eq!(ignore_pos(lexer.next()), Some(Ok(Token::Number("3"))));
    assert_eq!(ignore_pos(lexer.next()), Some(Ok(Token::Number("3.0"))));
    assert_eq!(ignore_pos(lexer.next()), Some(Ok(Token::Number("3.1416"))));
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Number("314.16e-2")))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Number("0.31416E1")))
    );
    assert_eq!(ignore_pos(lexer.next()), Some(Ok(Token::Number("0xff"))));

    assert_eq!(ignore_pos(lexer.next()), Some(Ok(Token::Number("0x56"))));
}

#[test]
fn lex_lua_string() {
    let str = "
            \"test\"
            \"\\
test'\\\"\"
        ";
    let mut lexer = Lexer::new(str);

    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::String(StringToken::Interpreted(
            "test".to_owned()
        ))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::String(StringToken::Interpreted(
            "\ntest'\"".to_owned()
        ))))
    );
}

#[test]
fn lex_lua_long_string() {
    let mut lexer = Lexer::new("[[ ]] [=[test]=] [=[ ]]");
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::String(StringToken::Raw(" "))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::String(StringToken::Raw("test")))),
    );
    assert!(matches!(lexer.next(), Some(Err(_))));
}

#[test]
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

#[test]
fn lex_lua_var() {
    let str = "ab ab1 ab_1_2 Aab";
    let mut lexer = Lexer::new(str);

    assert_eq!(ignore_pos(lexer.next()), Some(Ok(Token::Var("ab"))));
    assert_eq!(ignore_pos(lexer.next()), Some(Ok(Token::Var("ab1"))));
    assert_eq!(ignore_pos(lexer.next()), Some(Ok(Token::Var("ab_1_2"))));
    assert_eq!(ignore_pos(lexer.next()), Some(Ok(Token::Var("Aab"))));
}

#[test]
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
            Token::Var("n"),
        ]
    );
}

#[test]
fn lex_lua_windows_line_ending() {
    let mut lexer = Lexer::new("+\r\n+");
    assert_eq!(ignore_pos(lexer.next()), Some(Ok(Token::Plus)));
    assert_eq!(ignore_pos(lexer.next()), Some(Ok(Token::Plus)));
    assert_eq!(ignore_pos(lexer.next()), None);
}

#[test]
fn lex_lua_files() {
    let str = std::fs::read_to_string("tests/test_data").unwrap();
    let mut lexer = Lexer::new(&str);
    let mut i = 0;
    while let Some(tok) = lexer.next() {
        assert!(tok.is_ok());
        i += 1;
    }
    println!("{} tokens", i);
}
