// TODOs:
//
// - Exclude newlines in strings
// - Locale-dependant alphabetic chars in variables (???)

use lexer_gen::lexer_gen;

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
    lon_string_opening_eqs: usize,
    /// Number of closing `=`s seen when parsing a long string
    long_string_closing_eqs: usize,
    /// When parsing a short string, whether it's started with a double or single quote
    short_string_delim: Quote,
    /// Buffer for strings
    string_buf: String,
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

    rule Init {
        $whitespace,

        "+" => |handle: LexerHandle| handle.return_(Token::Plus),
        "-" => |handle: LexerHandle| handle.return_(Token::Minus),
        "*" => |handle: LexerHandle| handle.return_(Token::Star),
        "/" => |handle: LexerHandle| handle.return_(Token::Slash),
        "%" => |handle: LexerHandle| handle.return_(Token::Percent),
        "^" => |handle: LexerHandle| handle.return_(Token::Caret),
        "#" => |handle: LexerHandle| handle.return_(Token::Hash),
        "==" => |handle: LexerHandle| handle.return_(Token::EqEq),
        "~=" => |handle: LexerHandle| handle.return_(Token::TildeEq),
        "<=" => |handle: LexerHandle| handle.return_(Token::LtEq),
        ">=" => |handle: LexerHandle| handle.return_(Token::GtEq),
        "<" => |handle: LexerHandle| handle.return_(Token::Lt),
        ">" => |handle: LexerHandle| handle.return_(Token::Gt),
        "=" => |handle: LexerHandle| handle.return_(Token::Eq),
        "(" => |handle: LexerHandle| handle.return_(Token::LParen),
        ")" => |handle: LexerHandle| handle.return_(Token::RParen),
        "{" => |handle: LexerHandle| handle.return_(Token::LBrace),
        "}" => |handle: LexerHandle| handle.return_(Token::RBrace),
        "[" => |handle: LexerHandle| handle.return_(Token::LBracket),
        "]" => |handle: LexerHandle| handle.return_(Token::RBracket),
        ";" => |handle: LexerHandle| handle.return_(Token::Semicolon),
        ":" => |handle: LexerHandle| handle.return_(Token::Colon),
        "," => |handle: LexerHandle| handle.return_(Token::Comma),
        "." => |handle: LexerHandle| handle.return_(Token::Dot),
        ".." => |handle: LexerHandle| handle.return_(Token::DotDot),
        "..." => |handle: LexerHandle| handle.return_(Token::DotDotDot),
        "and" => |handle: LexerHandle| handle.return_(Token::Keyword(Keyword::And)),
        "break" => |handle: LexerHandle| handle.return_(Token::Keyword(Keyword::Break)),
        "do" => |handle: LexerHandle| handle.return_(Token::Keyword(Keyword::Do)),
        "else" => |handle: LexerHandle| handle.return_(Token::Keyword(Keyword::Else)),
        "elseif" => |handle: LexerHandle| handle.return_(Token::Keyword(Keyword::ElseIf)),
        "end" => |handle: LexerHandle| handle.return_(Token::Keyword(Keyword::End)),
        "false" => |handle: LexerHandle| handle.return_(Token::Keyword(Keyword::False)),
        "for" => |handle: LexerHandle| handle.return_(Token::Keyword(Keyword::For)),
        "function" => |handle: LexerHandle| handle.return_(Token::Keyword(Keyword::Function)),
        "if" => |handle: LexerHandle| handle.return_(Token::Keyword(Keyword::If)),
        "in" => |handle: LexerHandle| handle.return_(Token::Keyword(Keyword::In)),
        "local" => |handle: LexerHandle| handle.return_(Token::Keyword(Keyword::Local)),
        "nil" => |handle: LexerHandle| handle.return_(Token::Keyword(Keyword::Nil)),
        "not" => |handle: LexerHandle| handle.return_(Token::Keyword(Keyword::Not)),
        "or" => |handle: LexerHandle| handle.return_(Token::Keyword(Keyword::Or)),
        "repeat" => |handle: LexerHandle| handle.return_(Token::Keyword(Keyword::Repeat)),
        "return" => |handle: LexerHandle| handle.return_(Token::Keyword(Keyword::Return)),
        "then" => |handle: LexerHandle| handle.return_(Token::Keyword(Keyword::Then)),
        "true" => |handle: LexerHandle| handle.return_(Token::Keyword(Keyword::True)),
        "until" => |handle: LexerHandle| handle.return_(Token::Keyword(Keyword::Until)),
        "while" => |handle: LexerHandle| handle.return_(Token::Keyword(Keyword::While)),

        '"' => |mut handle: LexerHandle| {
            handle.state().short_string_delim = Quote::Double;
            handle.state().string_buf.clear();
            handle.switch(LexerRules::String)
        },

        '\'' => |mut handle: LexerHandle| {
            handle.state().short_string_delim = Quote::Single;
            handle.state().string_buf.clear();
            handle.switch(LexerRules::String)
        },

        $var_init $var_subseq+ => |handle: LexerHandle| {
            let match_ = handle.match_().to_owned();
            handle.return_(Token::Var(match_))
        },
    },

    rule String {
        '"' => |mut handle: LexerHandle| {
            if handle.state().short_string_delim == Quote::Double {
                let str = handle.state().string_buf.clone();
                handle.switch_and_return(LexerRules::Init, Token::String(str))
            } else {
                handle.state().string_buf.push('"');
                handle.continue_()
            }
        },

        "'" => |mut handle: LexerHandle| {
            if handle.state().short_string_delim == Quote::Single {
                let str = handle.state().string_buf.clone();
                handle.switch_and_return(LexerRules::Init, Token::String(str))
            } else {
                handle.state().string_buf.push('\'');
                handle.continue_()
            }
        },

        "\\a" => |mut handle: LexerHandle| {
            handle.state().string_buf.push('\u{7}');
            handle.continue_()
        },

        "\\b" => |mut handle: LexerHandle| {
            handle.state().string_buf.push('\u{8}');
            handle.continue_()
        },

        "\\f" => |mut handle: LexerHandle| {
            handle.state().string_buf.push('\u{c}');
            handle.continue_()
        },

        "\\n" => |mut handle: LexerHandle| {
            handle.state().string_buf.push('\n');
            handle.continue_()
        },

        "\\r" => |mut handle: LexerHandle| {
            handle.state().string_buf.push('\r');
            handle.continue_()
        },

        "\\t" => |mut handle: LexerHandle| {
            handle.state().string_buf.push('\t');
            handle.continue_()
        },

        "\\v" => |mut handle: LexerHandle| {
            handle.state().string_buf.push('\u{b}');
            handle.continue_()
        },

        "\\\\" => |mut handle: LexerHandle| {
            handle.state().string_buf.push('\\');
            handle.continue_()
        },

        "\\\"" => |mut handle: LexerHandle| {
            handle.state().string_buf.push('"');
            handle.continue_()
        },

        "\\'" => |mut handle: LexerHandle| {
            handle.state().string_buf.push('\'');
            handle.continue_()
        },

        "\\\n" => |mut handle: LexerHandle| {
            handle.state().string_buf.push('\n');
            handle.continue_()
        },

        _ => |mut handle: LexerHandle| {
            let char = handle.match_().chars().next_back().unwrap();
            handle.state().string_buf.push(char);
            handle.continue_()
        },
    },
}

fn ignore_pos<A, E>(ret: Option<Result<(usize, A, usize), E>>) -> Option<Result<A, E>> {
    ret.map(|res| res.map(|(_, a, _)| a))
}

#[test]
fn parse_lua_string() {
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

#[test]
fn parse_lua_var() {
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

#[test]
fn parse_lua_simple() {
    let lexer = Lexer::new(
        "+ - * / % ^ # == ~= <= >= < > = ( ) { } [ ] \
         ; : , . .. ... and break do else elseif end \
         false for function if in local nil not or repeat \
         return then true until while",
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
        ]
    );
}
