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

#[derive(Debug, Default, Clone, Copy)]
struct LexerState {
    /// Number of opening `=`s seen when parsing a long string
    lon_string_opening_eqs: usize,
    /// Number of closing `=`s seen when parsing a long string
    long_string_closing_eqs: usize,
}

lexer_gen! {
    Lexer(LexerState) -> Token;

    let whitespace = [' ' '\t' '\n'];

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
    },
}

fn next(lexer: &mut Lexer) -> Option<Result<Token, LexerError>> {
    lexer.next().map(|r| r.map(|(_, t, _)| t))
}

#[test]
fn parse_lua() {
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
