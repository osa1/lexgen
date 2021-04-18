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

        $init $subseq* => |str: &str| Token::Id(str.to_owned()),
    }

    let mut lexer: Lexer<'static> = Lexer::new("aA123_123-123");
    assert_eq!(
        lexer.next(),
        Some(Ok((0, Token::Id("aA123_123-123".to_owned()), 13)))
    );
}
