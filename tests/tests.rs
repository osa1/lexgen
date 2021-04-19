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

        // Whitespace
        [' ' '\t' '\n']+,

        $init $subseq* => |str: &str| LexerAction::Return(Token::Id(str.to_owned())),

        rule Blah {
            $init => |str: &str| todo!(),
        }
    }

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
