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
            $init $subseq* => |str: &str| LexerAction::Return(Token::Id(str.to_owned())),
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

#[test]
fn switch() {
    #[derive(Debug, PartialEq, Eq)]
    enum Token {
        Comment,
    }

    lexer_gen! {
        Lexer -> Token;

        rule Init {
            [' ' '\t' '\n']+,
            "/*" => |_: &str| LexerAction::Switch(LexerRules::Comment),
        }

        rule Comment {
            [' ' '\t' '\n']+,
            "*/" => |_: &str| LexerAction::ReturnAndSwitch(Token::Comment, LexerRules::Init),
        }
    }

    let mut lexer = Lexer::new("  /*   */  ");
    assert_eq!(lexer.next(), Some(Ok((2, Token::Comment, 9))));
    assert_eq!(lexer.next(), None);
}
