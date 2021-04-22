use lexer_gen::lexer_gen;

#[test]
fn wildcard_confusion() {
    // The bug: in the lexer below, when the input is "\\\"", the first backslash would be pushed
    // to the string buffer by the catch-all case. The correct behaviour is the catch-all case
    // should only run if the next character is not '"' or '\\', as those cases are handled by the
    // rules before it.

    #[derive(Debug, Default)]
    struct LexerState {
        buf: String,
    }

    lexer_gen! {
        Lexer(LexerState) -> String;

        let whitespace = [' ' '\t' '\n'];

        rule Init {
            '"' => |mut handle: LexerHandle| {
                println!("matched a double quote");
                let str = std::mem::replace(&mut handle.state().buf, String::new());
                handle.return_(str)
            },

            "\\\"" => |mut handle: LexerHandle| {
                println!("matched an escaped double quote");
                handle.state().buf.push('"');
                handle.continue_()
            },

            _ => |mut handle: LexerHandle| {
                let char = handle.match_().chars().next_back().unwrap();
                println!("wildcard matched {:?}", char);
                handle.state().buf.push(char);
                handle.continue_()
            },
        }
    }

    let mut lexer = Lexer::new("test\"", Default::default());
    assert_eq!(ignore_pos(lexer.next()), Some(Ok("test".to_owned())));
    assert_eq!(ignore_pos(lexer.next()), None);

    let mut lexer = Lexer::new("\\\"\"", Default::default());
    assert_eq!(ignore_pos(lexer.next()), Some(Ok("\"".to_owned())));
    assert_eq!(ignore_pos(lexer.next()), None);
}

fn ignore_pos<A, E>(ret: Option<Result<(usize, A, usize), E>>) -> Option<Result<A, E>> {
    ret.map(|res| res.map(|(_, a, _)| a))
}
