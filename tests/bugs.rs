use lexgen::lexer;

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

    lexer! {
        Lexer(LexerState) -> String;

        let whitespace = [' ' '\t' '\n'];

        '"' => |mut lexer| {
            println!("matched a double quote");
            let str = std::mem::replace(&mut lexer.state().buf, String::new());
            lexer.return_(str)
        },

        "\\\"" => |mut lexer| {
            println!("matched an escaped double quote");
            lexer.state().buf.push('"');
            lexer.continue_()
        },

        _ => |mut lexer| {
            let char = lexer.match_().chars().next_back().unwrap();
            println!("wildcard matched {:?}", char);
            lexer.state().buf.push(char);
            lexer.continue_()
        },
    }

    let mut lexer = Lexer::new("test\"");
    assert_eq!(ignore_pos(lexer.next()), Some(Ok("test".to_owned())));
    assert_eq!(ignore_pos(lexer.next()), None);

    let mut lexer = Lexer::new("\\\"\"");
    assert_eq!(ignore_pos(lexer.next()), Some(Ok("\"".to_owned())));
    assert_eq!(ignore_pos(lexer.next()), None);
}

fn ignore_pos<A, E>(ret: Option<Result<(usize, A, usize), E>>) -> Option<Result<A, E>> {
    ret.map(|res| res.map(|(_, a, _)| a))
}
