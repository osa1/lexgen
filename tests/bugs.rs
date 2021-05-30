use lexgen::lexer;

#[test]
fn failure_confusion_1() {
    // The bug: in the lexer below, when the input is "\\\"", the first backslash would be pushed
    // to the string buffer by the catch-all (now called "failure") case. The correct behaviour is
    // the failure case should only run if none of the other rules match to completion.

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

#[test]
fn failure_confusion_2() {
    // Similar to the bug above: the failure case should run if none of the other rules match to
    // completion.

    #[derive(Debug, Default)]
    struct LexerState {
        comment_depth: usize,
    }

    lexer! {
        Lexer(LexerState) -> ();


        rule Init {
            ' ',

            "(*" =>
                |mut lexer| {
                    lexer.state().comment_depth = 1;
                    lexer.switch(LexerRule::Comment)
                },
        }

        rule Comment {
            "(*" =>
                |mut lexer| {
                    let depth = &mut lexer.state().comment_depth;
                    *depth =  *depth + 1;
                    lexer.continue_()
                },

            "*)" =>
                |mut lexer| {
                    let depth = &mut lexer.state().comment_depth;
                    if *depth == 1 {
                        lexer.switch(LexerRule::Init)
                    } else {
                        *depth = *depth - 1;
                        lexer.continue_()
                    }
                },

            _,
        }
    }

    let mut lexer = Lexer::new("(* * *) (* (* ** *) *)");
    assert_eq!(lexer.next(), None);
}

#[test]
fn failure_confusion_3() {
    lexer! {
        Lexer -> usize;

        ' ' = 0,
        "ab" = 1,
        _ = 2,
    }

    let mut lexer = Lexer::new("a ab abc");
    assert_eq!(lexer.next(), Some(Ok((0, 2, 1))));
    assert_eq!(lexer.next(), Some(Ok((1, 0, 2))));
    assert_eq!(lexer.next(), Some(Ok((2, 1, 4))));
    assert_eq!(lexer.next(), Some(Ok((4, 0, 5))));
    assert_eq!(lexer.next(), Some(Ok((5, 1, 7))));
    assert_eq!(lexer.next(), Some(Ok((7, 2, 8))));
    assert_eq!(lexer.next(), None);
}

#[test]
fn failure_confusion_4() {
    lexer! {
        Lexer -> u32;

        ' ',
        "aaa" = 1,
        "aa" = 2,
        _ = 3,
    }

    let mut lexer = Lexer::new("aaa aa a");

    assert_eq!(lexer.next(), Some(Ok((0, 1, 3))));
    assert_eq!(lexer.next(), Some(Ok((4, 2, 6))));
    assert_eq!(lexer.next(), Some(Ok((7, 3, 8))));
    assert_eq!(lexer.next(), None);
}

#[test]
fn continue_confusion_1() {
    lexer! {
        Lexer -> u32;

        _,
    }

    let mut lexer = Lexer::new("");
    assert_eq!(lexer.next(), None);

    let mut lexer = Lexer::new("a");
    assert_eq!(lexer.next(), None);

    let mut lexer = Lexer::new("aaa");
    assert_eq!(lexer.next(), None);
}

#[test]
fn continue_confusion_2() {
    lexer! {
        Lexer -> u32;

        rule Init {
            _ => |lexer| lexer.switch(LexerRule::Test),
        }

        // Previously failure code would run on end-of-stream, which resets the state to `Test` and
        // continues, causing a loop.
        //
        // This issue does not exist in `Init` as we explicitly handle EOF there, to stop the main
        // loop.
        //
        // Instead end-of-stream in a state other than `Init` should fail with "unexpected EOF".
        rule Test {
            _,
        }
    }

    let mut lexer = Lexer::new("a");
    assert!(matches!(lexer.next(), Some(Err(_))));

    let mut lexer = Lexer::new("aa");
    assert!(matches!(lexer.next(), Some(Err(_))));
}

fn ignore_pos<A, E>(ret: Option<Result<(usize, A, usize), E>>) -> Option<Result<A, E>> {
    ret.map(|res| res.map(|(_, a, _)| a))
}
