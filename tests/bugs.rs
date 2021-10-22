mod test_utils;

use lexgen::lexer;
use test_utils::next;

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

        '"' => |lexer| {
            println!("matched a double quote");
            let str = std::mem::replace(&mut lexer.state().buf, String::new());
            lexer.return_(str)
        },

        "\\\"" => |lexer| {
            println!("matched an escaped double quote");
            lexer.state().buf.push('"');
            lexer.continue_()
        },

        _ => |lexer| {
            let char = lexer.match_().chars().next_back().unwrap();
            println!("wildcard matched {:?}", char);
            lexer.state().buf.push(char);
            lexer.continue_()
        },
    }

    let mut lexer = Lexer::new("test\"");
    assert_eq!(next(&mut lexer), Some(Ok("test".to_owned())));
    assert_eq!(next(&mut lexer), None);

    let mut lexer = Lexer::new("\\\"\"");
    assert_eq!(next(&mut lexer), Some(Ok("\"".to_owned())));
    assert_eq!(next(&mut lexer), None);
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

            "(*" => |lexer| {
                lexer.state().comment_depth = 1;
                lexer.switch(LexerRule::Comment)
            },
        }

        rule Comment {
            "(*" => |lexer| {
                let depth = &mut lexer.state().comment_depth;
                *depth =  *depth + 1;
                lexer.continue_()
            },

            "*)" => |lexer| {
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
fn failure_confusion_3_1() {
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
fn failure_confusion_3_2() {
    // In practice the case we test in the previous test happens when lexing single-letter
    // identifiers in a lexer that allows multi-letter identifiers (i.e. practically all language
    // lexers). Here's a more realistic example:
    lexer! {
        Lexer -> usize;

        $$ascii_lowercase+ = 1,
        ',' = 2,
    }

    let mut lexer = Lexer::new("f,");
    assert_eq!(lexer.next(), Some(Ok((0, 1, 1))));
    assert_eq!(lexer.next(), Some(Ok((1, 2, 2))));
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

#[test]
fn return_should_reset_match() {
    lexer! {
        Lexer -> &'input str;

        rule Init {
            "aaa" => |lexer| {
                let match_ = lexer.match_();
                lexer.switch_and_return(LexerRule::State1, match_)
            },
        }

        rule State1 {
            "bbb" => |lexer| {
                let match_ = lexer.match_();
                lexer.switch_and_return(LexerRule::Init, match_)
            },
        }
    }

    let mut lexer = Lexer::new("aaabbb");
    assert_eq!(next(&mut lexer), Some(Ok("aaa")));
    assert_eq!(next(&mut lexer), Some(Ok("bbb")));
    assert_eq!(next(&mut lexer), None);
}

#[test]
fn issue_16_backtracking_1() {
    fn return_match<'lexer, 'input>(lexer: &mut Lexer<'input>) -> LexerAction<&'input str> {
        let match_ = lexer.match_();
        lexer.return_(match_)
    }

    lexer! {
        Lexer -> &'input str;

        'a'+ 'b' => return_match,
        'a' => return_match,
    }

    let mut lexer = Lexer::new("aaaab");
    assert_eq!(next(&mut lexer), Some(Ok("aaaab")));
    assert_eq!(next(&mut lexer), None);

    let mut lexer = Lexer::new("aaaa");
    assert_eq!(next(&mut lexer), Some(Ok("a")));
    assert_eq!(next(&mut lexer), Some(Ok("a")));
    assert_eq!(next(&mut lexer), Some(Ok("a")));
    assert_eq!(next(&mut lexer), Some(Ok("a")));
    assert_eq!(next(&mut lexer), None);
}

#[test]
fn issue_16_backtracking_2() {
    fn return_match<'lexer, 'input>(lexer: &mut Lexer<'input>) -> LexerAction<&'input str> {
        let match_ = lexer.match_();
        lexer.return_(match_)
    }

    lexer! {
        Lexer -> &'input str;

        "xyzxyz" => return_match,
        "xyz" => return_match,
        "xya" => return_match,
    }

    let mut lexer = Lexer::new("xyzxya");
    assert_eq!(next(&mut lexer), Some(Ok("xyz")));
    assert_eq!(next(&mut lexer), Some(Ok("xya")));
    assert_eq!(next(&mut lexer), None);
}

#[test]
fn end_of_input_handling() {
    lexer! {
        Lexer -> (usize, &'input str);

        rule Init {
            'a' => |lexer| {
                let match_ = lexer.match_();
                lexer.switch_and_return(LexerRule::Rule1, (0, match_))
            },
        }

        rule Rule1 {
            $,

            'a' => |lexer| {
                let match_ = lexer.match_();
                lexer.return_((1, match_))
            },
        }
    }

    let mut lexer = Lexer::new("aa");
    assert_eq!(lexer.next(), Some(Ok((0, (0, "a"), 1))));
    assert_eq!(lexer.next(), Some(Ok((1, (1, "a"), 2))));
    assert_eq!(lexer.next(), None);
}
