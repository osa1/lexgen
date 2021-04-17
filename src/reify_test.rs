use std::str::CharIndices;

struct Lexer<'input, A> {
    state: u32,
    input: &'input str,
    iter: CharIndices<'input>,
    match_stack: Vec<(usize, usize, A)>,
    current_match_start: usize,
    current_match_end: usize,
}

impl<'input, A> Lexer<'input, A> {
    fn new(input: &'input str) -> Self {
        Lexer {
            state: 0,
            input,
            iter: input.char_indices(),
            match_stack: vec![],
            current_match_start: 0,
            current_match_end: 0,
        }
    }

    fn pop_match_or_fail(&mut self) -> Option<Result<(usize, A, usize), LexerError>> {
        match self.match_stack.pop() {
            Some((match_begin, match_end, a)) => {
                self.match_stack.clear();
                Some(Ok((match_begin, a, match_end)))
            }
            None => Some(Err(LexerError {
                char_idx: self.current_match_start,
            })),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct LexerError {
    pub char_idx: usize,
}

impl<'input> Iterator for Lexer<'input, usize> {
    type Item = Result<(usize, usize, usize), LexerError>;

    fn next(&mut self) -> Option<Result<(usize, usize, usize), LexerError>> {
        loop {
            println!("self.state={}", self.state);

            match self.state {
                0 => {
                    // Initial state
                    match self.iter.next() {
                        None if self.match_stack.is_empty() => return None,
                        None => return self.pop_match_or_fail(),
                        Some((char_idx, char)) => {
                            self.current_match_start = char_idx;
                            self.current_match_end = char_idx + char.len_utf8();
                            match char {
                                'a' => self.state = 1,
                                'b' => self.state = 2,
                                _ => return self.pop_match_or_fail(),
                            }
                        }
                    }
                }
                1 => {
                    // Accepting state for 'a'
                    self.match_stack
                        .push((self.current_match_start, self.current_match_end, 1));
                    // No outgoing edges, return to initial state
                    self.state = 0;
                }
                2 => {
                    // Accepting state for 'b'
                    self.match_stack
                        .push((self.current_match_start, self.current_match_end, 2));
                    // No outgoing edges, return to initial state
                    self.state = 0;
                }
                _ => panic!(),
            }
        }
    }
}

#[test]
fn test_reify() {
    let mut lexer: Lexer<'static, usize> = Lexer::new("a");
    assert_eq!(lexer.next(), Some(Ok((0, 1, 1))));
    assert_eq!(lexer.next(), None);
}
