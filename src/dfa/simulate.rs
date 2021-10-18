use super::{StateIdx, DFA};

use crate::range_map::Range;

#[derive(Debug, PartialEq, Eq)]
pub enum Value<'input, A> {
    Value { value: A, matched_str: &'input str },
    Error { loc: usize },
}

#[derive(Debug)]
struct Match<A> {
    value: A,
    match_start: usize,
    match_end: usize,
}

impl<A: Copy> DFA<StateIdx, A> {
    // TODO: Return (Vec<Value>, Option<Error>)
    pub fn simulate_2<'input>(&self, input: &'input str) -> Vec<Value<'input, A>> {
        let mut values: Vec<Value<'input, A>> = vec![];

        // Current state
        let mut state = StateIdx(0);

        // See comments for the same variable in NFA simulation
        let mut last_match: Option<Match<A>> = None;

        let mut char_indices = input.char_indices();

        // Where the current macth starts
        let mut match_start = 0;

        // Index of current character in input string
        let mut char_idx: usize = 0;

        'outer: loop {
            while let Some((char_idx_, char)) = char_indices.next() {
                char_idx = match_start + char_idx_;

                match next(self, state, char) {
                    None => {
                        match last_match.take() {
                            None => {
                                // We're stuck and can't backtrack, raise an error
                                values.push(Value::Error { loc: char_idx });
                                return values;
                            }
                            Some(last_match) => {
                                // Backtrack to the previous accepting state
                                match_start = last_match.match_end;
                                char_indices = input[match_start..].char_indices();

                                // Accept the previous match
                                values.push(Value::Value {
                                    value: last_match.value,
                                    matched_str: &input
                                        [last_match.match_start..last_match.match_end],
                                });

                                // Restart state machine
                                state = StateIdx(0);
                            }
                        }
                    }
                    Some(next_state) => {
                        state = next_state;

                        // Check for accepting state
                        if let Some(value) = self.states[state.0].accepting {
                            last_match = Some(Match {
                                value,
                                match_start,
                                match_end: char_idx + char.len_utf8(),
                            });
                        }
                    }
                }
            }

            // Reached EOF without errors, accept current match
            match last_match.take() {
                Some(last_match) => {
                    values.push(Value::Value {
                        value: last_match.value,
                        matched_str: &input[last_match.match_start..last_match.match_end],
                    });

                    if last_match.match_end == input.len() {
                        break 'outer;
                    } else {
                        // Backtrack
                        match_start = last_match.match_end;
                        char_indices = input[match_start..].char_indices();

                        // Restart state machine
                        state = StateIdx(0);
                    }
                }
                None => {
                    // We're stuck and can't backtrack, raise an error
                    values.push(Value::Error { loc: char_idx });
                    return values;
                }
            }
        }

        values
    }
}

fn next<A>(dfa: &DFA<StateIdx, A>, state: StateIdx, char: char) -> Option<StateIdx> {
    let state = &dfa.states[state.0];

    if let Some(next) = state.char_transitions.get(&char) {
        return Some(*next);
    }

    for range in state.range_transitions.iter() {
        let Range { start, end, values } = range;
        assert_eq!(values.len(), 1);
        let next = values[0];
        if char as u32 >= *start && char as u32 <= *end {
            return Some(next);
        }
    }

    if let Some(next) = state.fail_transition {
        return Some(next);
    }

    return None;
}

#[test]
fn issue_16() {
    use crate::ast::Regex;
    use crate::nfa::NFA;

    let mut nfa: NFA<usize> = NFA::new();

    nfa.add_regex(&Default::default(), &Regex::String("xyzxyz".to_owned()), 1);
    nfa.add_regex(&Default::default(), &Regex::String("xyz".to_owned()), 2);
    nfa.add_regex(&Default::default(), &Regex::String("xya".to_owned()), 3);

    let dfa: DFA<StateIdx, usize> = crate::nfa_to_dfa::nfa_to_dfa(&nfa);

    assert_eq!(
        dfa.simulate_2("xyzxya"),
        vec![
            Value::Value {
                value: 2,
                matched_str: "xyz"
            },
            Value::Value {
                value: 3,
                matched_str: "xya",
            },
        ]
    );

    assert_eq!(
        dfa.simulate_2("xyzxyz"),
        vec![Value::Value {
            value: 1,
            matched_str: "xyzxyz"
        }]
    );
}

#[test]
fn stuck_1() {
    use crate::nfa::NFA;

    let nfa: NFA<usize> = NFA::new();
    let dfa: DFA<StateIdx, usize> = crate::nfa_to_dfa::nfa_to_dfa(&nfa);
    assert_eq!(dfa.simulate_2("a"), vec![Value::Error { loc: 0 }]);
}

#[test]
fn stuck_2() {
    use crate::ast::Regex;
    use crate::nfa::NFA;

    let mut nfa: NFA<usize> = NFA::new();

    nfa.add_regex(&Default::default(), &Regex::String("ab".to_owned()), 1);

    let dfa: DFA<StateIdx, usize> = crate::nfa_to_dfa::nfa_to_dfa(&nfa);

    assert_eq!(
        dfa.simulate_2("aba"),
        vec![
            Value::Value {
                value: 1,
                matched_str: "ab"
            },
            Value::Error { loc: 2 },
        ]
    );
}

#[test]
fn stuck_3() {
    use crate::ast::Regex;
    use crate::nfa::NFA;

    let mut nfa: NFA<usize> = NFA::new();

    nfa.add_regex(&Default::default(), &Regex::String("aaab".to_owned()), 1);
    nfa.add_regex(&Default::default(), &Regex::String("a".to_owned()), 2);

    let dfa: DFA<StateIdx, usize> = crate::nfa_to_dfa::nfa_to_dfa(&nfa);

    assert_eq!(
        dfa.simulate_2("aaabb"),
        vec![
            Value::Value {
                value: 1,
                matched_str: "aaab"
            },
            Value::Error { loc: 4 },
        ]
    );
}
