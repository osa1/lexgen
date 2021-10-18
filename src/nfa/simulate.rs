use super::{StateIdx, NFA};

use fxhash::FxHashSet as Set;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Value<'input, A> {
    pub value: A,
    pub matched_str: &'input str,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Error {
    pub loc: usize,
}

#[derive(Debug)]
struct Match<A> {
    value: A,
    match_start: usize,
    match_end: usize,
}

impl<A: std::fmt::Debug + Copy> NFA<A> {
    pub fn simulate_2<'input>(&self, input: &'input str) -> (Vec<Value<'input, A>>, Option<Error>) {
        let mut values: Vec<Value<'input, A>> = vec![];

        // If we skipped an accepting state because we were able to make progress with the next
        // character, this state holds the previous match. If we get stuck we return this match.
        //
        // This implements backtracking in regexes like:
        //
        // - aaaaaab
        // - a
        //
        // in an input like "aaaa".
        let mut last_match: Option<Match<A>> = None;

        let mut states: Set<StateIdx> = Default::default();
        states.insert(StateIdx(0));
        states = self.compute_state_closure(&states);

        let mut char_indices = input.char_indices();

        // Where the current match starts
        let mut match_start: usize = 0;

        // Index of current character in input string
        let mut char_idx: usize = 0;

        'outer: loop {
            while let Some((char_idx_, char)) = char_indices.next() {
                char_idx = match_start + char_idx_;

                states = next(self, &states, char);

                // When stuck check if we skipped an accepting state
                if states.is_empty() {
                    match last_match.take() {
                        None => {
                            // We're stuck and can't backtrack, raise an error
                            return (values, Some(Error { loc: char_idx }));
                        }
                        Some(last_match) => {
                            // Backtrack to the previous accepting state
                            match_start = last_match.match_end;
                            char_indices = input[match_start..].char_indices();

                            // Accept the previous match
                            values.push(Value {
                                value: last_match.value,
                                matched_str: &input[last_match.match_start..last_match.match_end],
                            });

                            // Restart state machine
                            states.insert(StateIdx(0));
                            states = self.compute_state_closure(&states);
                        }
                    }
                } else {
                    // Check for accepting states
                    for state in &states {
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
                    values.push(Value {
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
                        states.insert(StateIdx(0));
                        states = self.compute_state_closure(&states);
                    }
                }
                None => {
                    // We're stuck and can't backtrack, raise an error
                    return (values, Some(Error { loc: char_idx }));
                }
            }
        }

        (values, None)
    }
}

fn next<A>(nfa: &NFA<A>, states: &Set<StateIdx>, char: char) -> Set<StateIdx> {
    let mut next_states: Set<StateIdx> = Default::default();

    for state in states {
        // Char transitions
        if let Some(char_nexts) = nfa.states[state.0].char_transitions.get(&char) {
            next_states.extend(char_nexts.into_iter());
            for (range, range_nexts) in &nfa.states[state.0].range_transitions {
                if char >= range.0 && char <= range.1 {
                    next_states.extend(range_nexts.into_iter());
                }
            }
        }

        // Range transitions
        for ((range_begin, range_end), nexts) in &nfa.states[state.0].range_transitions {
            if char >= *range_begin && char <= *range_end {
                next_states.extend(nexts.into_iter());
            }
        }
    }

    nfa.compute_state_closure(&next_states)
}

#[test]
fn simulate_backtracking() {
    use crate::ast::Regex;

    let mut nfa: NFA<usize> = NFA::new();

    nfa.add_regex(
        &Default::default(),
        &Regex::Concat(
            Box::new(Regex::OneOrMore(Box::new(Regex::Char('a')))),
            Box::new(Regex::Char('b')),
        ),
        1,
    );

    nfa.add_regex(&Default::default(), &Regex::Char('a'), 2);

    println!("NFA=\n{}", nfa);

    assert_eq!(
        nfa.simulate_2("a"),
        (
            vec![Value {
                value: 2,
                matched_str: "a"
            }],
            None
        )
    );

    assert_eq!(
        nfa.simulate_2("aa"),
        (
            vec![
                Value {
                    value: 2,
                    matched_str: "a",
                },
                Value {
                    value: 2,
                    matched_str: "a",
                },
            ],
            None
        )
    );

    assert_eq!(
        nfa.simulate_2("aab"),
        (
            vec![Value {
                value: 1,
                matched_str: "aab",
            }],
            None
        )
    );
}

#[test]
fn issue_16() {
    use crate::ast::Regex;

    let mut nfa: NFA<usize> = NFA::new();

    nfa.add_regex(&Default::default(), &Regex::String("xyzxyz".to_owned()), 1);
    nfa.add_regex(&Default::default(), &Regex::String("xyz".to_owned()), 2);
    nfa.add_regex(&Default::default(), &Regex::String("xya".to_owned()), 3);

    assert_eq!(
        nfa.simulate_2("xyzxya"),
        (
            vec![
                Value {
                    value: 2,
                    matched_str: "xyz"
                },
                Value {
                    value: 3,
                    matched_str: "xya",
                },
            ],
            None
        )
    );

    assert_eq!(
        nfa.simulate_2("xyzxyz"),
        (
            vec![Value {
                value: 1,
                matched_str: "xyzxyz"
            }],
            None
        )
    );
}

#[test]
fn stuck_1() {
    let nfa: NFA<usize> = NFA::new();
    assert_eq!(nfa.simulate_2("a"), (vec![], Some(Error { loc: 0 })));
}

#[test]
fn stuck_2() {
    use crate::ast::Regex;

    let mut nfa: NFA<usize> = NFA::new();

    nfa.add_regex(&Default::default(), &Regex::String("ab".to_owned()), 1);

    println!("NFA=\n{}", nfa);

    assert_eq!(
        nfa.simulate_2("aba"),
        (
            vec![Value {
                value: 1,
                matched_str: "ab"
            },],
            Some(Error { loc: 2 })
        )
    );
}

#[test]
fn stuck_3() {
    use crate::ast::Regex;

    let mut nfa: NFA<usize> = NFA::new();

    nfa.add_regex(&Default::default(), &Regex::String("aaab".to_owned()), 1);
    nfa.add_regex(&Default::default(), &Regex::String("a".to_owned()), 2);

    println!("NFA=\n{}", nfa);

    assert_eq!(
        nfa.simulate_2("aaabb"),
        (
            vec![Value {
                value: 1,
                matched_str: "aaab"
            }],
            Some(Error { loc: 4 })
        )
    );
}
