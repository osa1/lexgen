use super::{StateIdx, NFA};

use fxhash::FxHashSet as Set;

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

impl<A: std::fmt::Debug> NFA<A> {
    pub fn simulate_2<'a, 'input>(&'a self, input: &'input str) -> Vec<Value<'input, &'a A>> {
        let mut values: Vec<Value<'input, &'a A>> = vec![];

        // Match stack. When stuck, we pop the longest match from this & backtrack.
        let mut matches: Vec<Match<&'a A>> = vec![];

        let mut states: Set<StateIdx> = Default::default();
        states.insert(StateIdx(0));
        states = self.compute_state_closure(&states);

        println!("initial states = {:?}", states);

        // Does the NFA accept empty string?
        for state in &states {
            if let Some(value) = self.states[state.0].accepting.as_ref() {
                matches.push(Match {
                    value,
                    match_start: 0,
                    match_end: 0,
                });
            }
        }

        let mut char_indices = input.char_indices().peekable();

        // Where the current match starts
        let mut match_start: usize = 0;

        let mut char_idx: usize;

        'outer: loop {
            while let Some((char_idx_, char)) = char_indices.next() {
                char_idx = match_start + char_idx_;
                println!("char = {:?}, states = {:?}", char, states);

                states = next(self, &states, char);

                println!("next states = {:?}", states);

                // TODO: Handle EOF

                // If we're stuck we need to backtrack with the longest match
                if states.is_empty() {
                    println!("stuck!");
                    match matches.pop() {
                        None => {
                            // We're stuck and can't backtrack, raise an error
                            values.push(Value::Error { loc: char_idx });
                            return values;
                        }
                        Some(longest_match) => {
                            // Backtrack
                            match_start = longest_match.match_end;
                            char_indices = input[match_start..].char_indices().peekable();
                            matches.clear();
                            // Accept the longest match
                            values.push(Value::Value {
                                value: longest_match.value,
                                matched_str: &input
                                    [longest_match.match_start..longest_match.match_end],
                            });
                            // Restart state machine
                            states.insert(StateIdx(0));
                            states = self.compute_state_closure(&states);
                        }
                    }
                } else {
                    // Check for accepting states
                    for state in &states {
                        if let Some(value) = self.states[state.0].accepting.as_ref() {
                            matches.push(Match {
                                value,
                                match_start,
                                match_end: char_idx + char.len_utf8(),
                            });
                        }
                    }
                }
            }

            // Reached EOF without errors, accept longest match
            // TODO: What happens if `matches` is empty?
            if let Some(longest_match) = matches.pop() {
                println!("Accepting longest match");
                values.push(Value::Value {
                    value: longest_match.value,
                    matched_str: &input[longest_match.match_start..longest_match.match_end],
                });

                if longest_match.match_end == input.len() {
                    break 'outer;
                } else {
                    println!("backtrack!");
                    // Backtrack
                    match_start = longest_match.match_end;
                    char_indices = input[match_start..].char_indices().peekable();
                    matches.clear();
                    // Restart state machine
                    states.insert(StateIdx(0));
                    states = self.compute_state_closure(&states);
                }
            }
        }

        values
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
        vec![Value::Value {
            value: &2,
            matched_str: "a"
        }]
    );

    assert_eq!(
        nfa.simulate_2("aa"),
        vec![
            Value::Value {
                value: &2,
                matched_str: "a",
            },
            Value::Value {
                value: &2,
                matched_str: "a",
            },
        ]
    );

    assert_eq!(
        nfa.simulate_2("aab"),
        vec![Value::Value {
            value: &1,
            matched_str: "aab",
        }]
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
        vec![
            Value::Value {
                value: &2,
                matched_str: "xyz"
            },
            Value::Value {
                value: &3,
                matched_str: "xya",
            },
        ]
    );

    assert_eq!(
        nfa.simulate_2("xyzxyz"),
        vec![Value::Value {
            value: &1,
            matched_str: "xyzxyz"
        }]
    );
}
