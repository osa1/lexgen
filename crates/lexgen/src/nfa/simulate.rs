use super::{AcceptingState, StateIdx, NFA};
use crate::collections::Set;
use crate::dfa::simulate::simulate_right_ctx;
use crate::dfa::StateIdx as DfaStateIdx;
use crate::right_ctx::RightCtxDFAs;

pub type Matches<'input, A> = Vec<(&'input str, A)>;

pub type ErrorLoc = usize;

impl<A: std::fmt::Debug + Copy> NFA<A> {
    pub fn simulate<'input>(
        &self,
        input: &'input str,
        right_ctx_dfas: &RightCtxDFAs<DfaStateIdx>,
    ) -> (Matches<'input, A>, Option<ErrorLoc>) {
        let mut values: Matches<'input, A> = vec![];

        // If we skipped an accepting state because we were able to make progress with the next
        // character, this state holds the previous match. If we get stuck we return this match.
        //
        // This implements backtracking in regexes like:
        //
        // - aaaaaab
        // - a
        //
        // in an input like "aaaa".
        let mut last_match: Option<(usize, A, usize)> = None;

        let mut states: Set<StateIdx> = Default::default();
        states.insert(StateIdx(0));
        states = self.compute_state_closure(&states);

        let mut char_indices = input.char_indices();

        // Where the current match starts
        let mut match_start: usize = 0;

        // Index of current character in input string
        let mut char_idx;

        'outer: loop {
            while let Some((char_idx_, char)) = char_indices.next() {
                char_idx = match_start + char_idx_;

                states = next(self, &states, char);

                // When stuck check if we skipped an accepting state
                if states.is_empty() {
                    match last_match.take() {
                        None => {
                            // We're stuck and can't backtrack, raise an error
                            return (values, Some(match_start));
                        }
                        Some((last_match_start, last_match_value, last_match_end)) => {
                            // Backtrack to the previous accepting state
                            match_start = last_match_end;
                            char_indices = input[match_start..].char_indices();

                            // Accept the previous match
                            values
                                .push((&input[last_match_start..last_match_end], last_match_value));

                            // Restart state machine
                            states.insert(StateIdx(0));
                            states = self.compute_state_closure(&states);
                        }
                    }
                } else {
                    // Check for accepting states. Sort states to pick the one that comes first in
                    // the program.
                    let mut states_sorted: Vec<StateIdx> = states.iter().copied().collect();
                    states_sorted.sort();
                    for state in states_sorted {
                        if let Some(AcceptingState { value, right_ctx }) =
                            &self.states[state.0].accepting
                        {
                            match right_ctx {
                                None => {
                                    last_match =
                                        Some((match_start, *value, char_idx + char.len_utf8()));
                                    break;
                                }
                                Some(right_ctx_idx) => {
                                    let right_ctx_dfa = right_ctx_dfas.get(right_ctx_idx);
                                    if simulate_right_ctx(right_ctx_dfa, char_indices.clone()) {
                                        last_match =
                                            Some((match_start, *value, char_idx + char.len_utf8()));
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Reached EOF, take EOF transitions, check for accepting states
            states = next_end_of_input(self, &states);

            {
                let mut states_sorted: Vec<StateIdx> = states.iter().copied().collect();
                states_sorted.sort();

                for state in states_sorted {
                    if let Some(AcceptingState { value, right_ctx }) =
                        &self.states[state.0].accepting
                    {
                        match right_ctx {
                            None => {
                                values.push((&input[match_start..], *value));
                                break 'outer;
                            }
                            Some(right_ctx_idx) => {
                                let right_ctx_dfa = right_ctx_dfas.get(right_ctx_idx);
                                if simulate_right_ctx(right_ctx_dfa, char_indices.clone()) {
                                    values.push((&input[match_start..], *value));
                                    break 'outer;
                                }
                            }
                        }
                    }
                }
            }

            // Reached EOF but cannot accept input, backtrack if possible, otherwise raise an error
            match last_match.take() {
                Some((last_match_start, last_match_value, last_match_end)) => {
                    values.push((&input[last_match_start..last_match_end], last_match_value));

                    if last_match_end == input.len() {
                        break 'outer;
                    } else {
                        // Backtrack
                        match_start = last_match_end;
                        char_indices = input[match_start..].char_indices();

                        // Restart state machine
                        states.insert(StateIdx(0));
                        states = self.compute_state_closure(&states);
                    }
                }
                None => {
                    // We're stuck and can't backtrack, raise an error
                    return (values, Some(match_start));
                }
            }
        }

        (values, None)
    }
}

fn next<A>(nfa: &NFA<A>, states: &Set<StateIdx>, char: char) -> Set<StateIdx> {
    let mut next_states: Set<StateIdx> = Default::default();

    for state in states {
        // Range transitions
        for range in nfa.states[state.0].range_transitions.iter() {
            if char >= range.start && char <= range.end {
                next_states.extend(range.value.clone());
            }
        }

        // Any transitions
        next_states.extend(nfa.states[state.0].any_transitions.iter().copied());
    }

    nfa.compute_state_closure(&next_states)
}

fn next_end_of_input<A>(nfa: &NFA<A>, states: &Set<StateIdx>) -> Set<StateIdx> {
    let mut next_states: Set<StateIdx> = Default::default();

    for state in states {
        next_states.extend(nfa.states[state.0].end_of_input_transitions.iter().copied());
    }

    nfa.compute_state_closure(&next_states)
}
