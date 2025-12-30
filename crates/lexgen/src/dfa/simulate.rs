use super::{StateIdx, DFA};

pub use crate::nfa::simulate::{ErrorLoc, Matches};
use crate::nfa::AcceptingState;
use crate::range_map::Range;
use crate::right_ctx::RightCtxDFAs;

impl<A: Copy> DFA<StateIdx, A> {
    pub fn simulate<'input>(
        &self,
        input: &'input str,
        right_ctx_dfas: &RightCtxDFAs<StateIdx>,
    ) -> (Matches<'input, A>, Option<ErrorLoc>) {
        let mut values: Matches<'input, A> = vec![];

        // Current state
        let mut state = StateIdx(0);

        // See comments for the same variable in NFA simulation
        let mut last_match: Option<(usize, A, usize)> = None;

        let mut char_indices = input.char_indices();

        // Where the current match starts
        let mut match_start = 0;

        // Index of current character in input string
        let mut char_idx: usize;

        'outer: loop {
            while let Some((char_idx_, char)) = char_indices.next() {
                char_idx = match_start + char_idx_;

                match next(self, state, char) {
                    None => {
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
                                values.push((
                                    &input[last_match_start..last_match_end],
                                    last_match_value,
                                ));

                                // Restart state machine
                                state = StateIdx(0);
                            }
                        }
                    }
                    Some(next_state) => {
                        state = next_state;

                        // Check for accepting state
                        for AcceptingState { value, right_ctx } in &self.states[state.0].accepting {
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

            // Reached EOF, take EOF transition, check for accepting states
            if let Some(next) = next_end_of_input(self, state) {
                // Check for accepting state
                state = next;
                for AcceptingState { value, right_ctx } in &self.states[state.0].accepting {
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
                        state = StateIdx(0);
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

fn next<A>(dfa: &DFA<StateIdx, A>, state: StateIdx, char: char) -> Option<StateIdx> {
    let state = &dfa.states[state.0];

    for range in state.range_transitions.iter() {
        let Range { start, end, value } = range;
        if char >= *start && char <= *end {
            return Some(*value);
        }
    }

    if let Some(next) = state.any_transition {
        return Some(next);
    }

    None
}

fn next_end_of_input<A>(dfa: &DFA<StateIdx, A>, state: StateIdx) -> Option<StateIdx> {
    dfa.states[state.0].end_of_input_transition
}

// Similar to `simulate`, but does not keep track of the last match as we don't need "longest
// match" semantics and backtracking
pub fn simulate_right_ctx(dfa: &DFA<StateIdx, ()>, char_indices: std::str::CharIndices) -> bool {
    let mut state = dfa.initial_state();

    if dfa.is_accepting_state(state) {
        return true;
    }

    for (_, char) in char_indices {
        match next(dfa, state, char) {
            None => {
                // Stuck
                return false;
            }
            Some(next_state) => {
                if dfa.is_accepting_state(next_state) {
                    return true;
                }

                state = next_state;
            }
        }
    }

    match next_end_of_input(dfa, state) {
        None => false,
        Some(next_state) => dfa.is_accepting_state(next_state),
    }
}
