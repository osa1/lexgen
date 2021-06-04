use super::{State, StateIdx, DFA};

use fxhash::FxHashMap;

#[derive(Debug)]
pub enum Trans<A> {
    Accept(A),
    Trans(StateIdx),
}

/// Removes accepting states with no transitions, makes the transitions to those states accepting.
// TODO: We need to turn RHSs into identifiers by introducing functions for user actions
pub fn simplify<A: Clone, K>(
    dfa: DFA<StateIdx, A>,
    dfa_state_indices: &mut FxHashMap<K, StateIdx>,
) -> DFA<Trans<A>, A> {
    let mut empty_states: Vec<(StateIdx, Option<A>)> = vec![];
    let mut non_empty_states: Vec<(StateIdx, State<StateIdx, A>)> = vec![];

    for (state_idx, state) in dfa.into_state_indices() {
        if state.has_no_transitions() {
            empty_states.push((state_idx, state.accepting));
        } else {
            non_empty_states.push((state_idx, state));
        }
    }

    for (_, t) in dfa_state_indices.iter_mut() {
        let idx = match empty_states.binary_search_by(|(state_idx, _)| state_idx.cmp(&t)) {
            Ok(idx) | Err(idx) => idx,
        };
        *t = t.map(|i| i - idx);
    }

    let map_transition = |t: StateIdx| -> Option<Trans<A>> {
        match empty_states.binary_search_by(|(state_idx, _action)| state_idx.cmp(&t)) {
            Ok(idx) => empty_states[idx].1.clone().map(Trans::Accept),
            Err(idx) => Some(Trans::Trans(t.map(|i| i - idx))),
        }
    };

    let new_states: Vec<State<Trans<A>, A>> = non_empty_states
        .into_iter()
        .map(|(_state_idx, state)| {
            let State {
                initial,
                char_transitions,
                range_transitions,
                fail_transition,
                accepting,
            } = state;

            let char_transitions = char_transitions
                .into_iter()
                .filter_map(|(char, next)| map_transition(next).map(|next| (char, next)))
                .collect();

            let range_transitions = range_transitions.filter_map(map_transition);

            let fail_transition = fail_transition.and_then(map_transition);

            State {
                initial,
                char_transitions,
                range_transitions,
                fail_transition,
                accepting,
            }
        })
        .collect();

    DFA::from_states(new_states)
}
