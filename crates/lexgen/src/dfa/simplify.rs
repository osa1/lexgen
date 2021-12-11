use super::{State, StateIdx, DFA};
use crate::collections::Map;
use crate::nfa::AcceptingState;
use crate::semantic_action_table::SemanticActionIdx;

#[derive(Debug)]
pub enum Trans {
    Accept(Vec<AcceptingState<SemanticActionIdx, StateIdx>>),
    Trans(StateIdx),
}

/// Removes accepting states with no transitions, makes the transitions to those states accepting.
pub fn simplify<K>(
    dfa: DFA<StateIdx, SemanticActionIdx>,
    dfa_state_indices: &mut Map<K, StateIdx>,
) -> DFA<Trans, SemanticActionIdx> {
    let mut empty_states: Vec<(StateIdx, Vec<AcceptingState<SemanticActionIdx, StateIdx>>)> =
        vec![];

    let mut non_empty_states: Vec<(StateIdx, State<StateIdx, SemanticActionIdx>)> = vec![];

    for (state_idx, state) in dfa.into_state_indices() {
        if state.has_no_transitions() && !state.initial {
            empty_states.push((state_idx, state.accepting));
        } else {
            non_empty_states.push((state_idx, state));
        }
    }

    for (_, t) in dfa_state_indices.iter_mut() {
        let idx = match empty_states.binary_search_by(|(state_idx, _)| state_idx.cmp(t)) {
            Ok(idx) | Err(idx) => idx,
        };
        *t = t.map(|i| i - idx);
    }

    let map_transition = |t: StateIdx| -> Trans {
        match empty_states.binary_search_by(|(state_idx, _action)| state_idx.cmp(&t)) {
            Ok(idx) => Trans::Accept(empty_states[idx].1.clone()),
            Err(idx) => Trans::Trans(t.map(|i| i - idx)),
        }
    };

    let new_states: Vec<State<Trans, SemanticActionIdx>> = non_empty_states
        .into_iter()
        .map(|(_state_idx, state)| {
            let State {
                initial,
                char_transitions,
                range_transitions,
                any_transition,
                end_of_input_transition,
                accepting,
                predecessors,
            } = state;

            let char_transitions = char_transitions
                .into_iter()
                .map(|(char, next)| (char, map_transition(next)))
                .collect();

            let range_transitions = range_transitions.map(map_transition);

            let any_transition = any_transition.map(map_transition);

            let end_of_input_transition = end_of_input_transition.map(map_transition);

            let predecessors = predecessors
                .into_iter()
                .map(|pred| match map_transition(pred) {
                    Trans::Trans(pred) => pred,
                    _ => {
                        // This pass should only remove nodes without successors, so it's a bug if
                        // we remove a predecessor
                        panic!("Predecessor of a state is removed in simplification")
                    }
                })
                .collect();

            State {
                initial,
                char_transitions,
                range_transitions,
                any_transition,
                end_of_input_transition,
                accepting,
                predecessors,
            }
        })
        .collect();

    DFA::from_states(new_states)
}
