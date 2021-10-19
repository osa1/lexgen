use super::{State, StateIdx, DFA};
use crate::ast::RuleRhs;
use crate::semantic_action_table::{SemanticActionIdx, SemanticActionTable};

use fxhash::FxHashMap;

#[derive(Debug)]
pub enum Trans {
    Accept(RuleRhs<SemanticActionIdx>),
    Trans(StateIdx),
}

/// Removes accepting states with no transitions, makes the transitions to those states accepting.
pub fn simplify<K>(
    dfa: DFA<StateIdx, RuleRhs<SemanticActionIdx>>,
    dfa_state_indices: &mut FxHashMap<K, StateIdx>,
    semantic_action_table: &mut SemanticActionTable,
) -> DFA<Trans, RuleRhs<SemanticActionIdx>> {
    let mut empty_states: Vec<(StateIdx, Option<RuleRhs<SemanticActionIdx>>)> = vec![];
    let mut non_empty_states: Vec<(StateIdx, State<StateIdx, RuleRhs<SemanticActionIdx>>)> = vec![];

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

    let mut map_transition = |t: StateIdx| -> Option<Trans> {
        match empty_states.binary_search_by(|(state_idx, _action)| state_idx.cmp(&t)) {
            Ok(idx) => empty_states[idx].1.clone().map(|rhs| match rhs {
                RuleRhs::None => Trans::Accept(RuleRhs::None),
                RuleRhs::Rhs { expr, kind } => {
                    semantic_action_table.record_use(expr);
                    Trans::Accept(RuleRhs::Rhs { expr, kind })
                }
            }),
            Err(idx) => Some(Trans::Trans(t.map(|i| i - idx))),
        }
    };

    let new_states: Vec<State<Trans, RuleRhs<SemanticActionIdx>>> = non_empty_states
        .into_iter()
        .map(|(_state_idx, state)| {
            let State {
                initial,
                char_transitions,
                range_transitions,
                fail_transition,
                accepting,
                predecessors,
            } = state;

            let char_transitions = char_transitions
                .into_iter()
                .filter_map(|(char, next)| map_transition(next).map(|next| (char, next)))
                .collect();

            let range_transitions = range_transitions.filter_map(&mut map_transition);

            let fail_transition = fail_transition.and_then(&mut map_transition);

            let predecessors = predecessors
                .into_iter()
                .map(|pred| match map_transition(pred) {
                    Some(Trans::Trans(pred)) => pred,
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
                fail_transition,
                accepting,
                predecessors,
            }
        })
        .collect();

    DFA::from_states(new_states)
}
