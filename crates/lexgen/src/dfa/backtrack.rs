use crate::collections::Map;
use crate::dfa::{StateIdx, DFA};

use std::collections::hash_map::Entry;

pub(crate) fn update_backtracks<A>(dfa: &mut DFA<StateIdx, A>) {
    // State and whether the state is an accepting state.
    let mut work_list: Vec<(StateIdx, bool)> = dfa
        .states
        .iter()
        .enumerate()
        .filter_map(|(state_idx, state)| {
            if state.initial {
                Some((StateIdx(state_idx), false))
            } else {
                None
            }
        })
        .collect();

    // Set of visited nodes, with their backtrack state when visited. If a state's backtrack
    // property changes, we visit it again to make its successors backtrack.
    let mut visited: Map<StateIdx, bool> = Default::default();

    while let Some((state, backtrack)) = work_list.pop() {
        // Did we visit the state, with the right backtrack state?
        match visited.entry(state) {
            Entry::Occupied(mut entry) => {
                if *entry.get() == backtrack {
                    continue;
                }
                entry.insert(backtrack);
            }
            Entry::Vacant(entry) => {
                entry.insert(backtrack);
            }
        }

        // Whether the successor states should backtrack.
        let successor_backtrack = backtrack || dfa.is_accepting_state(state);

        for (_, next) in &dfa.states[state.0].char_transitions {
            work_list.push((*next, successor_backtrack));
        }

        for next_range in dfa.states[state.0].range_transitions.iter() {
            work_list.push((next_range.value, successor_backtrack));
        }

        if let Some(next) = dfa.states[state.0].any_transition {
            work_list.push((next, successor_backtrack));
        }

        if let Some(next) = dfa.states[state.0].end_of_input_transition {
            work_list.push((next, successor_backtrack));
        }
    }

    assert_eq!(visited.len(), dfa.states.len());

    for (state, backtrack) in visited {
        dfa.states[state.0].backtrack = backtrack;
    }
}
