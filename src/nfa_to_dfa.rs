use crate::dfa::DFA;
use crate::nfa::NFA;

use crate::dfa::StateIdx as DfaStateIdx;
use crate::nfa::StateIdx as NfaStateIdx;

use std::collections::hash_map::Entry;
use std::collections::BTreeSet;

use fxhash::{FxHashMap, FxHashSet};

pub(crate) fn nfa_to_dfa(nfa: &NFA) -> DFA {
    let initial_state = nfa.initial_state();

    let initial_states: BTreeSet<NfaStateIdx> = {
        let mut initial_states: FxHashSet<NfaStateIdx> = Default::default();
        initial_states.insert(initial_state);

        nfa.compute_state_closure(&initial_states)
            .into_iter()
            .collect()
    };

    let (mut dfa, dfa_initial_state) = DFA::new();

    // Maps sets NFA states to their states in the DFA
    let mut state_map: FxHashMap<BTreeSet<NfaStateIdx>, DfaStateIdx> = Default::default();
    state_map.insert(initial_states.clone(), dfa_initial_state);

    let mut work_list: Vec<BTreeSet<NfaStateIdx>> = vec![initial_states];
    let mut finished_dfa_states: FxHashSet<DfaStateIdx> = Default::default();

    while let Some(current_nfa_states) = work_list.pop() {
        let current_dfa_state = match state_map.get(&current_nfa_states) {
            None => {
                let dfa_state = dfa.new_state();
                state_map.insert(current_nfa_states.clone(), dfa_state);
                dfa_state
            }
            Some(dfa_state) => *dfa_state,
        };

        if finished_dfa_states.contains(&current_dfa_state) {
            continue;
        }

        finished_dfa_states.insert(current_dfa_state);

        for nfa_state in current_nfa_states.iter().copied() {
            // Add char transitions
            for (char, next_states) in nfa.char_transitions(nfa_state) {
                let next_states_closure: BTreeSet<NfaStateIdx> =
                    nfa.compute_state_closure(next_states).into_iter().collect();

                let next_dfa_state =
                    dfa_state_of_nfa_states(&mut dfa, &mut state_map, next_states_closure);

                dfa.add_char_transition(current_dfa_state, *char, next_dfa_state);
            }
            // Add range transitions
            for ((range_begin, range_end), next_states) in nfa.range_transitions(nfa_state) {
                let next_states_closure: BTreeSet<NfaStateIdx> =
                    nfa.compute_state_closure(next_states).into_iter().collect();

                let next_dfa_state =
                    dfa_state_of_nfa_states(&mut dfa, &mut state_map, next_states_closure);

                dfa.add_range_transition(
                    current_dfa_state,
                    *range_begin,
                    *range_end,
                    next_dfa_state,
                );
            }
        }
    }

    dfa
}

fn dfa_state_of_nfa_states(
    dfa: &mut DFA,
    state_map: &mut FxHashMap<BTreeSet<NfaStateIdx>, DfaStateIdx>,
    states: BTreeSet<NfaStateIdx>,
) -> DfaStateIdx {
    match state_map.entry(states) {
        Entry::Occupied(entry) => *entry.get(),
        Entry::Vacant(entry) => {
            let dfa_state = dfa.new_state();
            entry.insert(dfa_state);
            dfa_state
        }
    }
}
