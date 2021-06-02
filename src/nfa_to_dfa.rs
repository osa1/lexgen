use crate::dfa::DFA;
use crate::nfa::NFA;
use crate::range_map::RangeMap;

use crate::dfa::StateIdx as DfaStateIdx;
use crate::nfa::StateIdx as NfaStateIdx;

use std::collections::hash_map::Entry;
use std::collections::BTreeSet;

use fxhash::{FxHashMap, FxHashSet};

pub fn nfa_to_dfa<A: Clone>(nfa: &NFA<A>) -> DFA<DfaStateIdx, A> {
    let initial_state = nfa.initial_state();

    let initial_states: BTreeSet<NfaStateIdx> = {
        let mut initial_states: FxHashSet<NfaStateIdx> = Default::default();
        initial_states.insert(initial_state);

        nfa.compute_state_closure(&initial_states)
            .into_iter()
            .collect()
    };

    let (mut dfa, dfa_initial_state): (DFA<DfaStateIdx, A>, DfaStateIdx) = DFA::new();

    // Maps sets NFA states to their states in the DFA
    let mut state_map: FxHashMap<BTreeSet<NfaStateIdx>, DfaStateIdx> = Default::default();
    state_map.insert(initial_states.clone(), dfa_initial_state);

    let mut work_list: Vec<BTreeSet<NfaStateIdx>> = vec![initial_states];
    let mut finished_dfa_states: FxHashSet<DfaStateIdx> = Default::default();

    let fail_dfa_state = nfa.get_fail_action().map(|fail_action| {
        let fail_state = dfa.new_state();
        dfa.make_state_accepting(fail_state, fail_action.clone());
        fail_state
    });

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

        let mut char_transitions: FxHashMap<char, FxHashSet<NfaStateIdx>> = Default::default();
        let mut range_transitions: RangeMap<NfaStateIdx> = Default::default();

        for nfa_state in current_nfa_states.iter().copied() {
            if let Some(value) = nfa.get_accepting_state(nfa_state) {
                dfa.make_state_accepting(current_dfa_state, value.clone());
            }

            // Collect char transitions
            for (char, next_states) in nfa.char_transitions(nfa_state) {
                char_transitions
                    .entry(*char)
                    .or_default()
                    .extend(next_states.iter().copied());
            }

            // Collect range transitions
            for ((range_begin, range_end), next_states) in nfa.range_transitions(nfa_state) {
                let next_states = next_states.iter().copied().collect::<Vec<NfaStateIdx>>();
                range_transitions.insert_values(
                    *range_begin as u32,
                    *range_end as u32,
                    &next_states,
                );
            }
        }

        // Compute closures of transition targets and add transitions to DFA
        for (char, mut char_states) in char_transitions.into_iter() {
            // For ranges that also cover the char we need to add the range transitions to the char
            // transition
            for range in range_transitions.iter() {
                if char as u32 >= range.start && char as u32 <= range.end {
                    for range_state in &range.values {
                        char_states.insert(*range_state);
                    }
                }
            }

            let closure: BTreeSet<NfaStateIdx> = nfa
                .compute_state_closure(&char_states)
                .into_iter()
                .collect();
            let dfa_state = dfa_state_of_nfa_states(&mut dfa, &mut state_map, closure.clone());
            dfa.add_char_transition(current_dfa_state, char, dfa_state);

            work_list.push(closure);
        }

        for range in range_transitions.into_iter() {
            let state_set: FxHashSet<NfaStateIdx> = range.values.into_iter().collect();
            let closure: BTreeSet<NfaStateIdx> =
                nfa.compute_state_closure(&state_set).into_iter().collect();
            let dfa_state = dfa_state_of_nfa_states(&mut dfa, &mut state_map, closure.clone());
            dfa.add_range_transition(current_dfa_state, range.start, range.end, dfa_state);

            work_list.push(closure);
        }

        if let Some(fail_dfa_state) = fail_dfa_state {
            dfa.add_fail_transition(current_dfa_state, fail_dfa_state);
        }
    }

    dfa
}

fn dfa_state_of_nfa_states<A>(
    dfa: &mut DFA<DfaStateIdx, A>,
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
