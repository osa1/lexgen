use crate::collections::{Map, Set};
use crate::dfa::DFA;
use crate::nfa::NFA;
use crate::range_map::{Range, RangeMap};

use crate::dfa::StateIdx as DfaStateIdx;
use crate::nfa::StateIdx as NfaStateIdx;

use std::collections::hash_map::Entry;
use std::collections::BTreeSet;

pub fn nfa_to_dfa<A: Clone>(nfa: &NFA<A>) -> DFA<DfaStateIdx, A> {
    let initial_state = nfa.initial_state();

    let initial_states: BTreeSet<NfaStateIdx> = {
        let mut initial_states: Set<NfaStateIdx> = Default::default();
        initial_states.insert(initial_state);

        nfa.compute_state_closure(&initial_states)
            .into_iter()
            .collect()
    };

    let (mut dfa, dfa_initial_state): (DFA<DfaStateIdx, A>, DfaStateIdx) = DFA::new();

    // Maps sets NFA states to their states in the DFA
    let mut state_map: Map<BTreeSet<NfaStateIdx>, DfaStateIdx> = Default::default();
    state_map.insert(initial_states.clone(), dfa_initial_state);

    let mut work_list: Vec<BTreeSet<NfaStateIdx>> = vec![initial_states];
    let mut finished_dfa_states: Set<DfaStateIdx> = Default::default();

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

        let mut char_transitions: Map<char, Set<NfaStateIdx>> = Default::default();
        let mut range_transitions: RangeMap<Set<NfaStateIdx>> = Default::default();
        let mut any_transitions: Set<NfaStateIdx> = Default::default();
        let mut end_of_input_transitions: Set<NfaStateIdx> = Default::default();

        for nfa_state in current_nfa_states.iter().copied() {
            if let Some(accepting_state) = nfa.get_accepting_state(nfa_state) {
                dfa.make_state_accepting(current_dfa_state, accepting_state.value.clone());
            }

            // Collect char transitions
            for (char, next_states) in nfa.char_transitions(nfa_state) {
                char_transitions
                    .entry(*char)
                    .or_default()
                    .extend(next_states.iter().copied());
            }

            // Collect range transitions
            for range in nfa.range_transitions(nfa_state) {
                range_transitions.insert(
                    range.start,
                    range.end,
                    range.value.clone(),
                    |states_1, states_2| states_1.extend(states_2.into_iter()),
                );
            }

            // Collect any transitions
            any_transitions.extend(nfa.any_transitions(nfa_state));

            // Collect end-of-input transitions
            end_of_input_transitions.extend(nfa.end_of_input_transitions(nfa_state));
        }

        // Compute closures of transition targets and add transitions to DFA
        for (char, mut char_states) in char_transitions.into_iter() {
            // For ranges that also cover the char we need to add the range transitions to the char
            // transition
            for range in range_transitions.iter() {
                if range.contains(char) {
                    for range_state in &range.value {
                        char_states.insert(*range_state);
                    }
                }
            }

            // Same for '_' (match any character) transitions
            for any_next in &any_transitions {
                char_states.insert(*any_next);
            }

            let closure: BTreeSet<NfaStateIdx> = nfa
                .compute_state_closure(&char_states)
                .into_iter()
                .collect();
            let dfa_state = dfa_state_of_nfa_states(&mut dfa, &mut state_map, closure.clone());
            dfa.add_char_transition(current_dfa_state, char, dfa_state);

            work_list.push(closure);
        }

        let mut dfa_range_transitions: Vec<Range<DfaStateIdx>> =
            Vec::with_capacity(range_transitions.len());

        for range in range_transitions.into_iter() {
            let mut range_states: Set<NfaStateIdx> = range.value;

            for any_next in &any_transitions {
                range_states.insert(*any_next);
            }

            let closure: BTreeSet<NfaStateIdx> = nfa
                .compute_state_closure(&range_states)
                .into_iter()
                .collect();

            let dfa_state = dfa_state_of_nfa_states(&mut dfa, &mut state_map, closure.clone());

            dfa_range_transitions.push(Range {
                start: range.start,
                end: range.end,
                value: dfa_state,
            });

            work_list.push(closure);
        }

        dfa.set_range_transitions(
            current_dfa_state,
            RangeMap::from_non_overlapping_sorted_ranges(dfa_range_transitions),
        );

        {
            let closure: BTreeSet<NfaStateIdx> = nfa
                .compute_state_closure(&any_transitions)
                .into_iter()
                .collect();

            if !closure.is_empty() {
                let dfa_state = dfa_state_of_nfa_states(&mut dfa, &mut state_map, closure.clone());
                dfa.set_any_transition(current_dfa_state, dfa_state);
                work_list.push(closure);
            }
        }

        {
            let closure: BTreeSet<NfaStateIdx> = nfa
                .compute_state_closure(&end_of_input_transitions)
                .into_iter()
                .collect();

            if !closure.is_empty() {
                let dfa_state = dfa_state_of_nfa_states(&mut dfa, &mut state_map, closure.clone());
                dfa.set_end_of_input_transition(current_dfa_state, dfa_state);
                work_list.push(closure);
            }
        }
    }

    dfa
}

fn dfa_state_of_nfa_states<A>(
    dfa: &mut DFA<DfaStateIdx, A>,
    state_map: &mut Map<BTreeSet<NfaStateIdx>, DfaStateIdx>,
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
