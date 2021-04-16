use crate::dfa::DFA;
use crate::nfa::NFA;

use crate::dfa::StateIdx as DfaStateIdx;
use crate::display::BTreeSetDisplay;
use crate::nfa::StateIdx as NfaStateIdx;

use std::collections::hash_map::Entry;
use std::collections::BTreeSet;

use fxhash::{FxHashMap, FxHashSet};

pub(crate) fn nfa_to_dfa<A: Clone>(nfa: &NFA<A>) -> DFA<A> {
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

        println!("work={}", BTreeSetDisplay(&current_nfa_states));
        println!("{}", dfa);
        println!("current_dfa_state={}", current_dfa_state);

        if finished_dfa_states.contains(&current_dfa_state) {
            continue;
        }

        finished_dfa_states.insert(current_dfa_state);

        for nfa_state in current_nfa_states.iter().copied() {
            if let Some(value) = nfa.get_accepting_state(nfa_state) {
                dfa.add_accepting_state(current_dfa_state, value);
            }

            // Add char transitions
            for (char, next_states) in nfa.char_transitions(nfa_state) {
                let next_states_closure: BTreeSet<NfaStateIdx> =
                    nfa.compute_state_closure(next_states).into_iter().collect();

                work_list.push(next_states_closure.clone());

                let next_dfa_state =
                    dfa_state_of_nfa_states(&mut dfa, &mut state_map, next_states_closure);

                dfa.add_char_transition(current_dfa_state, *char, next_dfa_state);
            }

            // Add range transitions
            for ((range_begin, range_end), next_states) in nfa.range_transitions(nfa_state) {
                let next_states_closure: BTreeSet<NfaStateIdx> =
                    nfa.compute_state_closure(next_states).into_iter().collect();

                work_list.push(next_states_closure.clone());

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

fn dfa_state_of_nfa_states<A>(
    dfa: &mut DFA<A>,
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::{CharOrRange, CharSet, Regex};
    use crate::regex_to_nfa::regex_to_nfa;

    fn regex_to_dfa(re: &Regex) -> DFA<()> {
        nfa_to_dfa(&regex_to_nfa(re, ()))
    }

    #[test]
    fn nfa_to_dfa_simulate_char() {
        let re = Regex::Char('a');
        let dfa = regex_to_dfa(&re);
        assert_eq!(dfa.simulate(&mut "".chars()), None);
        assert_eq!(dfa.simulate(&mut "aa".chars()), None);
        assert_eq!(dfa.simulate(&mut "a".chars()), Some(&()));
        assert_eq!(dfa.simulate(&mut "b".chars()), None);
    }

    #[test]
    fn nfa_to_dfa_simulate_string() {
        let re = Regex::String("ab".to_owned());
        let dfa = regex_to_dfa(&re);
        assert_eq!(dfa.simulate(&mut "".chars()), None);
        assert_eq!(dfa.simulate(&mut "a".chars()), None);
        assert_eq!(dfa.simulate(&mut "ab".chars()), Some(&()));
        assert_eq!(dfa.simulate(&mut "abc".chars()), None);
    }

    #[test]
    fn nfa_to_dfa_simulate_char_set_char() {
        let re = Regex::CharSet(CharSet(vec![
            CharOrRange::Char('a'),
            CharOrRange::Char('b'),
        ]));
        let dfa = regex_to_dfa(&re);
        assert_eq!(dfa.simulate(&mut "".chars()), None);
        assert_eq!(dfa.simulate(&mut "a".chars()), Some(&()));
        assert_eq!(dfa.simulate(&mut "b".chars()), Some(&()));
        assert_eq!(dfa.simulate(&mut "ab".chars()), None);
        assert_eq!(dfa.simulate(&mut "ba".chars()), None);
    }

    #[test]
    fn nfa_to_dfa_simulate_char_set_range() {
        let re = Regex::CharSet(CharSet(vec![
            CharOrRange::Char('a'),
            CharOrRange::Char('b'),
            CharOrRange::Range('0', '9'),
        ]));
        let dfa = regex_to_dfa(&re);
        assert_eq!(dfa.simulate(&mut "".chars()), None);
        assert_eq!(dfa.simulate(&mut "a".chars()), Some(&()));
        assert_eq!(dfa.simulate(&mut "b".chars()), Some(&()));
        assert_eq!(dfa.simulate(&mut "0".chars()), Some(&()));
        assert_eq!(dfa.simulate(&mut "1".chars()), Some(&()));
        assert_eq!(dfa.simulate(&mut "9".chars()), Some(&()));
        assert_eq!(dfa.simulate(&mut "ba".chars()), None);
    }

    #[test]
    fn nfa_to_dfa_simulate_zero_or_more() {
        let re = Regex::ZeroOrMore(Box::new(Regex::Char('a')));
        let dfa = regex_to_dfa(&re);
        assert_eq!(dfa.simulate(&mut "".chars()), Some(&()));
        assert_eq!(dfa.simulate(&mut "a".chars()), Some(&()));
        assert_eq!(dfa.simulate(&mut "aa".chars()), Some(&()));
        assert_eq!(dfa.simulate(&mut "aab".chars()), None);
    }

    #[test]
    fn nfa_to_dfa_simulate_one_or_more() {
        let re = Regex::OneOrMore(Box::new(Regex::Char('a')));
        let dfa = regex_to_dfa(&re);
        assert_eq!(dfa.simulate(&mut "".chars()), None);
        assert_eq!(dfa.simulate(&mut "a".chars()), Some(&()));
        assert_eq!(dfa.simulate(&mut "aa".chars()), Some(&()));
        assert_eq!(dfa.simulate(&mut "aab".chars()), None);
    }

    #[test]
    fn nfa_to_dfa_simulate_zero_or_one() {
        let re = Regex::ZeroOrOne(Box::new(Regex::Char('a')));
        let dfa = regex_to_dfa(&re);
        assert_eq!(dfa.simulate(&mut "".chars()), Some(&()));
        assert_eq!(dfa.simulate(&mut "a".chars()), Some(&()));
        assert_eq!(dfa.simulate(&mut "aa".chars()), None);
    }

    #[test]
    fn nfa_to_dfa_simulate_concat() {
        let re = Regex::Concat(Box::new(Regex::Char('a')), Box::new(Regex::Char('b')));
        let dfa = regex_to_dfa(&re);
        assert_eq!(dfa.simulate(&mut "".chars()), None);
        assert_eq!(dfa.simulate(&mut "a".chars()), None);
        assert_eq!(dfa.simulate(&mut "ab".chars()), Some(&()));
        assert_eq!(dfa.simulate(&mut "aba".chars()), None);
        assert_eq!(dfa.simulate(&mut "abb".chars()), None);
    }

    #[test]
    fn nfa_to_dfa_simulate_or() {
        let re = Regex::Or(Box::new(Regex::Char('a')), Box::new(Regex::Char('b')));
        let dfa = regex_to_dfa(&re);
        assert_eq!(dfa.simulate(&mut "".chars()), None);
        assert_eq!(dfa.simulate(&mut "a".chars()), Some(&()));
        assert_eq!(dfa.simulate(&mut "b".chars()), Some(&()));
        assert_eq!(dfa.simulate(&mut "aa".chars()), None);
        assert_eq!(dfa.simulate(&mut "ab".chars()), None);
    }

    #[test]
    fn fna_to_dfa_multiple_accepting_states() {
        let re1 = Regex::String("aaa".to_owned());
        let re2 = Regex::String("aab".to_owned());
        let mut nfa = regex_to_nfa(&re1, 1usize);
        println!("{}", nfa);
        println!("---");
        nfa.add_regex(&re2, 2usize);
        println!("{}", nfa);
        let dfa = nfa_to_dfa(&nfa);
        // assert_eq!(dfa.simulate(&mut "aaaa".chars()), Some(&1));
        // assert_eq!(dfa.simulate(&mut "aaab".chars()), Some(&2));
        // assert_eq!(dfa.simulate(&mut "aaaba".chars()), None);
        // assert_eq!(dfa.simulate(&mut "aaac".chars()), None);
    }
}
