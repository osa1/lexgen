use fxhash::{FxHashMap, FxHashSet};

#[derive(Debug)]
pub(crate) struct NFA {
    states: Vec<State>,
    accepting: FxHashSet<StateIdx>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct StateIdx(usize);

#[derive(Debug)]
struct State {
    char_transitions: FxHashMap<char, FxHashSet<StateIdx>>,
    range_transitions: FxHashMap<(char, char), FxHashSet<StateIdx>>,
    empty_transitions: FxHashSet<StateIdx>,
}

impl State {
    fn new() -> State {
        State {
            char_transitions: Default::default(),
            range_transitions: Default::default(),
            empty_transitions: Default::default(),
        }
    }
}

impl NFA {
    pub(crate) fn new() -> (NFA, StateIdx) {
        (
            NFA {
                states: vec![State::new()],
                accepting: Default::default(),
            },
            StateIdx(0),
        )
    }

    pub(crate) fn new_state(&mut self) -> StateIdx {
        let new_state_idx = StateIdx(self.states.len());
        self.states.push(State::new());
        new_state_idx
    }

    pub(crate) fn add_char_transition(&mut self, state: StateIdx, char: char, next: StateIdx) {
        let not_exists = self.states[state.0]
            .char_transitions
            .entry(char)
            .or_insert(Default::default())
            .insert(next);

        assert!(not_exists, "add_char_transition");
    }

    pub(crate) fn add_range_transition(
        &mut self,
        state: StateIdx,
        range_begin: char,
        range_end: char,
        next: StateIdx,
    ) {
        let not_exists = self.states[state.0]
            .range_transitions
            .entry((range_begin, range_end))
            .or_insert(Default::default())
            .insert(next);

        assert!(not_exists, "add_range_transition");
    }

    pub(crate) fn add_empty_transition(&mut self, state: StateIdx, next: StateIdx) {
        let not_exists = self.states[state.0].empty_transitions.insert(next);

        assert!(not_exists, "add_empty_transition");
    }

    pub(crate) fn make_accepting(&mut self, state: StateIdx) {
        self.accepting.insert(state);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    pub(crate) fn simulate(nfa: &NFA, chars: &mut dyn Iterator<Item = char>) -> bool {
        let mut states: FxHashSet<StateIdx> = Default::default();
        states.insert(StateIdx(0));
        compute_state_closure(nfa, &mut states);

        for char in chars {
            // println!("char = {}", char);
            // println!("states = {:?}", states);

            let mut next_states: FxHashSet<StateIdx> = Default::default();
            for state in states.iter() {
                if let Some(nexts) = next_char_states(nfa, *state, char) {
                    next_states.extend(nexts.into_iter());
                }
                for ((range_begin, range_end), nexts) in &nfa.states[state.0].range_transitions {
                    if char >= *range_begin && char <= *range_end {
                        next_states.extend(nexts.into_iter());
                    }
                }
            }

            compute_state_closure(nfa, &mut next_states);

            states = next_states;
        }

        states.iter().any(|state| nfa.accepting.contains(state))
    }

    fn compute_state_closure(nfa: &NFA, states: &mut FxHashSet<StateIdx>) {
        let mut changed = true;
        while changed {
            changed = false;

            let mut next = states.clone();

            for state in states.iter() {
                for next_state in next_empty_states(nfa, *state) {
                    changed |= next.insert(*next_state);
                }
            }

            *states = next;
        }
    }

    fn next_char_states(nfa: &NFA, state: StateIdx, char: char) -> Option<&FxHashSet<StateIdx>> {
        let state = &nfa.states[state.0];
        state.char_transitions.get(&char)
    }

    fn next_empty_states(nfa: &NFA, state: StateIdx) -> &FxHashSet<StateIdx> {
        let state = &nfa.states[state.0];
        &state.empty_transitions
    }

    use crate::ast::{CharOrRange, CharSet, Regex};
    use crate::regex_to_nfa::regex_to_nfa;

    #[test]
    fn nfa_simulate_char() {
        let re = Regex::Char('a');
        let nfa = regex_to_nfa(&re);
        assert!(!simulate(&nfa, &mut "".chars()));
        assert!(simulate(&nfa, &mut "a".chars()));
    }

    #[test]
    fn nfa_simulate_string() {
        let re = Regex::String("ab".to_owned());
        let nfa = regex_to_nfa(&re);
        assert!(!simulate(&nfa, &mut "".chars()));
        assert!(!simulate(&nfa, &mut "a".chars()));
        assert!(simulate(&nfa, &mut "ab".chars()));
    }

    #[test]
    fn nfa_simulate_char_set_char() {
        let re = Regex::CharSet(CharSet(vec![
            CharOrRange::Char('a'),
            CharOrRange::Char('b'),
        ]));
        let nfa = regex_to_nfa(&re);
        assert!(!simulate(&nfa, &mut "".chars()));
        assert!(simulate(&nfa, &mut "a".chars()));
        assert!(simulate(&nfa, &mut "b".chars()));
        assert!(!simulate(&nfa, &mut "ab".chars()));
        assert!(!simulate(&nfa, &mut "ba".chars()));
    }

    #[test]
    fn nfa_simulate_char_set_range() {
        let re = Regex::CharSet(CharSet(vec![
            CharOrRange::Char('a'),
            CharOrRange::Char('b'),
            CharOrRange::Range('0', '9'),
        ]));
        let nfa = regex_to_nfa(&re);
        assert!(!simulate(&nfa, &mut "".chars()));
        assert!(simulate(&nfa, &mut "a".chars()));
        assert!(simulate(&nfa, &mut "b".chars()));
        assert!(simulate(&nfa, &mut "0".chars()));
        assert!(simulate(&nfa, &mut "1".chars()));
        assert!(simulate(&nfa, &mut "9".chars()));
        assert!(!simulate(&nfa, &mut "ba".chars()));
    }

    #[test]
    fn nfa_simulate_zero_or_more() {
        let re = Regex::ZeroOrMore(Box::new(Regex::Char('a')));
        let nfa = regex_to_nfa(&re);
        assert!(simulate(&nfa, &mut "".chars()));
        assert!(simulate(&nfa, &mut "a".chars()));
        assert!(simulate(&nfa, &mut "aa".chars()));
        assert!(!simulate(&nfa, &mut "aab".chars()));
    }

    #[test]
    fn nfa_simulate_one_or_more() {
        let re = Regex::OneOrMore(Box::new(Regex::Char('a')));
        let nfa = regex_to_nfa(&re);
        assert!(!simulate(&nfa, &mut "".chars()));
        assert!(simulate(&nfa, &mut "a".chars()));
        assert!(simulate(&nfa, &mut "aa".chars()));
        assert!(!simulate(&nfa, &mut "aab".chars()));
    }

    #[test]
    fn nfa_simulate_zero_or_one() {
        let re = Regex::ZeroOrOne(Box::new(Regex::Char('a')));
        let nfa = regex_to_nfa(&re);
        assert!(simulate(&nfa, &mut "".chars()));
        assert!(simulate(&nfa, &mut "a".chars()));
        assert!(!simulate(&nfa, &mut "aa".chars()));
    }

    #[test]
    fn nfa_simulate_concat() {
        let re = Regex::Concat(Box::new(Regex::Char('a')), Box::new(Regex::Char('b')));
        let nfa = regex_to_nfa(&re);
        assert!(!simulate(&nfa, &mut "".chars()));
        assert!(!simulate(&nfa, &mut "a".chars()));
        assert!(simulate(&nfa, &mut "ab".chars()));
        assert!(!simulate(&nfa, &mut "aba".chars()));
        assert!(!simulate(&nfa, &mut "abb".chars()));
    }

    #[test]
    fn nfa_simulate_or() {
        let re = Regex::Or(Box::new(Regex::Char('a')), Box::new(Regex::Char('b')));
        let nfa = regex_to_nfa(&re);
        assert!(!simulate(&nfa, &mut "".chars()));
        assert!(simulate(&nfa, &mut "a".chars()));
        assert!(simulate(&nfa, &mut "b".chars()));
        assert!(!simulate(&nfa, &mut "aa".chars()));
        assert!(!simulate(&nfa, &mut "ab".chars()));
    }
}
