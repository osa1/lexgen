use fxhash::{FxHashMap, FxHashSet};

#[derive(Debug)]
pub(crate) struct DFA {
    states: Vec<State>,
    accepting: FxHashSet<StateIdx>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct StateIdx(usize);

#[derive(Debug)]
struct State {
    char_transitions: FxHashMap<char, StateIdx>,
    range_transitions: FxHashMap<(char, char), StateIdx>,
}

impl State {
    fn new() -> State {
        State {
            char_transitions: Default::default(),
            range_transitions: Default::default(),
        }
    }
}

impl DFA {
    pub(crate) fn new() -> (DFA, StateIdx) {
        (
            DFA {
                states: vec![State::new()],
                accepting: Default::default(),
            },
            StateIdx(0),
        )
    }

    pub(crate) fn initial_state(&self) -> StateIdx {
        StateIdx(0)
    }

    pub(crate) fn add_accepting_state(&mut self, state: StateIdx) {
        self.accepting.insert(state);
    }

    pub(crate) fn new_state(&mut self) -> StateIdx {
        let new_state_idx = StateIdx(self.states.len());
        self.states.push(State::new());
        new_state_idx
    }

    pub(crate) fn add_char_transition(&mut self, state: StateIdx, char: char, next: StateIdx) {
        let old = self.states[state.0].char_transitions.insert(char, next);
        assert!(old.is_none());
    }

    pub(crate) fn add_range_transition(
        &mut self,
        state: StateIdx,
        range_begin: char,
        range_end: char,
        next: StateIdx,
    ) {
        let old = self.states[state.0]
            .range_transitions
            .insert((range_begin, range_end), next);
        assert!(old.is_none());
    }

    pub(crate) fn simulate(&self, chars: &mut dyn Iterator<Item = char>) -> bool {
        let mut state = StateIdx(0);

        'char_loop: for char in chars {
            if let Some(next) = self.states[state.0].char_transitions.get(&char) {
                state = *next;
                continue;
            }

            for ((range_begin, range_end), next) in &self.states[state.0].range_transitions {
                if char >= *range_begin && char <= *range_end {
                    state = *next;
                    continue 'char_loop;
                }
            }

            return false;
        }

        self.accepting.contains(&state)
    }
}
