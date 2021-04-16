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
