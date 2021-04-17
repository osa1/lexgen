use fxhash::FxHashMap;

/// Deterministic finite automate, parameterized on values of accepting states.
#[derive(Debug)]
pub struct DFA<A> {
    states: Vec<State>,
    accepting: FxHashMap<StateIdx, A>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StateIdx(usize);

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

impl<A> DFA<A> {
    pub fn new() -> (DFA<A>, StateIdx) {
        (
            DFA {
                states: vec![State::new()],
                accepting: Default::default(),
            },
            StateIdx(0),
        )
    }

    pub fn initial_state(&self) -> StateIdx {
        StateIdx(0)
    }

    pub fn add_accepting_state(&mut self, state: StateIdx, value: A) {
        self.accepting.insert(state, value);
    }

    pub fn new_state(&mut self) -> StateIdx {
        let new_state_idx = StateIdx(self.states.len());
        self.states.push(State::new());
        new_state_idx
    }

    pub fn add_char_transition(&mut self, state: StateIdx, char: char, next: StateIdx) {
        let old = self.states[state.0].char_transitions.insert(char, next);
        assert!(
            old.is_none(),
            "state={:?}, char={:?}, old={:?}, new={:?}",
            state,
            char,
            old,
            next
        );
    }

    pub fn add_range_transition(
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

    pub fn simulate(&self, chars: &mut dyn Iterator<Item = char>) -> Option<&A> {
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

            return None;
        }

        self.accepting.get(&state)
    }
}

use std::fmt::{self, Display, Formatter};

impl Display for StateIdx {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<A> Display for DFA<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (state_idx, state) in self.states.iter().enumerate() {
            if self.accepting.contains_key(&StateIdx(state_idx)) {
                write!(f, "{:>4}", format!("*{}", state_idx))?;
            } else {
                write!(f, "{:>4}:", state_idx)?;
            }

            let State {
                char_transitions,
                range_transitions,
            } = state;

            let mut first = true;

            for (char, next) in char_transitions.iter() {
                if !first {
                    write!(f, "     ")?;
                } else {
                    first = false;
                }

                writeln!(f, "{:?} -> {}", char, next)?;
            }

            for ((range_begin, range_end), next) in range_transitions.iter() {
                if !first {
                    write!(f, "     ")?;
                } else {
                    first = false;
                }

                writeln!(f, "{:?} - {:?} -> {}", range_begin, range_end, next)?;
            }

            if char_transitions.is_empty() && range_transitions.is_empty() {
                writeln!(f)?;
            }
        }

        Ok(())
    }
}
