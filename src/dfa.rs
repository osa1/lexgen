pub mod codegen;

use fxhash::FxHashMap;

/// Deterministic finite automate, parameterized on values of accepting states.
#[derive(Debug)]
pub struct DFA<A> {
    states: Vec<State>,
    accepting: FxHashMap<StateIdx, A>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StateIdx(usize);

#[derive(Debug, Clone)]
struct State {
    char_transitions: FxHashMap<char, StateIdx>,
    range_transitions: FxHashMap<(char, char), StateIdx>,
    fail_transition: Option<StateIdx>,
}

impl State {
    fn new() -> State {
        State {
            char_transitions: Default::default(),
            range_transitions: Default::default(),
            fail_transition: None,
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
        self.accepting.entry(state).or_insert(value);
        // Old code that doesn't work when a char transitions overlaps with a range transition:
        //
        // let old = self.accepting.insert(state, value);
        // assert!(
        //     old.is_none(),
        //     "add_accepting_state overriding action in state={:?}",
        //     state,
        // );
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

    pub fn add_fail_transition(&mut self, state: StateIdx, next: StateIdx) {
        assert!(self.states[state.0].fail_transition.is_none());
        self.states[state.0].fail_transition = Some(next);
    }
}

impl<A> DFA<A> {
    /// Extend the current DFA with another DFA. The extended DFA's states will be renumbered. This
    /// does not add any transitions from the original DFA states to the extension. Accepting
    /// states of the extension is preserved.
    ///
    /// Returns initial state for the extension in the new DFA.
    pub fn add_dfa(&mut self, other: DFA<A>) -> StateIdx {
        let n_current_states = self.states.len();

        for state in &other.states {
            let mut char_transitions: FxHashMap<char, StateIdx> = Default::default();
            let mut range_transitions: FxHashMap<(char, char), StateIdx> = Default::default();
            let mut fail_transition: Option<StateIdx> = None;

            for (char, next) in &state.char_transitions {
                char_transitions.insert(*char, StateIdx(next.0 + n_current_states));
            }

            for (range, next) in &state.range_transitions {
                range_transitions.insert(*range, StateIdx(next.0 + n_current_states));
            }

            if let Some(next) = &state.fail_transition {
                fail_transition = Some(StateIdx(next.0 + n_current_states));
            }

            self.states.push(State {
                char_transitions,
                range_transitions,
                fail_transition,
            });
        }

        for (idx, action) in other.accepting {
            self.accepting
                .insert(StateIdx(idx.0 + n_current_states), action);
        }

        StateIdx(n_current_states)
    }
}

impl<A> DFA<A> {
    #[cfg(test)]
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

            if let Some(next) = self.states[state.0].fail_transition {
                state = next;
                continue;
            }

            return None;
        }

        match self.accepting.get(&state) {
            Some(action) => Some(action),
            None => match self.states[state.0].fail_transition {
                None => None,
                Some(fail_state) => self.accepting.get(&fail_state),
            },
        }
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
                write!(f, "{:>4}:", format!("*{}", state_idx))?;
            } else {
                write!(f, "{:>4}:", state_idx)?;
            }

            let State {
                char_transitions,
                range_transitions,
                fail_transition,
            } = state;

            let mut first = true;

            if let Some(next) = fail_transition {
                writeln!(f, "FAIL -> {}", next)?;
                first = false;
            }

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
