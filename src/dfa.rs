pub mod codegen;
pub mod simplify;

use crate::range_map::{Range, RangeMap};

use std::convert::TryFrom;

use fxhash::FxHashMap;

/// Deterministic finite automate, parameterized on values of accepting states.
#[derive(Debug)]
pub struct DFA<T, A> {
    // Indexed by `StateIdx`
    states: Vec<State<T, A>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StateIdx(usize);

impl StateIdx {
    fn map<F>(&self, f: F) -> StateIdx
    where
        F: Fn(usize) -> usize,
    {
        StateIdx(f(self.0))
    }
}

#[derive(Debug)]
struct State<T, A> {
    // Is this the initial state of a rule set? This is important as failure transitions in initial
    // states consumes the current character, but failure transitions in other states don't.
    initial: bool,
    char_transitions: FxHashMap<char, T>,
    range_transitions: RangeMap<T>,
    fail_transition: Option<T>,
    accepting: Option<A>,
}

impl<T, A> State<T, A> {
    fn new() -> State<T, A> {
        State {
            initial: false,
            char_transitions: Default::default(),
            range_transitions: Default::default(),
            fail_transition: None,
            accepting: None,
        }
    }

    fn has_no_transitions(&self) -> bool {
        self.char_transitions.is_empty()
            && self.range_transitions.is_empty()
            && self.fail_transition.is_none()
    }
}

impl<A> DFA<StateIdx, A> {
    pub fn new() -> (DFA<StateIdx, A>, StateIdx) {
        let mut initial_state = State::new();
        initial_state.initial = true;
        (
            DFA {
                states: vec![initial_state],
            },
            StateIdx(0),
        )
    }

    pub fn initial_state(&self) -> StateIdx {
        StateIdx(0)
    }

    pub fn make_state_accepting(&mut self, state: StateIdx, value: A) {
        // Give first rule priority
        let accepting = &mut self.states[state.0].accepting;
        if accepting.is_none() {
            *accepting = Some(value);
        }
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
        range_begin: u32,
        range_end: u32,
        next: StateIdx,
    ) {
        self.states[state.0]
            .range_transitions
            .insert(range_begin, range_end, next);
    }

    pub fn add_fail_transition(&mut self, state: StateIdx, next: StateIdx) {
        assert!(self.states[state.0].fail_transition.is_none());
        self.states[state.0].fail_transition = Some(next);
    }
}

impl<T, A> DFA<T, A> {
    fn from_states(states: Vec<State<T, A>>) -> DFA<T, A> {
        DFA { states }
    }

    fn into_state_indices(self) -> impl Iterator<Item = (StateIdx, State<T, A>)> {
        self.states
            .into_iter()
            .enumerate()
            .map(|(state_idx, state)| (StateIdx(state_idx), state))
    }
}

impl<A> DFA<StateIdx, A> {
    /// Extend the current DFA with another DFA. The extended DFA's states will be renumbered. This
    /// does not add any transitions from the original DFA states to the extension. Accepting
    /// states of the extension is preserved.
    ///
    /// Returns initial state for the extension in the new DFA.
    pub fn add_dfa(&mut self, other: DFA<StateIdx, A>) -> StateIdx {
        let n_current_states = self.states.len();

        for State {
            initial,
            char_transitions,
            range_transitions,
            fail_transition,
            accepting,
        } in other.states
        {
            let mut new_char_transitions: FxHashMap<char, StateIdx> = Default::default();
            let mut new_range_transitions: RangeMap<StateIdx> = Default::default();
            let mut new_fail_transition: Option<StateIdx> = None;

            for (char, next) in char_transitions {
                new_char_transitions.insert(char, StateIdx(next.0 + n_current_states));
            }

            for range in range_transitions.iter() {
                let values = &range.values;
                assert_eq!(values.len(), 1);
                new_range_transitions.insert(
                    range.start,
                    range.end,
                    StateIdx(values[0].0 + n_current_states),
                );
            }

            if let Some(next) = fail_transition {
                new_fail_transition = Some(StateIdx(next.0 + n_current_states));
            }

            self.states.push(State {
                initial,
                char_transitions: new_char_transitions,
                range_transitions: new_range_transitions,
                fail_transition: new_fail_transition,
                accepting,
            });
        }

        StateIdx(n_current_states)
    }
}

impl<A> DFA<StateIdx, A> {
    #[cfg(test)]
    pub fn simulate(&self, chars: &mut dyn Iterator<Item = char>) -> Option<&A> {
        let mut state = StateIdx(0);

        'char_loop: for char in chars {
            if let Some(next) = self.states[state.0].char_transitions.get(&char) {
                state = *next;
                continue;
            }

            for range in self.states[state.0].range_transitions.iter() {
                let Range { start, end, values } = range;
                assert_eq!(values.len(), 1);
                let next = values[0];
                if char as u32 >= *start && char as u32 <= *end {
                    state = next;
                    continue 'char_loop;
                }
            }

            if let Some(next) = self.states[state.0].fail_transition {
                state = next;
                continue;
            }

            return None;
        }

        match &self.states[state.0].accepting {
            Some(action) => Some(action),
            None => match self.states[state.0].fail_transition {
                None => None,
                Some(fail_state) => self.states[fail_state.0].accepting.as_ref(),
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

impl<A> Display for DFA<StateIdx, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (state_idx, state) in self.states.iter().enumerate() {
            let State {
                initial,
                char_transitions,
                range_transitions,
                fail_transition,
                accepting,
            } = state;

            if accepting.is_some() {
                if *initial {
                    write!(f, "{:>5}:", format!("i*{}", state_idx))?;
                } else {
                    write!(f, "{:>5}:", format!("*{}", state_idx))?;
                }
            } else {
                if *initial {
                    write!(f, "{:>5}:", format!("i{}", state_idx))?;
                } else {
                    write!(f, "{:>5}:", state_idx)?;
                }
            }

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

            for Range { start, end, values } in range_transitions.iter() {
                if !first {
                    write!(f, "     ")?;
                } else {
                    first = false;
                }

                assert_eq!(values.len(), 1);
                writeln!(
                    f,
                    "{:?} - {:?} -> {}",
                    char::try_from(*start).unwrap(),
                    char::try_from(*end).unwrap(),
                    values[0]
                )?;
            }

            if char_transitions.is_empty()
                && range_transitions.is_empty()
                && fail_transition.is_none()
            {
                writeln!(f)?;
            }
        }

        Ok(())
    }
}
