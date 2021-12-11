pub mod codegen;
pub mod simplify;

#[cfg(test)]
pub mod simulate;

use crate::collections::{Map, Set};
use crate::nfa::{AcceptingState, RightCtx};
use crate::range_map::{Range, RangeMap};

use std::convert::TryFrom;
use std::iter::{FromIterator, IntoIterator};

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
pub struct State<T, A> {
    // Is this the initial state of a rule set? This is important as failure transitions in initial
    // states consume the current character, but failure transitions in other states don't. (#12)
    initial: bool,
    char_transitions: Map<char, T>,
    range_transitions: RangeMap<T>,
    any_transition: Option<T>,
    end_of_input_transition: Option<T>,
    accepting: Vec<AcceptingState<A, StateIdx>>,
    // Predecessors of the state, used to inline code for a state with one predecessor in the
    // predecessor's code
    predecessors: Set<StateIdx>,
}

impl<T, A> State<T, A> {
    fn new() -> State<T, A> {
        State {
            initial: false,
            char_transitions: Default::default(),
            range_transitions: Default::default(),
            any_transition: None,
            end_of_input_transition: None,
            accepting: vec![],
            predecessors: Default::default(),
        }
    }

    fn has_no_transitions(&self) -> bool {
        self.char_transitions.is_empty()
            && self.range_transitions.is_empty()
            && self.any_transition.is_none()
            && self.end_of_input_transition.is_none()
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

    pub fn make_state_accepting(&mut self, state: StateIdx, accept: AcceptingState<A, StateIdx>) {
        self.states[state.0].accepting.push(accept);
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

        self.states[next.0].predecessors.insert(state);
    }

    pub fn set_range_transitions(&mut self, state: StateIdx, range_map: RangeMap<StateIdx>) {
        assert!(self.states[state.0].range_transitions.is_empty());

        for range in range_map.iter() {
            self.states[range.value.0].predecessors.insert(state);
        }

        self.states[state.0].range_transitions = range_map;
    }

    pub fn set_any_transition(&mut self, state: StateIdx, next: StateIdx) {
        assert!(self.states[state.0].any_transition.is_none());
        self.states[state.0].any_transition = Some(next);
        self.states[next.0].predecessors.insert(state);
    }

    pub fn set_end_of_input_transition(&mut self, state: StateIdx, next: StateIdx) {
        assert!(self.states[state.0].end_of_input_transition.is_none());
        self.states[state.0].end_of_input_transition = Some(next);
        self.states[next.0].predecessors.insert(state);
    }
}

impl<T, A> DFA<T, A> {
    fn from_states(states: Vec<State<T, A>>) -> DFA<T, A> {
        DFA { states }
    }

    pub fn into_state_indices(self) -> impl Iterator<Item = (StateIdx, State<T, A>)> {
        self.states
            .into_iter()
            .enumerate()
            .map(|(state_idx, state)| (StateIdx(state_idx), state))
    }
}

impl<T, A> FromIterator<(StateIdx, State<T, A>)> for DFA<T, A> {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (StateIdx, State<T, A>)>,
    {
        let mut states: Vec<(StateIdx, State<T, A>)> = iter.into_iter().collect();
        states.sort_by_key(|&(state_idx, _)| state_idx);

        DFA {
            states: states.into_iter().map(|(_, state)| state).collect(),
        }
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
            any_transition,
            end_of_input_transition,
            accepting,
            predecessors,
        } in other.states
        {
            let mut new_char_transitions: Map<char, StateIdx> = Default::default();
            let mut new_any_transition: Option<StateIdx> = None;
            let mut new_end_of_input_transition: Option<StateIdx> = None;

            for (char, next) in char_transitions {
                new_char_transitions.insert(char, StateIdx(next.0 + n_current_states));
            }

            let new_range_transitions =
                range_transitions.map(|state_idx| StateIdx(state_idx.0 + n_current_states));

            if let Some(next) = any_transition {
                new_any_transition = Some(StateIdx(next.0 + n_current_states));
            }

            if let Some(next) = end_of_input_transition {
                new_end_of_input_transition = Some(StateIdx(next.0 + n_current_states));
            }

            let predecessors = predecessors
                .into_iter()
                .map(|pred| StateIdx(pred.0 + n_current_states))
                .collect();

            self.states.push(State {
                initial,
                char_transitions: new_char_transitions,
                range_transitions: new_range_transitions,
                any_transition: new_any_transition,
                end_of_input_transition: new_end_of_input_transition,
                accepting,
                predecessors,
            });
        }

        StateIdx(n_current_states)
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
                any_transition,
                end_of_input_transition,
                accepting,
                predecessors: _,
            } = state;

            if !accepting.is_empty() {
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

            for (char, next) in char_transitions.iter() {
                if !first {
                    write!(f, "      ")?;
                } else {
                    first = false;
                }

                writeln!(f, "{:?} -> {}", char, next)?;
            }

            for Range { start, end, value } in range_transitions.iter() {
                if !first {
                    write!(f, "      ")?;
                } else {
                    first = false;
                }

                writeln!(
                    f,
                    "{:?} - {:?} -> {}",
                    char::try_from(*start).unwrap(),
                    char::try_from(*end).unwrap(),
                    value,
                )?;
            }

            if let Some(next) = any_transition {
                if !first {
                    write!(f, "      ")?;
                } else {
                    first = false;
                }

                writeln!(f, "_ -> {}", next)?;
            }

            if let Some(next) = end_of_input_transition {
                if !first {
                    write!(f, "      ")?;
                }

                writeln!(f, "$ -> {}", next)?;
            }

            if char_transitions.is_empty()
                && range_transitions.is_empty()
                && any_transition.is_none()
                && end_of_input_transition.is_none()
            {
                writeln!(f)?;
            }
        }

        Ok(())
    }
}
