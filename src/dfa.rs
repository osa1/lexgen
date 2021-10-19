pub mod codegen;
pub mod simplify;

#[cfg(test)]
pub mod simulate;

use crate::range_map::{Range, RangeMap};

use std::convert::TryFrom;
use std::iter::{FromIterator, IntoIterator};

use fxhash::{FxHashMap, FxHashSet};

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
    // states consume the current character, but failure transitions in other states don't.
    initial: bool,
    char_transitions: FxHashMap<char, T>,
    range_transitions: RangeMap<T>,
    fail_transition: Option<T>,
    accepting: Option<A>,
    // Predecessors of the state, used to inline code for a state with one predecessor in the
    // predecessor's code
    predecessors: FxHashSet<StateIdx>,
}

impl<T, A> State<T, A> {
    fn new() -> State<T, A> {
        State {
            initial: false,
            char_transitions: Default::default(),
            range_transitions: Default::default(),
            fail_transition: None,
            accepting: None,
            predecessors: Default::default(),
        }
    }

    fn has_no_transitions(&self) -> bool {
        self.char_transitions.is_empty()
            && self.range_transitions.is_empty()
            && self.fail_transition.is_none()
    }

    pub fn map_semantic_action<B, F>(self, f: F) -> State<T, B>
    where
        F: FnMut(A) -> B,
    {
        let State {
            initial,
            char_transitions,
            range_transitions,
            fail_transition,
            accepting,
            predecessors,
        } = self;
        State {
            initial,
            char_transitions,
            range_transitions,
            fail_transition,
            accepting: accepting.map(f),
            predecessors,
        }
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

        self.states[next.0].predecessors.insert(state);
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

        self.states[next.0].predecessors.insert(state);
    }

    pub fn add_fail_transition(&mut self, state: StateIdx, next: StateIdx) {
        assert!(self.states[state.0].fail_transition.is_none());
        self.states[state.0].fail_transition = Some(next);
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
            fail_transition,
            accepting,
            predecessors,
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

            let predecessors = predecessors
                .into_iter()
                .map(|pred| StateIdx(pred.0 + n_current_states))
                .collect();

            self.states.push(State {
                initial,
                char_transitions: new_char_transitions,
                range_transitions: new_range_transitions,
                fail_transition: new_fail_transition,
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
                fail_transition,
                accepting,
                predecessors: _,
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
                    write!(f, "      ")?;
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
