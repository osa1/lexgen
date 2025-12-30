#[cfg(test)]
pub mod simulate;

use crate::ast::{Regex, Var};
use crate::collections::{Map, Set};
use crate::display::HashSetDisplay;
use crate::range_map::{Range, RangeMap};
use crate::regex_to_nfa;
use crate::right_ctx::RightCtxIdx;

/// Non-deterministic finite automate, parameterized on values of accepting states.
#[derive(Debug)]
pub struct NFA<A> {
    // Indexed by `StateIdx`
    states: Vec<State<A>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StateIdx(usize);

#[derive(Debug)]
struct State<A> {
    range_transitions: RangeMap<Set<StateIdx>>,
    empty_transitions: Set<StateIdx>,
    any_transitions: Set<StateIdx>,
    end_of_input_transitions: Set<StateIdx>,
    accepting: Option<AcceptingState<A>>,
}

#[derive(Debug, Clone, Copy)]
pub struct AcceptingState<A> {
    pub value: A,
    pub right_ctx: Option<RightCtxIdx>,
}

impl<A> State<A> {
    fn new() -> State<A> {
        State {
            range_transitions: Default::default(),
            empty_transitions: Default::default(),
            any_transitions: Default::default(),
            end_of_input_transitions: Default::default(),
            accepting: None,
        }
    }
}

impl<A> NFA<A> {
    pub fn new() -> NFA<A> {
        NFA {
            states: vec![State::new()],
        }
    }

    pub fn initial_state(&self) -> StateIdx {
        StateIdx(0)
    }

    pub fn get_accepting_state(&self, state: StateIdx) -> Option<&AcceptingState<A>> {
        self.states[state.0].accepting.as_ref()
    }

    pub fn range_transitions(
        &self,
        state: StateIdx,
    ) -> impl Iterator<Item = &Range<Set<StateIdx>>> {
        self.states[state.0].range_transitions.iter()
    }

    pub fn any_transitions(&self, state: StateIdx) -> impl Iterator<Item = StateIdx> + '_ {
        self.states[state.0].any_transitions.iter().copied()
    }

    pub fn end_of_input_transitions(&self, state: StateIdx) -> impl Iterator<Item = StateIdx> + '_ {
        self.states[state.0]
            .end_of_input_transitions
            .iter()
            .copied()
    }

    pub fn new_state(&mut self) -> StateIdx {
        let new_state_idx = StateIdx(self.states.len());
        self.states.push(State::new());
        new_state_idx
    }

    pub fn add_regex(
        &mut self,
        bindings: &Map<Var, Regex>,
        re: &Regex,
        right_ctx: Option<RightCtxIdx>,
        value: A,
    ) {
        let re_accepting_state = self.new_state();

        self.make_state_accepting(re_accepting_state, value, right_ctx);

        let re_initial_state = self.new_state();
        let nfa_initial_state = self.initial_state();

        self.add_empty_transition(nfa_initial_state, re_initial_state);

        regex_to_nfa::add_re(self, bindings, re, re_initial_state, re_accepting_state);
    }

    pub fn add_char_transition(&mut self, state: StateIdx, char: char, next: StateIdx) {
        self.add_range_transition(state, char, char, next)
    }

    pub fn add_range_transition(
        &mut self,
        state: StateIdx,
        range_start: char,
        range_end: char,
        next: StateIdx,
    ) {
        let mut set: Set<StateIdx> = Default::default();
        set.insert(next);
        self.states[state.0].range_transitions.insert(
            range_start,
            range_end,
            set,
            |values_1, values_2| values_1.extend(values_2),
        );
    }

    pub fn add_range_transitions(&mut self, state: StateIdx, ranges: RangeMap<()>, next: StateIdx) {
        let mut set: Set<StateIdx> = Default::default();
        set.insert(next);

        let ranges = ranges.map(|()| set.clone());

        self.states[state.0]
            .range_transitions
            .insert_ranges(ranges.into_iter(), |values_1, values_2| {
                values_1.extend(values_2)
            });
    }

    pub fn add_empty_transition(&mut self, state: StateIdx, next: StateIdx) {
        let not_exists = self.states[state.0].empty_transitions.insert(next);

        assert!(not_exists, "add_empty_transition");
    }

    pub fn add_any_transition(&mut self, state: StateIdx, next: StateIdx) {
        let not_exists = self.states[state.0].any_transitions.insert(next);

        assert!(not_exists, "add_any_transition");
    }

    pub fn add_end_of_input_transition(&mut self, state: StateIdx, next: StateIdx) {
        let not_exists = self.states[state.0].end_of_input_transitions.insert(next);

        assert!(not_exists, "add_end_of_input_transition");
    }

    fn make_state_accepting(&mut self, state: StateIdx, value: A, right_ctx: Option<RightCtxIdx>) {
        let old = self.states[state.0]
            .accepting
            .replace(AcceptingState { value, right_ctx });

        assert!(old.is_none(), "make_state_accepting");
    }

    pub fn compute_state_closure(&self, states: &Set<StateIdx>) -> Set<StateIdx> {
        let mut worklist: Vec<StateIdx> = states.iter().copied().collect();
        let mut closure: Set<StateIdx> = states.clone();

        while let Some(work) = worklist.pop() {
            for next_state in self.next_empty_states(work) {
                if closure.insert(*next_state) {
                    worklist.push(*next_state);
                }
            }
        }

        closure
    }

    fn next_empty_states(&self, state: StateIdx) -> &Set<StateIdx> {
        let state = &self.states[state.0];
        &state.empty_transitions
    }
}

use std::fmt::{self, Display, Formatter};

impl Display for StateIdx {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<A> Display for NFA<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (state_idx, state) in self.states.iter().enumerate() {
            let State {
                range_transitions,
                empty_transitions,
                any_transitions,
                end_of_input_transitions,
                accepting,
            } = state;

            match accepting {
                Some(AcceptingState {
                    value: _,
                    right_ctx,
                }) => match right_ctx {
                    Some(right_ctx_idx) => {
                        write!(f, "{:>4}", format!("*{}", state_idx),)?;
                        write!(f, " (ctx {})", right_ctx_idx.as_usize())?;
                    }
                    None => {
                        write!(f, "{:>4}", format!("*{}", state_idx))?;
                    }
                },
                None => {
                    write!(f, "{:>4}:", state_idx)?;
                }
            }

            let mut first = true;

            if !empty_transitions.is_empty() {
                if !first {
                    write!(f, "     ")?;
                } else {
                    first = false;
                }

                writeln!(f, "e -> {}", HashSetDisplay(empty_transitions))?;
            }

            for range in range_transitions.iter() {
                if !first {
                    write!(f, "     ")?;
                } else {
                    first = false;
                }

                if range.start == range.end {
                    writeln!(f, "{:?} -> {}", range.start, HashSetDisplay(&range.value))?;
                } else {
                    writeln!(
                        f,
                        "{:?} - {:?} -> {}",
                        range.start,
                        range.end,
                        HashSetDisplay(&range.value)
                    )?;
                }
            }

            if !any_transitions.is_empty() {
                if !first {
                    write!(f, "     ")?;
                } else {
                    first = false;
                }

                writeln!(f, "_ -> {}", HashSetDisplay(any_transitions))?;
            }

            if !end_of_input_transitions.is_empty() {
                if !first {
                    write!(f, "     ")?;
                }

                writeln!(f, "$ -> {}", HashSetDisplay(end_of_input_transitions))?;
            }

            if empty_transitions.is_empty()
                && range_transitions.is_empty()
                && any_transitions.is_empty()
                && end_of_input_transitions.is_empty()
            {
                writeln!(f)?;
            }
        }

        Ok(())
    }
}
