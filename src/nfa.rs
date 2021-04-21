use crate::ast::{Regex, Var};
use crate::regex_to_nfa;

use fxhash::{FxHashMap, FxHashSet};

/// Non-deterministic finite automate, parameterized on values of accepting states.
#[derive(Debug)]
pub struct NFA<A: Clone> {
    states: Vec<State>,
    accepting: FxHashMap<StateIdx, A>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StateIdx(usize);

#[derive(Debug)]
struct State {
    char_transitions: FxHashMap<char, FxHashSet<StateIdx>>,
    range_transitions: FxHashMap<(char, char), FxHashSet<StateIdx>>,
    fail_transitions: FxHashSet<StateIdx>,
    empty_transitions: FxHashSet<StateIdx>,
}

impl State {
    fn new() -> State {
        State {
            char_transitions: Default::default(),
            range_transitions: Default::default(),
            fail_transitions: Default::default(),
            empty_transitions: Default::default(),
        }
    }
}

impl<A: Clone> NFA<A> {
    pub fn new() -> NFA<A> {
        NFA {
            states: vec![State::new()],
            accepting: Default::default(),
        }
    }

    pub fn initial_state(&self) -> StateIdx {
        StateIdx(0)
    }

    pub fn get_accepting_state(&self, state: StateIdx) -> Option<A> {
        self.accepting.get(&state).cloned()
    }

    pub fn char_transitions(
        &self,
        state: StateIdx,
    ) -> impl Iterator<Item = (&char, &FxHashSet<StateIdx>)> {
        self.states[state.0].char_transitions.iter()
    }

    pub fn range_transitions(
        &self,
        state: StateIdx,
    ) -> impl Iterator<Item = (&(char, char), &FxHashSet<StateIdx>)> {
        self.states[state.0].range_transitions.iter()
    }

    pub fn fail_transitions(&self, state: StateIdx) -> impl Iterator<Item = &StateIdx> {
        self.states[state.0].fail_transitions.iter()
    }

    pub fn new_state(&mut self) -> StateIdx {
        let new_state_idx = StateIdx(self.states.len());
        self.states.push(State::new());
        new_state_idx
    }

    pub fn add_regex(&mut self, bindings: &FxHashMap<Var, Regex>, re: &Regex, value: A) {
        let re_accepting_state = self.new_state();
        self.make_accepting(re_accepting_state, value);

        let re_initial_state = self.new_state();
        let nfa_initial_state = self.initial_state();

        regex_to_nfa::add_re(self, bindings, re, re_initial_state, re_accepting_state);

        self.add_empty_transition(nfa_initial_state, re_initial_state);
    }

    pub fn add_char_transition(&mut self, state: StateIdx, char: char, next: StateIdx) {
        let not_exists = self.states[state.0]
            .char_transitions
            .entry(char)
            .or_insert(Default::default())
            .insert(next);

        assert!(not_exists, "add_char_transition");
    }

    pub fn add_range_transition(
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

    pub fn add_empty_transition(&mut self, state: StateIdx, next: StateIdx) {
        let not_exists = self.states[state.0].empty_transitions.insert(next);

        assert!(not_exists, "add_empty_transition");
    }

    pub fn add_fail_transition(&mut self, state: StateIdx, next: StateIdx) {
        let not_exists = self.states[state.0].fail_transitions.insert(next);

        assert!(not_exists, "add_fail_transition");
    }

    pub fn make_accepting(&mut self, state: StateIdx, value: A) {
        self.accepting.insert(state, value);
    }

    pub fn compute_state_closure(&self, states: &FxHashSet<StateIdx>) -> FxHashSet<StateIdx> {
        let mut worklist: Vec<StateIdx> = states.iter().copied().collect();
        let mut closure: FxHashSet<StateIdx> = states.clone();

        while let Some(work) = worklist.pop() {
            for next_state in self.next_empty_states(work) {
                if closure.insert(*next_state) {
                    worklist.push(*next_state);
                }
            }
        }

        closure
    }

    fn next_empty_states(&self, state: StateIdx) -> &FxHashSet<StateIdx> {
        let state = &self.states[state.0];
        &state.empty_transitions
    }
}

impl<A: Clone + std::fmt::Debug> NFA<A> {
    #[cfg(test)]
    pub fn simulate(&self, chars: &mut dyn Iterator<Item = char>) -> Option<&A> {
        let mut states: FxHashSet<StateIdx> = Default::default();
        states.insert(StateIdx(0));
        states = self.compute_state_closure(&states);

        for char in chars {
            println!("states before = {:?}", states);
            println!("char = {}", char);

            let mut next_states: FxHashSet<StateIdx> = Default::default();

            for state in &states {
                if let Some(char_nexts) = self.states[state.0].char_transitions.get(&char) {
                    next_states.extend(char_nexts.into_iter());
                    for (range, range_nexts) in &self.states[state.0].range_transitions {
                        if char >= range.0 && char <= range.1 {
                            next_states.extend(range_nexts.into_iter());
                        }
                    }
                }
                for ((range_begin, range_end), nexts) in &self.states[state.0].range_transitions {
                    if char >= *range_begin && char <= *range_end {
                        next_states.extend(nexts.into_iter());
                    }
                }
            }

            if next_states.is_empty() {
                for state in &states {
                    next_states.extend(self.states[state.0].fail_transitions.iter().copied());
                }
            }

            states = self.compute_state_closure(&mut next_states);
            println!("states after = {:?}", states);
        }

        let mut accepting_state_values: Vec<&A> = states
            .iter()
            .filter_map(|state| self.accepting.get(state))
            .collect();

        accepting_state_values.pop()
    }
}

use std::fmt::{self, Display, Formatter};

impl Display for StateIdx {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<A: Clone> Display for NFA<A> {
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
                fail_transitions,
                empty_transitions,
            } = state;

            let mut first = true;

            if !empty_transitions.is_empty() {
                if !first {
                    write!(f, "     ")?;
                } else {
                    first = false;
                }

                writeln!(f, "e -> {}", StateSetDisplay(empty_transitions))?;
            }

            if !fail_transitions.is_empty() {
                if !first {
                    write!(f, "     ")?;
                } else {
                    first = false;
                }

                writeln!(f, "_ -> {}", StateSetDisplay(fail_transitions))?;
            }

            for (char, next) in char_transitions.iter() {
                if !first {
                    write!(f, "     ")?;
                } else {
                    first = false;
                }

                writeln!(f, "{:?} -> {}", char, StateSetDisplay(next))?;
            }

            for ((range_begin, range_end), next) in range_transitions.iter() {
                if !first {
                    write!(f, "     ")?;
                } else {
                    first = false;
                }

                writeln!(
                    f,
                    "{:?} - {:?} -> {}",
                    range_begin,
                    range_end,
                    StateSetDisplay(next)
                )?;
            }

            if empty_transitions.is_empty()
                && char_transitions.is_empty()
                && range_transitions.is_empty()
                && fail_transitions.is_empty()
            {
                writeln!(f)?;
            }
        }

        Ok(())
    }
}

struct StateSetDisplay<'a>(&'a FxHashSet<StateIdx>);

impl Display for StateSetDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;

        let n_states = self.0.len();
        for (state_idx, state) in self.0.iter().enumerate() {
            write!(f, "{}", state.0)?;
            if state_idx != n_states - 1 {
                write!(f, ", ")?;
            }
        }

        write!(f, "}}")
    }
}
