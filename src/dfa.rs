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
                write!(f, "{:>4}:", format!("*{}", state_idx))?;
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

use proc_macro2::TokenStream;
use quote::quote;

impl DFA<syn::Expr> {
    pub fn reify(&self, token_type: syn::Type) -> TokenStream {
        let mut match_arms: Vec<TokenStream> = vec![];

        for (state_idx, state) in self.states.iter().enumerate() {
            let mut state_char_arms: Vec<TokenStream> = vec![];
            let accepting = self.accepting.get(&StateIdx(state_idx));

            // Add char transitions
            for (char, StateIdx(next_state)) in &state.char_transitions {
                if accepting.is_some() {
                    // In an accepting state we only consume the next character if we're making a
                    // transition. See `state_code` below for where we use `peek` instead of `next`
                    // in accepting states.
                    state_char_arms.push(quote!(
                        #char => {
                            let _ = self.iter.next();
                            self.state = #next_state;
                        }
                    ));
                } else {
                    state_char_arms.push(quote!(#char => self.state = #next_state));
                }
            }

            // Add default case
            state_char_arms.push(quote!(_ => return self.pop_match_or_fail()));

            let state_code: TokenStream = if state_idx == 0 {
                // Initial state. Difference from other states is we return `None` when the
                // iterator ends. In non-initial states EOF returns the last (longest) match, or
                // fails (error).
                quote!(
                    match self.iter.next() {
                        None if self.match_stack.is_empty() => return None,
                        None => return self.pop_match_or_fail(),
                        Some((char_idx, char)) => {
                            self.current_match_start = char_idx;
                            self.current_match_end = char_idx + char.len_utf8();
                            match char {
                                #(#state_char_arms,)*
                            }
                        }
                    }
                )
            } else if let Some(rhs) = accepting {
                // Non-initial, accepting state
                quote!({
                    let str = &self.input[self.current_match_start..self.current_match_end];
                    self.match_stack.push((self.current_match_start, (#rhs)(str), self.current_match_end));
                    match self.iter.peek() {
                        None => return self.pop_match_or_fail(),
                        Some((char_idx, char)) => {
                            self.current_match_end += char.len_utf8();
                            match char {
                                #(#state_char_arms,)*
                            }
                        }
                    }
                })
            } else {
                // Non-initial, non-accepting state. In a non-accepting state we want to consume a
                // character anyway so we can use `next` instead of `peek`.
                quote!(match self.iter.next() {
                    None => return self.pop_match_or_fail(),
                    Some((char_idx, char)) => {
                        self.current_match_end += char.len_utf8();
                        match char {
                            #(#state_char_arms,)*
                        }
                    }
                })
            };

            match_arms.push(quote!(
                    #state_idx => #state_code
            ));
        }

        match_arms.push(quote!(_ => unreachable!()));

        quote!(
            struct Lexer<'input> {
                state: usize,
                input: &'input str,
                iter: std::iter::Peekable<std::str::CharIndices<'input>>,
                match_stack: Vec<(usize, #token_type, usize)>,
                current_match_start: usize,
                current_match_end: usize,
            }

            #[derive(Debug)]
            struct LexerError {
                char_idx: usize,
            }

            impl<'input> Lexer<'input> {
                fn new(input: &'input str) -> Self {
                    Lexer {
                        state: 0,
                        input,
                        iter: input.char_indices().peekable(),
                        match_stack: vec![],
                        current_match_start: 0,
                        current_match_end: 0,
                    }
                }

                fn pop_match_or_fail(&mut self) -> Option<Result<(usize, #token_type, usize), LexerError>> {
                    match self.match_stack.pop() {
                        Some((match_begin, a, match_end)) => {
                            self.match_stack.clear();
                            self.state = 0;
                            Some(Ok((match_begin, a, match_end)))
                        }
                        None => Some(Err(LexerError {
                            char_idx: self.current_match_start,
                        })),
                    }
                }
            }

            impl<'input> Iterator for Lexer<'input> {
                type Item = Result<(usize, #token_type, usize), LexerError>;

                fn next(&mut self) -> Option<Self::Item> {
                    loop {
                        match self.state {
                            #(#match_arms,)*
                        }
                    }
                }
            }
        )
    }
}
