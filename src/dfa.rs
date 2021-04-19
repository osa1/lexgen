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
}

impl<A: Clone> DFA<A> {
    /// Extend the current DFA with another DFA. The extended DFA's states will be renumbered. This
    /// does not add any transitions from the original DFA states to the extension. Accepting
    /// states of the extension is preserved.
    ///
    /// Returns initial state for the extension in the new DFA.
    pub fn add_dfa(&mut self, other: &DFA<A>) -> StateIdx {
        let n_current_states = self.states.len();

        for state in &other.states {
            let mut char_transitions: FxHashMap<char, StateIdx> = Default::default();
            let mut range_transitions: FxHashMap<(char, char), StateIdx> = Default::default();

            for (char, next) in &state.char_transitions {
                char_transitions.insert(*char, StateIdx(next.0 + n_current_states));
            }

            for (range, next) in &state.range_transitions {
                range_transitions.insert(*range, StateIdx(next.0 + n_current_states));
            }

            self.states.push(State {
                char_transitions,
                range_transitions,
            });
        }

        for (idx, action) in &other.accepting {
            self.accepting
                .insert(StateIdx(idx.0 + n_current_states), action.clone());
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

pub fn reify(
    dfa: &DFA<Option<syn::Expr>>,
    rule_states: &FxHashMap<String, StateIdx>,
    type_name: syn::Ident,
    token_type: syn::Type,
) -> TokenStream {
    let pop_match: TokenStream = quote!(match self.pop_match_or_fail() {
        Ok(None) => continue,
        Ok(Some(tok)) => return Some(Ok(tok)),
        Err(err) => return Some(Err(err)),
    });

    let action_enum_name = syn::Ident::new(&(type_name.to_string() + "Action"), type_name.span());

    let match_arms = generate_state_arms(dfa, &pop_match, &action_enum_name);

    let rule_name_enum_name = syn::Ident::new(&(type_name.to_string() + "Rules"), type_name.span());
    let rule_name_idents: Vec<syn::Ident> = rule_states
        .keys()
        .map(|rule_name| syn::Ident::new(rule_name, proc_macro2::Span::call_site()))
        .collect();

    quote!(
        // Possible outcomes of a user action
        enum #action_enum_name {
            // User action did not return a token, continue with lexing
            Continue,
            // User action returned a token, add it to the match stack
            Return(#token_type),
            // User action requested switching to the given rule set
            Switch(#rule_name_enum_name),
        }

        // An enum for the rule sets in the DFA. `Init` is the initial, unnamed rule set.
        enum #rule_name_enum_name {
            Init,
            #(#rule_name_idents,)*
        }

        // The lexer type
        struct #type_name<'input> {
            state: usize,
            input: &'input str,
            iter: std::iter::Peekable<std::str::CharIndices<'input>>,
            match_stack: Vec<Option<(usize, #token_type, usize)>>,
            current_match_start: usize,
            current_match_end: usize,
        }

        #[derive(Debug, PartialEq, Eq)]
        struct LexerError {
            char_idx: usize,
        }

        impl<'input> #type_name<'input> {
            fn new(input: &'input str) -> Self {
                #type_name {
                    state: 0,
                    input,
                    iter: input.char_indices().peekable(),
                    match_stack: vec![],
                    current_match_start: 0,
                    current_match_end: 0,
                }
            }

            fn pop_match_or_fail(&mut self) -> Result<Option<(usize, #token_type, usize)>, LexerError> {
                match self.match_stack.pop() {
                    Some(None) => {
                        self.match_stack.clear();
                        self.state = 0;
                        Ok(None)
                    }
                    Some(Some(tok)) => {
                        self.match_stack.clear();
                        self.state = 0;
                        Ok(Some(tok))
                    }
                    None => Err(LexerError {
                        char_idx: self.current_match_start,
                    }),
                }
            }
        }

        impl<'input> Iterator for #type_name<'input> {
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

/// Generate arms of `match self.state { ... }` of a DFA.
fn generate_state_arms(
    dfa: &DFA<Option<syn::Expr>>,
    pop_match: &TokenStream,
    action_enum_name: &syn::Ident,
) -> Vec<TokenStream> {
    let DFA { states, accepting } = dfa;

    let mut match_arms: Vec<TokenStream> = vec![];

    for (
        state_idx,
        State {
            char_transitions,
            range_transitions,
        },
    ) in states.iter().enumerate()
    {
        let state_char_arms = generate_state_char_arms(
            accepting,
            state_idx,
            char_transitions,
            range_transitions,
            pop_match,
        );

        let accepting = accepting.get(&StateIdx(state_idx));

        let state_code: TokenStream = if state_idx == 0 {
            // Initial state. Difference from other states is we return `None` when the
            // iterator ends. In non-initial states EOF returns the last (longest) match, or
            // fails (error).
            quote!(
                match self.iter.next() {
                    None if self.match_stack.is_empty() => return None,
                    None => #pop_match,
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
            let push = match rhs {
                None => quote!(self.match_stack.push(None)),
                Some(rhs) => quote!(
                    match (#rhs)(str) {
                        #action_enum_name::Continue =>
                            self.match_stack.push(None),
                        #action_enum_name::Return(tok) =>
                            self.match_stack.push(Some((self.current_match_start, tok, self.current_match_end))),
                        #action_enum_name::Switch(rule_set) =>
                            todo!(),
                    }
                ),
            };
            quote!({
                let str = &self.input[self.current_match_start..self.current_match_end];
                #push;
                match self.iter.peek() {
                    None => #pop_match,
                    Some((char_idx, char)) => {
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
                None => #pop_match,
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

    match_arms
}

/// Generate arms on `match self.iter.next() { ... }` or `match self.iter.peek() { ... }` of DFA state.
fn generate_state_char_arms(
    accepting: &FxHashMap<StateIdx, Option<syn::Expr>>,
    state_idx: usize,
    char_transitions: &FxHashMap<char, StateIdx>,
    range_transitions: &FxHashMap<(char, char), StateIdx>,
    pop_match: &TokenStream,
) -> Vec<TokenStream> {
    // Arms of the `match` for the current character
    let mut state_char_arms: Vec<TokenStream> = vec![];
    let accepting = accepting.get(&StateIdx(state_idx));

    // Add char transitions. Collect characters for next states, to be able to use or
    // patterns in arms and reduce code size
    let mut state_chars: FxHashMap<StateIdx, Vec<char>> = Default::default();
    for (char, state_idx) in char_transitions {
        state_chars.entry(*state_idx).or_default().push(*char);
    }

    for (StateIdx(next_state), chars) in state_chars.iter() {
        let pat = quote!(#(#chars)|*);

        if accepting.is_some() {
            // In an accepting state we only consume the next character if we're making a
            // transition. See `state_code` below for where we use `peek` instead of `next`
            // in accepting states.
            state_char_arms.push(quote!(
                #pat => {
                    self.current_match_end += char.len_utf8();
                    let _ = self.iter.next();
                    self.state = #next_state;
                }
            ));
        } else {
            state_char_arms.push(quote!(
                #pat => self.state = #next_state
            ));
        }
    }

    // Add range transitions. Same as above, use chain of "or"s for ranges with same transition.
    let mut state_ranges: FxHashMap<StateIdx, Vec<(char, char)>> = Default::default();
    for (range, state_idx) in range_transitions {
        state_ranges.entry(*state_idx).or_default().push(*range);
    }

    for (StateIdx(next_state), mut ranges) in state_ranges.into_iter() {
        let x = if accepting.is_some() {
            quote!(*x)
        } else {
            quote!(x)
        };
        let guard = if ranges.len() == 1 {
            let (range_begin, range_end) = ranges.pop().unwrap();
            quote!(#x >= #range_begin && #x <= #range_end)
        } else {
            let (range_begin, range_end) = ranges.pop().unwrap();
            let mut guard = quote!(#x >= #range_begin && #x <= #range_end);
            while let Some((range_begin, range_end)) = ranges.pop() {
                guard = quote!((#x >= #range_begin && #x <= #range_end) || #guard);
            }
            guard
        };

        if accepting.is_some() {
            state_char_arms.push(quote!(
                x if #guard => {
                    self.current_match_end += x.len_utf8();
                    let _ = self.iter.next();
                    self.state = #next_state;
                }
            ));
        } else {
            state_char_arms.push(quote!(
                x if #guard => {
                    self.state = #next_state;
                }
            ));
        }
    }

    // Add default case
    state_char_arms.push(quote!(_ => #pop_match));

    state_char_arms
}
