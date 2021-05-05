use crate::ast::{RuleKind, RuleRhs};

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
                fail_transition,
            } = state;

            let mut first = true;

            if let Some(next) = fail_transition {
                writeln!(f, "_ -> {}", next)?;
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

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

pub fn reify(
    dfa: &DFA<Option<RuleRhs>>,
    user_state_type: Option<syn::Type>,
    user_error_type: Option<syn::Type>,
    user_error_type_lifetimes: &[syn::Lifetime],
    rule_states: &FxHashMap<String, StateIdx>,
    type_name: syn::Ident,
    token_type: syn::Type,
    public: bool,
) -> TokenStream {
    let user_state_type = user_state_type
        .map(|ty| ty.into_token_stream())
        .unwrap_or(quote!(()));

    let action_enum_name = syn::Ident::new(&(type_name.to_string() + "Action"), type_name.span());

    let handle_type_name = syn::Ident::new(&(type_name.to_string() + "Handle"), type_name.span());

    let match_arms = generate_state_arms(
        dfa,
        &handle_type_name,
        &action_enum_name,
        user_error_type.as_ref(),
        &token_type,
    );

    let rule_name_enum_name = syn::Ident::new(&(type_name.to_string() + "Rule"), type_name.span());
    let rule_name_idents: Vec<syn::Ident> = rule_states
        .keys()
        .map(|rule_name| syn::Ident::new(rule_name, proc_macro2::Span::call_site()))
        .collect();

    let switch_method = generate_switch(&rule_name_enum_name, rule_states);

    let visibility = if public { quote!(pub) } else { quote!() };

    let lexer_error_type = match user_error_type {
        None => quote!(
            #[derive(Debug, PartialEq, Eq)]
            #visibility struct LexerError {
                char_idx: usize,
            }
        ),
        Some(user_error_type) => quote!(
            #[derive(Debug, PartialEq, Eq)]
            #visibility enum LexerError<#(#user_error_type_lifetimes),*> {
                LexerError { char_idx: usize },
                UserError(#user_error_type),
            }
        ),
    };

    quote!(
        // Possible outcomes of a user action
        enum #action_enum_name<T> {
            // User action did not return a token, continue with lexing
            Continue,
            // User action returned a token, add it to the match stack
            Return(T),
            // User action requested switching to the given rule set
            Switch(#rule_name_enum_name),
            // Combination or `Switch` and `Return`: add token to the match stack, switch to the
            // given rule set
            SwitchAndReturn(T, #rule_name_enum_name),
        }

        // An enum for the rule sets in the DFA. `Init` is the initial, unnamed rule set.
        #[derive(Clone, Copy)]
        enum #rule_name_enum_name {
            #(#rule_name_idents,)*
        }

        // The "handle" type passed to user actions. Allows getting the current match, modifying
        // user state, returning tokens, and switching to a different lexer state.
        struct #handle_type_name<'lexer, 'input> {
            iter: &'lexer mut std::iter::Peekable<std::str::CharIndices<'input>>,
            match_: &'input str,
            user_state: &'lexer mut #user_state_type,
        }

        // The lexer type
        #visibility struct #type_name<'input> {
            // Current lexer state
            state: usize,
            // Which lexer state to switch to on successful match
            initial_state: usize,
            user_state: #user_state_type,
            input: &'input str,
            iter: std::iter::Peekable<std::str::CharIndices<'input>>,
            current_match_start: usize,
            current_match_end: usize,
        }

        #lexer_error_type

        impl<'lexer, 'input> #handle_type_name<'lexer, 'input> {
            fn switch_and_return(self, rule: #rule_name_enum_name, token: #token_type) -> #action_enum_name<#token_type> {
                #action_enum_name::SwitchAndReturn(token, rule)
            }

            fn return_(self, token: #token_type) -> #action_enum_name<#token_type> {
                #action_enum_name::Return(token)
            }

            fn switch(self, rule: #rule_name_enum_name) -> #action_enum_name<#token_type> {
                #action_enum_name::Switch(rule)
            }

            fn continue_(self) -> #action_enum_name<#token_type> {
                #action_enum_name::Continue
            }

            fn state(&mut self) -> &mut #user_state_type {
                self.user_state
            }

            fn match_(&self) -> &'input str {
                self.match_
            }

            fn peek(&mut self) -> Option<char> {
                self.iter.peek().map(|(_, char)| *char)
            }
        }

        impl<'input> #type_name<'input> {
            #visibility fn new(input: &'input str) -> Self {
                Self::new_with_state(input, Default::default())
            }

            #visibility fn new_with_state(input: &'input str, user_state: #user_state_type) -> Self {
                #type_name {
                    state: 0,
                    initial_state: 0,
                    user_state: Default::default(),
                    input,
                    iter: input.char_indices().peekable(),
                    current_match_start: 0,
                    current_match_end: 0,
                }
            }

            #switch_method
        }

        impl<'input> Iterator for #type_name<'input> {
            type Item = Result<(usize, #token_type, usize), LexerError<#(#user_error_type_lifetimes),*>>;

            fn next(&mut self) -> Option<Self::Item> {
                loop {
                    // println!("state = {:?}, next char = {:?}, #matches = {}", self.state, self.iter.peek());
                    match self.state {
                        #(#match_arms,)*
                    }
                }
            }
        }
    )
}

fn generate_switch(
    enum_name: &syn::Ident,
    rule_states: &FxHashMap<String, StateIdx>,
) -> TokenStream {
    let mut arms: Vec<TokenStream> = vec![];

    for (rule_name, StateIdx(state_idx)) in rule_states.iter() {
        let rule_ident = syn::Ident::new(rule_name, proc_macro2::Span::call_site());
        arms.push(quote!(
            #enum_name::#rule_ident =>
                self.state = #state_idx
        ));
    }

    quote!(
        fn switch(&mut self, rule: #enum_name) {
            match rule {
                #enum_name::Init =>
                    self.state = 0,
                #(#arms,)*
            }
            self.initial_state = self.state;
        }
    )
}

/// Generate arms of `match self.state { ... }` of a DFA.
fn generate_state_arms(
    dfa: &DFA<Option<RuleRhs>>,
    handle_type_name: &syn::Ident,
    action_enum_name: &syn::Ident,
    user_error_type: Option<&syn::Type>,
    token_type: &syn::Type,
) -> Vec<TokenStream> {
    let DFA { states, accepting } = dfa;

    let mut match_arms: Vec<TokenStream> = vec![];

    let make_lexer_error = |arg: TokenStream| -> TokenStream {
        match user_error_type {
            None => quote!(LexerError { char_idx: #arg }),
            Some(_) => quote!(LexerError::LexerError { char_idx: #arg }),
        }
    };

    for (
        state_idx,
        State {
            char_transitions,
            range_transitions,
            fail_transition,
        },
    ) in states.iter().enumerate()
    {
        let accepting = accepting.get(&StateIdx(state_idx));

        let state_code: TokenStream = if state_idx == 0 {
            // Initial state. Difference from other states is we return `None` when the
            // iterator ends. In non-initial states EOF returns the last (longest) match, or
            // fails (error).
            let error = make_lexer_error(quote!(self.current_match_start));
            let action = quote!({
                return Some(Err(#error));
            });

            let state_char_arms = generate_state_char_arms(
                false,
                char_transitions,
                range_transitions,
                fail_transition,
                &action,
            );

            quote!(
                match self.iter.next() {
                    None => return None,
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
            let user_action_cases = quote!(
                #action_enum_name::Continue => {
                    self.state = self.initial_state;
                    continue;
                }
                #action_enum_name::Return(tok) => {
                    self.state = self.initial_state;
                    return Some(Ok((self.current_match_start, tok, self.current_match_end)));
                }
                #action_enum_name::Switch(rule_set) => {
                    self.switch(rule_set);
                    continue;
                }
                #action_enum_name::SwitchAndReturn(tok, rule_set) => {
                    self.switch(rule_set);
                    return Some(Ok((self.current_match_start, tok, self.current_match_end)));
                }
            );

            // Non-initial, accepting state
            let action = match rhs {
                None => quote!({
                    self.state = self.initial_state;
                    continue;
                }),
                Some(RuleRhs {
                    expr,
                    kind: RuleKind::Normal,
                }) => quote!({
                    let rhs: fn(#handle_type_name<'_, 'input>) -> #action_enum_name<#token_type> = #expr;

                    let str = &self.input[self.current_match_start..self.current_match_end];
                    let handle = #handle_type_name {
                        iter: &mut self.iter,
                        match_: str,
                        user_state: &mut self.user_state,
                    };

                    match rhs(handle) {
                        #user_action_cases
                    }
                }),
                Some(RuleRhs {
                    expr,
                    kind: RuleKind::Fallible,
                }) => {
                    let user_error_type = match user_error_type {
                        None => panic!(
                            "Fallible rules can only be used with a user error type, \
                            declared with `type Error = ...;` syntax"
                        ),
                        Some(user_error_type) => user_error_type,
                    };
                    quote!({
                        let rhs: fn(#handle_type_name<'_, 'input>) -> Result<#action_enum_name<#token_type>, #user_error_type> = #expr;

                        let str = &self.input[self.current_match_start..self.current_match_end];
                        let handle = #handle_type_name {
                            iter: &mut self.iter,
                            match_: str,
                            user_state: &mut self.user_state,
                        };

                        match rhs(handle) {
                            Err(user_error) => {
                                self.state = self.initial_state;
                                return Some(Err(LexerError::UserError(user_error)));
                            }
                            Ok(action) => {
                                match action {
                                    #user_action_cases
                                }
                            }
                        }
                    })
                }
                Some(RuleRhs {
                    expr,
                    kind: RuleKind::Simple,
                }) => quote!({
                    let rhs: #token_type = #expr;
                    self.state = self.initial_state;
                    return Some(Ok((self.current_match_start, rhs, self.current_match_end)));
                }),
            };

            let state_char_arms = generate_state_char_arms(
                true,
                char_transitions,
                range_transitions,
                fail_transition,
                &action,
            );

            if char_transitions.is_empty()
                && range_transitions.is_empty()
                && fail_transition.is_none()
            {
                action
            } else {
                quote!({
                    match self.iter.peek() {
                        None => {
                            #action
                        }
                        Some((char_idx, char)) => {
                            match char {
                                #(#state_char_arms,)*
                            }
                        }
                    }
                })
            }
        } else {
            // Non-initial, non-accepting state. In a non-accepting state we want to consume a
            // character anyway so we can use `next` instead of `peek`.
            let error = make_lexer_error(quote!(self.current_match_start));
            let action = quote!({
                return Some(Err(#error));
            });

            let state_char_arms = generate_state_char_arms(
                false,
                char_transitions,
                range_transitions,
                fail_transition,
                &action,
            );

            quote!(match self.iter.next() {
                None => return Some(Err(#error)),
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

/// Generate arms on `match self.iter.next() { ... }` (for non-matching states) or `match
/// self.iter.peek() { ... }` (for matching states) of DFA state.
fn generate_state_char_arms(
    accepting: bool,
    char_transitions: &FxHashMap<char, StateIdx>,
    range_transitions: &FxHashMap<(char, char), StateIdx>,
    fail_transition: &Option<StateIdx>,
    action: &TokenStream,
) -> Vec<TokenStream> {
    // Arms of the `match` for the current character
    let mut state_char_arms: Vec<TokenStream> = vec![];

    // Add char transitions. Collect characters for next states, to be able to use or
    // patterns in arms and reduce code size
    let mut state_chars: FxHashMap<StateIdx, Vec<char>> = Default::default();
    for (char, state_idx) in char_transitions {
        state_chars.entry(*state_idx).or_default().push(*char);
    }

    for (StateIdx(next_state), chars) in state_chars.iter() {
        let pat = quote!(#(#chars)|*);

        if accepting {
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
        let x = if accepting { quote!(*x) } else { quote!(x) };
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

        if accepting {
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
    match fail_transition {
        None => state_char_arms.push(quote!(_ => #action)),
        Some(StateIdx(next_state)) => {
            state_char_arms.push(quote!(_ => { self.state = #next_state; }))
        }
    }

    state_char_arms
}
