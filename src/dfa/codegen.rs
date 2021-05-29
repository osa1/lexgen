use super::{State, StateIdx, DFA};

use crate::ast::{RuleKind, RuleRhs};
use crate::range_map::RangeMap;

use std::convert::TryFrom;

use fxhash::FxHashMap;
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
            #[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
            fn switch_and_return<T>(self, rule: #rule_name_enum_name, token: T) -> #action_enum_name<T> {
                #action_enum_name::SwitchAndReturn(token, rule)
            }

            fn return_<T>(self, token: T) -> #action_enum_name<T> {
                #action_enum_name::Return(token)
            }

            fn switch<T>(self, rule: #rule_name_enum_name) -> #action_enum_name<T> {
                #action_enum_name::Switch(rule)
            }

            fn continue_<T>(self) -> #action_enum_name<T> {
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
                    // println!("state = {:?}, next char = {:?}", self.state, self.iter.peek());
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
    let DFA { states } = dfa;

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
            initial,
            char_transitions,
            range_transitions,
            fail_transition,
            accepting,
        },
    ) in states.iter().enumerate()
    {
        let state_code: TokenStream = if state_idx == 0 {
            assert!(*initial);

            // Initial state. Difference from other states is we return `None` when the
            // iterator ends. In non-initial states EOF returns the last (longest) match, or
            // fails (error).
            let error = make_lexer_error(quote!(self.current_match_start));
            let action = quote!({
                return Some(Err(#error));
            });

            let state_char_arms = generate_state_char_arms(
                true,
                char_transitions,
                range_transitions,
                fail_transition,
                &action,
            );

            quote!(
                match self.iter.peek().copied() {
                    None => return None,
                    Some((char_idx, char)) => {
                        self.current_match_start = char_idx;
                        self.current_match_end = char_idx;
                        match char {
                            #(#state_char_arms,)*
                        }
                    }
                }
            )
        } else if let Some(rhs) = accepting {
            // Non-initial, accepting state
            let action = match rhs {
                None => quote!({
                    self.state = self.initial_state;
                    continue;
                }),
                Some(rhs) => generate_semantic_action(
                    rhs,
                    handle_type_name,
                    action_enum_name,
                    user_error_type,
                    token_type,
                ),
            };

            let state_char_arms = generate_state_char_arms(
                *initial,
                char_transitions,
                range_transitions,
                fail_transition,
                &action,
            );

            if char_transitions.is_empty() && range_transitions.is_empty() {
                action
            } else {
                quote!({
                    match self.iter.peek().copied() {
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
                *initial,
                char_transitions,
                range_transitions,
                fail_transition,
                &action,
            );

            quote!(match self.iter.peek().copied() {
                None => return Some(Err(#error)),
                Some((char_idx, char)) => {
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

/// Generate arms on `match self.iter.next() { ... }` (for initial state) or `match
/// self.iter.peek().copied() { ... }` (for other states) of DFA state.
fn generate_state_char_arms(
    initial: bool,
    char_transitions: &FxHashMap<char, StateIdx>,
    range_transitions: &RangeMap<StateIdx>,
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

        state_char_arms.push(quote!(
            #pat => {
                self.current_match_end += char.len_utf8();
                let _ = self.iter.next();
                self.state = #next_state;
            }
        ));
    }

    // Add range transitions. Same as above, use chain of "or"s for ranges with same transition.
    let mut state_ranges: FxHashMap<StateIdx, Vec<(char, char)>> = Default::default();
    for range in range_transitions.iter() {
        assert_eq!(range.values.len(), 1);
        state_ranges.entry(range.values[0]).or_default().push((
            char::try_from(range.start).unwrap(),
            char::try_from(range.end).unwrap(),
        ));
    }

    for (StateIdx(next_state), ranges) in state_ranges.into_iter() {
        let range_checks: Vec<TokenStream> = ranges
            .into_iter()
            .map(|(range_begin, range_end)| quote!((x >= #range_begin && x <= #range_end)))
            .collect();

        let guard = quote!(#(#range_checks)||*);

        state_char_arms.push(quote!(
            x if #guard => {
                self.current_match_end += x.len_utf8();
                let _ = self.iter.next();
                self.state = #next_state;
            }
        ));
    }

    // Add default case
    match fail_transition {
        None => state_char_arms.push(quote!(_ => #action)),
        Some(StateIdx(next_state)) => {
            if initial {
                state_char_arms.push(quote!(_ => {
                    self.current_match_end += char.len_utf8();
                    let _ = self.iter.next();
                    self.state = #next_state;
                }));
            } else {
                state_char_arms.push(quote!(_ => { self.state = #next_state; }));
            }
        }
    }

    state_char_arms
}

fn generate_semantic_action(
    rhs: &RuleRhs,
    handle_type_name: &syn::Ident,
    action_enum_name: &syn::Ident,
    user_error_type: Option<&syn::Type>,
    token_type: &syn::Type,
) -> TokenStream {
    let RuleRhs { expr, kind } = rhs;

    match kind {
        RuleKind::Simple => quote!({
            let rhs: #token_type = #expr;
            self.state = self.initial_state;
            return Some(Ok((self.current_match_start, rhs, self.current_match_end)));
        }),

        RuleKind::Fallible => {
            let user_error_type = match user_error_type {
                None => panic!(
                    "Fallible rules can only be used with a user error type, \
                            declared with `type Error = ...;` syntax"
                ),
                Some(user_error_type) => user_error_type,
            };
            quote!({
                let rhs: fn(#handle_type_name<'_, 'input>) -> #action_enum_name<Result<#token_type, #user_error_type>> = #expr;

                let str = &self.input[self.current_match_start..self.current_match_end];
                let handle = #handle_type_name {
                    iter: &mut self.iter,
                    match_: str,
                    user_state: &mut self.user_state,
                };

                match rhs(handle) {
                    #action_enum_name::Continue => {
                        self.state = self.initial_state;
                        continue;
                    }
                    #action_enum_name::Return(res) => {
                        self.state = self.initial_state;
                        return Some(match res {
                            Ok(tok) => Ok((self.current_match_start, tok, self.current_match_end)),
                            Err(err) => Err(LexerError::UserError(err)),
                        });
                    }
                    #action_enum_name::Switch(rule_set) => {
                        self.switch(rule_set);
                        continue;
                    }
                    #action_enum_name::SwitchAndReturn(res, rule_set) => {
                        self.switch(rule_set);
                        return Some(match res {
                            Ok(tok) => Ok((self.current_match_start, tok, self.current_match_end)),
                            Err(err) => Err(LexerError::UserError(err)),
                        });
                    }
                }
            })
        }

        RuleKind::Infallible => quote!({
            let rhs: fn(#handle_type_name<'_, 'input>) -> #action_enum_name<#token_type> = #expr;

            let str = &self.input[self.current_match_start..self.current_match_end];
            let handle = #handle_type_name {
                iter: &mut self.iter,
                match_: str,
                user_state: &mut self.user_state,
            };

            match rhs(handle) {
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
            }
        }),
    }
}
