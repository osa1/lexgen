mod search_table;

use super::simplify::Trans;
use super::{State, StateIdx, DFA};
use search_table::SearchTableSet;

use crate::ast::{RuleKind, RuleRhs};
use crate::range_map::RangeMap;

use std::convert::TryFrom;

use fxhash::FxHashMap;
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};

// Max. size for guards in ranges. When a case have more ranges than this we generate a binary
// search table.
//
// Using binary search for large number of guards should be more efficient in runtime, but more
// importantly, when using builtin regexes like `$$uppercase` that has a lot of cases (see
// `char_ranges` module), rustc uses GiBs of RAM when compiling the generated code, even in debug
// mode. For example, the test `builtins` takes more than 32GiB of memory to compile.
//
// Binary search does less comparisons in the worst case when we have more than 3 cases, but the
// code for binary search is more complicated than a chain of `||`s, so I think it makes sense to
// have a slightly larger number here.
const MAX_GUARD_SIZE: usize = 9;

pub fn reify(
    dfa: DFA<Trans<RuleRhs>, RuleRhs>,
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

    let mut search_tables = SearchTableSet::new();

    let match_arms = generate_state_arms(
        dfa,
        &handle_type_name,
        &action_enum_name,
        user_error_type.as_ref(),
        &token_type,
        &mut search_tables,
    );

    let rule_name_enum_name = syn::Ident::new(&(type_name.to_string() + "Rule"), type_name.span());
    let rule_name_idents: Vec<syn::Ident> = rule_states
        .keys()
        .map(|rule_name| syn::Ident::new(rule_name, Span::call_site()))
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

    let binary_search_fn = if search_tables.is_empty() {
        quote!()
    } else {
        quote!(
            fn binary_search(c: char, table: &[(char, char)]) -> bool {
                table
                    .binary_search_by(|(start, end)| match c.cmp(start) {
                        std::cmp::Ordering::Greater => {
                            if c <= *end {
                                std::cmp::Ordering::Equal
                            } else {
                                std::cmp::Ordering::Less
                            }
                        }
                        std::cmp::Ordering::Equal => std::cmp::Ordering::Equal,
                        std::cmp::Ordering::Less => std::cmp::Ordering::Greater,
                    })
                    .is_ok()
            }
        )
    };

    let search_tables: Vec<TokenStream> = search_tables
        .iter()
        .map(|(ranges, ident)| {
            let n_ranges = ranges.len();
            let pairs: Vec<TokenStream> = ranges
                .iter()
                .map(|(start, end)| quote!((#start, #end)))
                .collect();
            quote!(
                static #ident: [(char, char); #n_ranges] = [
                    #(#pairs),*
                ];
            )
        })
        .collect();

    quote!(
        // Possible outcomes of a user action
        enum #action_enum_name<T> {
            // User action did not return a token, continue with lexing
            Continue,
            // User action returned a token, return it
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

        #(#search_tables)*
        #binary_search_fn

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
        let rule_ident = syn::Ident::new(rule_name, Span::call_site());
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
    dfa: DFA<Trans<RuleRhs>, RuleRhs>,
    handle_type_name: &syn::Ident,
    action_enum_name: &syn::Ident,
    user_error_type: Option<&syn::Type>,
    token_type: &syn::Type,
    search_tables: &mut SearchTableSet,
) -> Vec<TokenStream> {
    let DFA { states } = dfa;

    let mut match_arms: Vec<TokenStream> = vec![];

    let make_lexer_error = || -> TokenStream {
        match user_error_type {
            None => quote!(LexerError {
                char_idx: self.current_match_start
            }),
            Some(_) => quote!(LexerError::LexerError {
                char_idx: self.current_match_start
            }),
        }
    };

    let n_states = states.len();

    for (
        state_idx,
        State {
            initial,
            char_transitions,
            range_transitions,
            fail_transition,
            accepting,
        },
    ) in states.into_iter().enumerate()
    {
        let state_code: TokenStream = if state_idx == 0 {
            assert!(initial);

            // Initial state. Difference from other states is we return `None` when the
            // stream ends. In non-initial states EOS (end-of-stream) returns the last (longest)
            // match, or fails (error).
            let error = make_lexer_error();
            let action = quote!({
                return Some(Err(#error));
            });

            let state_char_arms = generate_state_char_arms(
                true,
                char_transitions,
                range_transitions,
                fail_transition,
                &action,
                search_tables,
                handle_type_name,
                action_enum_name,
                user_error_type,
                token_type,
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
                RuleRhs::None => quote!(
                    self.state = self.initial_state;
                ),
                RuleRhs::Rhs { expr, kind } => generate_semantic_action(
                    &expr,
                    kind,
                    handle_type_name,
                    action_enum_name,
                    user_error_type,
                    token_type,
                ),
            };

            if char_transitions.is_empty() && range_transitions.is_empty() {
                quote!({
                    #action
                })
            } else {
                let state_char_arms = generate_state_char_arms(
                    initial,
                    char_transitions,
                    range_transitions,
                    None,
                    &action,
                    search_tables,
                    handle_type_name,
                    action_enum_name,
                    user_error_type,
                    token_type,
                );

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
            // Non-initial, non-accepting state
            let error = make_lexer_error();
            let action = match &fail_transition {
                None => quote!(
                    return Some(Err(#error));
                ),
                Some(Trans::Trans(StateIdx(next))) => quote!(
                    self.state = #next;
                ),
                Some(Trans::Accept(action)) => match action {
                    RuleRhs::None => quote!(
                        self.state = self.initial_state;
                    ),
                    RuleRhs::Rhs { expr, kind } => generate_semantic_action(
                        expr,
                        *kind,
                        handle_type_name,
                        action_enum_name,
                        user_error_type,
                        token_type,
                    ),
                },
            };

            let state_char_arms = generate_state_char_arms(
                initial,
                char_transitions,
                range_transitions,
                fail_transition,
                &action,
                search_tables,
                handle_type_name,
                action_enum_name,
                user_error_type,
                token_type,
            );

            let end_of_stream_action = if initial {
                // In an initial state other than the state 0 we fail with "unexpected EOF"
                quote!(return Some(Err(#error));)
            } else {
                // Otherwise we run the semantic action and go to initial state of the current DFA.
                // Initial state will then fail.
                action
            };

            quote!(match self.iter.peek().copied() {
                None => {
                    #end_of_stream_action
                }
                Some((char_idx, char)) => {
                    match char {
                        #(#state_char_arms,)*
                    }
                }
            })
        };

        let state_idx_code = if state_idx == n_states - 1 {
            quote!(_)
        } else {
            quote!(#state_idx)
        };

        match_arms.push(quote!(
            #state_idx_code => #state_code
        ));
    }

    match_arms
}

/// Generate arms for `match self.iter.peek().copied() { ... }`
fn generate_state_char_arms(
    initial: bool,
    char_transitions: FxHashMap<char, Trans<RuleRhs>>,
    range_transitions: RangeMap<Trans<RuleRhs>>,
    fail_transition: Option<Trans<RuleRhs>>,
    fail_action: &TokenStream,
    search_tables: &mut SearchTableSet,
    handle_type_name: &syn::Ident,
    action_enum_name: &syn::Ident,
    user_error_type: Option<&syn::Type>,
    token_type: &syn::Type,
) -> Vec<TokenStream> {
    // Arms of the `match` for the current character
    let mut state_char_arms: Vec<TokenStream> = vec![];

    // Add char transitions. Collect characters for next states, to be able to use or
    // patterns in arms and reduce code size
    let mut state_chars: FxHashMap<StateIdx, Vec<char>> = Default::default();
    for (char, next) in char_transitions {
        match next {
            Trans::Accept(action) => {
                let action_code = generate_rhs_code(
                    &action,
                    handle_type_name,
                    action_enum_name,
                    user_error_type,
                    token_type,
                );
                state_char_arms.push(quote!(
                    #char => {
                        self.current_match_end += char.len_utf8();
                        let _ = self.iter.next();
                        #action_code
                    }
                ));
            }
            Trans::Trans(state_idx) => state_chars.entry(state_idx).or_default().push(char),
        }
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
    for mut range in range_transitions.into_iter() {
        assert_eq!(range.values.len(), 1);
        match range.values.remove(0) {
            Trans::Trans(state_idx) => state_ranges.entry(state_idx).or_default().push((
                char::try_from(range.start).unwrap(),
                char::try_from(range.end).unwrap(),
            )),
            Trans::Accept(action) => {
                let action_code = generate_rhs_code(
                    &action,
                    handle_type_name,
                    action_enum_name,
                    user_error_type,
                    token_type,
                );
                let range_start = char::from_u32(range.start).unwrap();
                let range_end = char::from_u32(range.end).unwrap();
                state_char_arms.push(quote!(
                    x if x >= #range_start && x <= #range_end => {
                        self.current_match_end += char.len_utf8();
                        let _ = self.iter.next();
                        #action_code
                    }
                ));
            }
        }
    }

    for (StateIdx(next_state), ranges) in state_ranges.into_iter() {
        let guard = if ranges.len() > MAX_GUARD_SIZE {
            let binary_search_table_id = search_tables.add_table(ranges);

            quote!(binary_search(x, &#binary_search_table_id))
        } else {
            let range_checks: Vec<TokenStream> = ranges
                .into_iter()
                .map(|(range_begin, range_end)| quote!((x >= #range_begin && x <= #range_end)))
                .collect();

            quote!(#(#range_checks)||*)
        };

        state_char_arms.push(quote!(
            x if #guard => {
                self.current_match_end += x.len_utf8();
                let _ = self.iter.next();
                self.state = #next_state;
            }
        ));
    }

    // Add default case
    let default_case = match fail_transition {
        None => quote!(_ => {
            #fail_action
        }),
        Some(Trans::Trans(StateIdx(next_state))) => {
            if initial {
                quote!(_ => {
                    self.current_match_end += char.len_utf8();
                    let _ = self.iter.next();
                    self.state = #next_state;
                })
            } else {
                quote!(_ => {
                    self.state = #next_state;
                })
            }
        }
        Some(Trans::Accept(action)) => {
            let action_code = generate_rhs_code(
                &action,
                handle_type_name,
                action_enum_name,
                user_error_type,
                token_type,
            );
            if initial {
                quote!(_ => {
                    self.current_match_end += char.len_utf8();
                    let _ = self.iter.next();
                    #action_code
                })
            } else {
                quote!(_ => {
                    #action_code
                })
            }
        }
    };

    state_char_arms.push(default_case);

    state_char_arms
}

// NB. Generates multiple states without enclosing `{...}`, see comments in
// `generate_semantic_action`
fn generate_rhs_code(
    action: &RuleRhs,
    handle_type_name: &syn::Ident,
    action_enum_name: &syn::Ident,
    user_error_type: Option<&syn::Type>,
    token_type: &syn::Type,
) -> TokenStream {
    match action {
        RuleRhs::None => quote!(
            self.state = self.initial_state;
        ),
        RuleRhs::Rhs { kind, expr } => generate_semantic_action(
            &expr,
            *kind,
            handle_type_name,
            action_enum_name,
            user_error_type,
            token_type,
        ),
    }
}

// NB. This function generates multiple statements but without enclosing `{...}`. Make sure to
// generate braces in the use site. This is to avoid redundant `{...}` in some cases (allows
// prepending/appending statements without creating new blocks).
fn generate_semantic_action(
    expr: &syn::Expr,
    kind: RuleKind,
    handle_type_name: &syn::Ident,
    action_enum_name: &syn::Ident,
    user_error_type: Option<&syn::Type>,
    token_type: &syn::Type,
) -> TokenStream {
    match kind {
        RuleKind::Simple => quote!(
            let rhs: #token_type = #expr;
            self.state = self.initial_state;
            return Some(Ok((self.current_match_start, rhs, self.current_match_end)));
        ),

        RuleKind::Fallible => {
            let user_error_type = match user_error_type {
                None => panic!(
                    "Fallible rules can only be used with a user error type, \
                            declared with `type Error = ...;` syntax"
                ),
                Some(user_error_type) => user_error_type,
            };
            quote!(
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
                    }
                    #action_enum_name::SwitchAndReturn(res, rule_set) => {
                        self.switch(rule_set);
                        return Some(match res {
                            Ok(tok) => Ok((self.current_match_start, tok, self.current_match_end)),
                            Err(err) => Err(LexerError::UserError(err)),
                        });
                    }
                }
            )
        }

        RuleKind::Infallible => quote!(
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
                }
                #action_enum_name::Return(tok) => {
                    self.state = self.initial_state;
                    return Some(Ok((self.current_match_start, tok, self.current_match_end)));
                }
                #action_enum_name::Switch(rule_set) => {
                    self.switch(rule_set);
                }
                #action_enum_name::SwitchAndReturn(tok, rule_set) => {
                    self.switch(rule_set);
                    return Some(Ok((self.current_match_start, tok, self.current_match_end)));
                }
            }
        ),
    }
}
