mod ctx;
mod search_table;

use ctx::CgCtx;

use super::simplify::Trans;
use super::{State, StateIdx, DFA};

use crate::ast::{RuleKind, RuleRhs};
use crate::range_map::RangeMap;
use crate::semantic_action_table::{SemanticActionIdx, SemanticActionTable};

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
    dfa: DFA<Trans, SemanticActionIdx>,
    semantic_actions: SemanticActionTable,
    user_state_type: Option<syn::Type>,
    user_error_type: Option<syn::Type>,
    user_error_type_lifetimes: Vec<syn::Lifetime>,
    rule_states: FxHashMap<String, StateIdx>,
    lexer_name: syn::Ident,
    token_type: syn::Type,
    public: bool,
) -> TokenStream {
    let rule_name_enum_name =
        syn::Ident::new(&(lexer_name.to_string() + "Rule"), lexer_name.span());

    let rule_name_idents: Vec<syn::Ident> = rule_states
        .keys()
        .map(|rule_name| syn::Ident::new(rule_name, Span::call_site()))
        .collect();

    let visibility = if public { quote!(pub) } else { quote!() };

    let lexer_error_type = match &user_error_type {
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

    let mut ctx = CgCtx::new(
        &dfa,
        semantic_actions,
        lexer_name,
        token_type,
        user_error_type,
        rule_states,
    );

    let user_state_type = user_state_type
        .map(|ty| ty.into_token_stream())
        .unwrap_or(quote!(()));

    let match_arms = generate_state_arms(&mut ctx, dfa);

    let switch_method = generate_switch(&ctx, &rule_name_enum_name);

    let search_tables = ctx.take_search_tables();

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

    let action_type_name = ctx.action_type_name();
    let lexer_name = ctx.lexer_name();
    let token_type = ctx.token_type();

    let semantic_action_fn_ret_ty = match ctx.user_error_type() {
        None => quote!(#action_type_name<#token_type>),
        Some(user_error_type) => quote!(#action_type_name<Result<#token_type, #user_error_type>>),
    };

    // Type of semantic actions, used to store the last match, to be used when backtracking
    let semantic_action_fn_type = quote!(for<'lexer, 'input> fn(&'lexer mut #lexer_name<'input>) -> #semantic_action_fn_ret_ty);

    let semantic_action_fns = generate_semantic_action_fns(&ctx, &semantic_action_fn_ret_ty);

    quote!(
        // Possible outcomes of a user action
        enum #action_type_name<T> {
            // User action did not return a token, continue with lexing
            Continue,
            // User action returned a token, return it
            Return(T),
        }

        impl<T> #action_type_name<T> {
            fn map_token<F, T1>(self, f: F) -> #action_type_name<T1>
                where
                    F: Fn(T) -> T1
            {
                match self {
                    #action_type_name::Continue =>
                        #action_type_name::Continue,
                    #action_type_name::Return(t) =>
                        #action_type_name::Return(f(t)),
                }
            }
        }

        // An enum for the rule sets in the DFA. `Init` is the initial, unnamed rule set.
        #[derive(Clone, Copy)]
        enum #rule_name_enum_name {
            #(#rule_name_idents,)*
        }

        // The lexer type
        // NB. 'input lifetime used in `semantic_action_fn_type`
        #visibility struct #lexer_name<'input_> {
            // Current lexer state
            __state: usize,

            // Set after end-of-input is handled by a rule, or by default in `Init` rule
            __done: bool,

            // Which lexer state to switch to on successful match
            __initial_state: usize,

            __user_state: #user_state_type,

            // User-provided input string. Does not change after initialization.
            __input: &'input_ str,

            // Start index of `iter`. We update this as we backtrack and update `iter`.
            __iter_byte_idx: usize,

            // Character iterator. `Peekable` is used in the handler's `peek` method. Note that we
            // can't use byte index returned by this directly, as we re-initialize this field when
            // backtracking. Add `iter_byte_idx` to the byte index before using. When resetting,
            // update `iter_byte_idx`.
            __iter: std::iter::Peekable<std::str::CharIndices<'input_>>,

            // Where does the current match start. Byte index in `input`.
            __current_match_start: usize,

            // Where does the current match end (exclusive). Byte index in `input`.
            __current_match_end: usize,

            // If we skipped an accepting state, this holds the triple:
            // - Skipped match start (byte index in `input`)
            // - Semantic action (a function name)
            // - Skipped match end (exclusive, byte index in `input`)
            __last_match: Option<(usize, #semantic_action_fn_type, usize)>,
        }

        #lexer_error_type

        // Methods below for using in semantic actions
        impl<'input> #lexer_name<'input> {
            fn switch_and_return<T>(&mut self, rule: #rule_name_enum_name, token: T) -> #action_type_name<T> {
                self.switch::<T>(rule);
                #action_type_name::Return(token)
            }

            fn return_<T>(&self, token: T) -> #action_type_name<T> {
                #action_type_name::Return(token)
            }

            #switch_method

            fn continue_<T>(&self) -> #action_type_name<T> {
                #action_type_name::Continue
            }

            fn state(&mut self) -> &mut #user_state_type {
                &mut self.__user_state
            }

            fn reset_match(&mut self) {
                self.__current_match_start = self.__current_match_end;
            }

            fn match_(&self) -> &'input str {
                &self.__input[self.__current_match_start..self.__current_match_end]
            }

            fn peek(&mut self) -> Option<char> {
                self.__iter.peek().map(|(_, char)| *char)
            }
        }

        impl<'input> #lexer_name<'input> {
            #visibility fn new(input: &'input str) -> Self {
                Self::new_with_state(input, Default::default())
            }

            #visibility fn new_with_state(input: &'input str, user_state: #user_state_type) -> Self {
                #lexer_name {
                    __state: 0,
                    __done: false,
                    __initial_state: 0,
                    __user_state: Default::default(),
                    __input: input,
                    __iter_byte_idx: 0,
                    __iter: input.char_indices().peekable(),
                    __current_match_start: 0,
                    __current_match_end: 0,
                    __last_match: None,
                }
            }
        }

        #(#search_tables)*
        #binary_search_fn
        #semantic_action_fns

        impl<'input> Iterator for #lexer_name<'input> {
            type Item = Result<(usize, #token_type, usize), LexerError<#(#user_error_type_lifetimes),*>>;

            fn next(&mut self) -> Option<Self::Item> {
                loop {
                    if self.__done {
                        return None;
                    }

                    // println!("state = {:?}, next char = {:?}", self.__state, self.__iter.peek());
                    match self.__state {
                        #(#match_arms,)*
                    }
                }
            }
        }
    )
}

fn generate_switch(ctx: &CgCtx, enum_name: &syn::Ident) -> TokenStream {
    let action_type_name = ctx.action_type_name();
    let mut arms: Vec<TokenStream> = vec![];

    for (rule_name, state_idx) in ctx.rule_states().iter() {
        let StateIdx(state_idx) = ctx.renumber_state(*state_idx);
        let rule_ident = syn::Ident::new(rule_name, Span::call_site());
        arms.push(quote!(
            #enum_name::#rule_ident =>
                self.__state = #state_idx
        ));
    }

    quote!(
        fn switch<A>(&mut self, rule: #enum_name) -> #action_type_name<A> {
            match rule {
                #(#arms,)*
            }
            self.__initial_state = self.__state;
            #action_type_name::Continue
        }
    )
}

/// Generate arms of `match self.__state { ... }` of a DFA.
fn generate_state_arms(ctx: &mut CgCtx, dfa: DFA<Trans, SemanticActionIdx>) -> Vec<TokenStream> {
    let DFA { states } = dfa;

    let mut match_arms: Vec<TokenStream> = vec![];

    let n_states = states.len();

    for (state_idx, state) in states.iter().enumerate() {
        if state.predecessors.len() == 1 && !state.initial {
            continue;
        }

        let state_code: TokenStream = generate_state_arm(ctx, state_idx, state, &states);

        let StateIdx(state_idx) = ctx.renumber_state(StateIdx(state_idx));
        let state_idx_pat = if state_idx == n_states - ctx.n_inlined_states() - 1 {
            quote!(_)
        } else {
            quote!(#state_idx)
        };

        match_arms.push(quote!(
            #state_idx_pat => { #state_code }
        ));
    }

    match_arms
}

// NB. Does not generate braces around the code
fn generate_state_arm(
    ctx: &mut CgCtx,
    state_idx: usize,
    state: &State<Trans, SemanticActionIdx>,
    states: &[State<Trans, SemanticActionIdx>],
) -> TokenStream {
    let State {
        initial,
        char_transitions,
        range_transitions,
        any_transition,
        end_of_input_transition,
        accepting,
        predecessors: _,
    } = state;

    let error = if ctx.user_error_type().is_some() {
        quote!(LexerError::LexerError {
            char_idx: self.__current_match_start
        })
    } else {
        quote!(LexerError {
            char_idx: self.__current_match_start
        })
    };

    // TODO: This duplicates a lot of code. Maybe implement a __backtrack method?
    let fail = |ctx: &CgCtx| -> TokenStream {
        let action = generate_semantic_action_call(ctx, &quote!(semantic_action));
        quote!(match self.__last_match.take() {
            None => return Some(Err(#error)),
            Some((match_start, semantic_action, match_end)) => {
                self.__done = false;
                self.__current_match_start = match_start;
                self.__current_match_end = match_end;
                self.__iter = self.__input[match_end..].char_indices().peekable();
                self.__iter_byte_idx = match_end;
                #action
            }
        })
    };

    // When we can't take char or range transitions, take the 'any' transition if it exists, or
    // fail (backtrack or raise error)
    let default_action = any_transition
        .as_ref()
        .map(|any_transition| generate_any_transition(ctx, states, any_transition))
        .unwrap_or_else(|| fail(ctx));

    let state_char_arms = generate_state_char_arms(
        ctx,
        states,
        char_transitions,
        range_transitions,
        &default_action,
    );

    // In initial state (rule `Init`) unhandled end-of-input yields `None`. In other states we
    // expect to see a end-of-input handler, or fail with "unexpected end-of-input".
    let end_of_input_default_action = if state_idx == 0 {
        quote!(return None;)
    } else {
        fail(ctx)
    };

    let end_of_input_action = match end_of_input_transition {
        Some(end_of_input_transition) => match end_of_input_transition {
            Trans::Accept(action) => generate_rhs_code(ctx, *action),
            Trans::Trans(next_state) => {
                let StateIdx(next_state) = ctx.renumber_state(*next_state);
                quote!(self.__state = #next_state;)
            }
        },
        None => end_of_input_default_action,
    };

    let end_of_input_action = quote!(
        self.__done = true; // don't handle end-of-input again
        #end_of_input_action
    );

    if state_idx == 0 {
        assert!(initial);

        // See #12 for the special case in state 0 (rule Init)
        quote!(
            match self.__iter.next() {
                None => {
                    #end_of_input_action
                }
                Some((char_idx, char)) => {
                    let char_idx = self.__iter_byte_idx + char_idx;
                    self.__current_match_start = char_idx;
                    self.__current_match_end = char_idx;
                    match char {
                        #(#state_char_arms,)*
                    }
                }
            }
        )
    } else if let Some(rhs) = accepting {
        // Accepting state
        let semantic_fn = rhs.symbol();

        quote!(
            self.__last_match =
                Some((self.__current_match_start, #semantic_fn, self.__current_match_end));

            match self.__iter.next() {
                None => {
                    #end_of_input_action
                }
                Some((char_idx, char)) => {
                    match char {
                        #(#state_char_arms,)*
                    }
                }
            }
        )
    } else {
        // Non-accepting state
        quote!(match self.__iter.next() {
            None => {
                #end_of_input_action
            }
            Some((_, char)) => {
                match char {
                    #(#state_char_arms,)*
                }
            }
        })
    }
}

fn generate_any_transition(
    ctx: &mut CgCtx,
    states: &[State<Trans, SemanticActionIdx>],
    trans: &Trans,
) -> TokenStream {
    let action = match trans {
        Trans::Trans(StateIdx(next_state)) => {
            if states[*next_state].predecessors.len() == 1 {
                generate_state_arm(ctx, *next_state, &states[*next_state], states)
            } else {
                let StateIdx(next_state) = ctx.renumber_state(StateIdx(*next_state));
                quote!(self.__state = #next_state;)
            }
        }

        Trans::Accept(action) => generate_rhs_code(ctx, *action),
    };

    quote!(
        self.__current_match_end += char.len_utf8();
        #action
    )
}

/// Generate arms for `match char { ... }`
fn generate_state_char_arms(
    ctx: &mut CgCtx,
    states: &[State<Trans, SemanticActionIdx>],
    char_transitions: &FxHashMap<char, Trans>,
    range_transitions: &RangeMap<Trans>,
    // RHS of the default alternative for this `match` (_ => <default_rhs>)
    default_rhs: &TokenStream,
) -> Vec<TokenStream> {
    // Arms of the `match` for the current character
    let mut state_char_arms: Vec<TokenStream> = vec![];

    // Add char transitions. Collect characters for next states, to be able to use or
    // patterns in arms and reduce code size
    let mut state_chars: FxHashMap<StateIdx, Vec<char>> = Default::default();
    for (char, next) in char_transitions {
        match next {
            Trans::Accept(action) => {
                let action_code = generate_rhs_code(ctx, *action);
                state_char_arms.push(quote!(
                    #char => {
                        self.__current_match_end += char.len_utf8();
                        #action_code
                    }
                ));
            }
            Trans::Trans(state_idx) => state_chars.entry(*state_idx).or_default().push(*char),
        }
    }

    for (StateIdx(next_state), chars) in state_chars.iter() {
        let pat = quote!(#(#chars)|*);

        let next = if states[*next_state].predecessors.len() == 1 {
            generate_state_arm(ctx, *next_state, &states[*next_state], states)
        } else {
            let StateIdx(next_state) = ctx.renumber_state(StateIdx(*next_state));
            quote!(
                self.__state = #next_state;
            )
        };

        state_char_arms.push(quote!(
            #pat => {
                self.__current_match_end += char.len_utf8();
                #next
            }
        ));
    }

    // Add range transitions. Same as above, use chain of "or"s for ranges with same transition.
    let mut state_ranges: FxHashMap<StateIdx, Vec<(char, char)>> = Default::default();
    for range in range_transitions.iter() {
        assert_eq!(range.values.len(), 1);
        match &range.values[0] {
            Trans::Trans(state_idx) => state_ranges.entry(*state_idx).or_default().push((
                char::try_from(range.start).unwrap(),
                char::try_from(range.end).unwrap(),
            )),
            Trans::Accept(action) => {
                let action_code = generate_rhs_code(ctx, *action);
                let range_start = char::from_u32(range.start).unwrap();
                let range_end = char::from_u32(range.end).unwrap();
                state_char_arms.push(quote!(
                    x if x >= #range_start && x <= #range_end => {
                        self.__current_match_end += char.len_utf8();
                        #action_code
                    }
                ));
            }
        }
    }

    for (StateIdx(next_state), ranges) in state_ranges.into_iter() {
        let guard = if ranges.len() > MAX_GUARD_SIZE {
            let binary_search_table_id = ctx.add_search_table(ranges);

            quote!(binary_search(x, &#binary_search_table_id))
        } else {
            let range_checks: Vec<TokenStream> = ranges
                .into_iter()
                .map(|(range_begin, range_end)| quote!((x >= #range_begin && x <= #range_end)))
                .collect();

            quote!(#(#range_checks)||*)
        };

        let next = if states[next_state].predecessors.len() == 1 {
            generate_state_arm(ctx, next_state, &states[next_state], states)
        } else {
            let StateIdx(next_state) = ctx.renumber_state(StateIdx(next_state));
            quote!(
                self.__state = #next_state;
            )
        };

        state_char_arms.push(quote!(
            x if #guard => {
                self.__current_match_end += x.len_utf8();
                #next
            }
        ));
    }

    state_char_arms.push(quote!(_ => { #default_rhs }));

    state_char_arms
}

/// Generate call to the semantic action function with the given index and handle the result.
fn generate_rhs_code(ctx: &CgCtx, action: SemanticActionIdx) -> TokenStream {
    generate_semantic_action_call(ctx, &action.symbol().into_token_stream())
}

/// Generate call to the given semantic action function and handle the result.
fn generate_semantic_action_call(ctx: &CgCtx, action_fn: &TokenStream) -> TokenStream {
    generate_action_result_handler(ctx, quote!(#action_fn(self)))
}

fn generate_action_result_handler(ctx: &CgCtx, action_result: TokenStream) -> TokenStream {
    let action_type_name = ctx.action_type_name();
    let has_user_error = ctx.user_error_type().is_some();

    let map_res = if has_user_error {
        quote!(match res {
            Ok(tok) => Ok((match_start, tok, self.__current_match_end)),
            Err(err) => Err(LexerError::UserError(err)),
        })
    } else {
        quote!(Ok((match_start, res, self.__current_match_end)))
    };

    quote!(match #action_result {
        #action_type_name::Continue => {
            self.__state = self.__initial_state;
        }
        #action_type_name::Return(res) => {
            self.__state = self.__initial_state;
            let match_start = self.__current_match_start;
            self.__current_match_start = self.__current_match_end;
            return Some(#map_res);
        }
    })
}

fn generate_semantic_action_fns(
    ctx: &CgCtx,
    semantic_action_fn_ret_ty: &TokenStream,
) -> TokenStream {
    let lexer_name = ctx.lexer_name();
    let token_type = ctx.token_type();
    let user_error_type = ctx.user_error_type();
    let action_type_name = ctx.action_type_name();

    let fns: Vec<TokenStream> = ctx
        .iter_semantic_actions()
        .map(|(idx, action)| {
            let ident = idx.symbol();

            let rhs = match action {
                RuleRhs::None => {
                    if user_error_type.is_some() {
                        quote!(|__lexer: &mut #lexer_name| __lexer.continue_().map_token(Ok))
                    } else {
                        quote!(|__lexer: &mut #lexer_name| __lexer.continue_())
                    }
                }

                RuleRhs::Rhs { expr, kind } => {
                    match kind {
                        RuleKind::Simple => {
                            if user_error_type.is_some() {
                                quote!(|__lexer: &mut #lexer_name| __lexer.return_(#expr).map_token(Ok))
                            } else {
                                quote!(|__lexer: &mut #lexer_name| __lexer.return_(#expr))
                            }
                        }
                        RuleKind::Fallible => quote!(#expr),
                        RuleKind::Infallible => {
                            if user_error_type.is_some() {
                                quote!(|__lexer: &mut #lexer_name| {
                                    let semantic_action:
                                        for<'lexer, 'input> fn(&'lexer mut #lexer_name<'input>) -> #action_type_name<#token_type> =
                                            #expr;

                                    semantic_action(__lexer).map_token(Ok)
                                })
                            } else {
                                quote!(#expr)
                            }
                        }
                    }
                }
            };

            quote!(
                static #ident: for<'lexer, 'input> fn(&'lexer mut #lexer_name<'input>) -> #semantic_action_fn_ret_ty =
                    #rhs;
            )
        })
        .collect();

    quote!(#(#fns)*)
}
