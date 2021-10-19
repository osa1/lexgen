//! Please see the [project README][1] for usage.
//!
//! [1]: https://github.com/osa1/lexgen

mod ast;
mod builtin;
mod char_ranges;
mod dfa;
mod display;
mod nfa;
mod nfa_to_dfa;
mod number_semantic_actions;
mod range_map;
mod regex_to_nfa;
mod semantic_action_table;

#[cfg(test)]
mod tests;

use ast::{Lexer, Regex, Rule, RuleLhs, RuleRhs, SingleRule, Var};
use dfa::{StateIdx as DfaStateIdx, DFA};
use nfa::NFA;
use nfa_to_dfa::nfa_to_dfa;

use std::collections::hash_map::Entry;

use fxhash::FxHashMap;
use proc_macro::TokenStream;

#[proc_macro]
pub fn lexer(input: TokenStream) -> TokenStream {
    let Lexer {
        public,
        type_name,
        user_state_type,
        token_type,
        rules: top_level_rules,
    } = syn::parse_macro_input!(input as Lexer<RuleRhs>);

    // Maps DFA names to their initial states in the final DFA
    let mut dfas: FxHashMap<String, dfa::StateIdx> = Default::default();

    let mut bindings: FxHashMap<Var, Regex> = Default::default();

    let mut dfa: Option<DFA<DfaStateIdx, RuleRhs>> = None;

    let mut user_error_type: Option<syn::Type> = None;
    let mut user_error_lifetimes: Vec<syn::Lifetime> = vec![];

    let have_named_rules = top_level_rules
        .iter()
        .any(|rule| matches!(rule, Rule::RuleSet { .. }));

    for rule in top_level_rules {
        match rule {
            Rule::Binding { var, re } => match bindings.entry(var) {
                Entry::Occupied(entry) => {
                    panic!("Variable {:?} is defined multiple times", entry.key().0);
                }
                Entry::Vacant(entry) => {
                    entry.insert(re);
                }
            },
            Rule::RuleSet { name, rules } => {
                if name == "Init" {
                    if dfa.is_some() {
                        panic!("\"Init\" rule set can only be defined once");
                    }
                    let mut nfa: NFA<RuleRhs> = NFA::new();
                    for SingleRule { lhs, rhs } in rules {
                        match lhs {
                            RuleLhs::Regex(re) => nfa.add_regex(&bindings, &re, rhs),
                            RuleLhs::Fail => nfa.set_fail_action(rhs),
                        }
                    }

                    // println!("NFA=\n{}", nfa);

                    let dfa_ = nfa_to_dfa(&nfa);
                    let initial_state = dfa_.initial_state();

                    // println!("DFA=\n{}", dfa_);

                    dfa = Some(dfa_);
                    if dfas.insert(name.to_string(), initial_state).is_some() {
                        panic!("Rule set {:?} is defined multiple times", name.to_string());
                    }
                } else {
                    let dfa = match dfa.as_mut() {
                        None => panic!("First rule set should be named \"Init\""),
                        Some(dfa) => dfa,
                    };
                    let mut nfa: NFA<RuleRhs> = NFA::new();

                    for SingleRule { lhs, rhs } in rules {
                        match lhs {
                            RuleLhs::Regex(re) => nfa.add_regex(&bindings, &re, rhs),
                            RuleLhs::Fail => nfa.set_fail_action(rhs),
                        }
                    }

                    // println!("NFA=\n{}", nfa);

                    let dfa_ = nfa_to_dfa(&nfa);
                    // println!("DFA=\n{}", dfa_);
                    let dfa_idx = dfa.add_dfa(dfa_);

                    if dfas.insert(name.to_string(), dfa_idx).is_some() {
                        panic!("Rule set {:?} is defined multiple times", name.to_string());
                    }
                }
            }
            Rule::UnnamedRules { rules } => {
                if dfa.is_some() || have_named_rules {
                    panic!(
                        "Unnamed rules cannot be mixed with named rules. Make sure to either \
                        have all your rules in `rule ... {} ... {}` syntax, or remove `rule`s \
                        entirely and have your rules at the top-level.",
                        '{', '}',
                    );
                }

                let mut nfa: NFA<RuleRhs> = NFA::new();
                for SingleRule { lhs, rhs } in rules {
                    match lhs {
                        RuleLhs::Regex(re) => nfa.add_regex(&bindings, &re, rhs),
                        RuleLhs::Fail => nfa.set_fail_action(rhs),
                    }
                }

                // println!("NFA=\n{}", nfa);

                let dfa_ = nfa_to_dfa(&nfa);

                // println!("DFA=\n{}", dfa_);

                let initial_state = dfa_.initial_state();

                dfa = Some(dfa_);
                dfas.insert("Init".to_owned(), initial_state);
            }
            Rule::ErrorType { ty, lifetimes } => match user_error_type {
                None => {
                    user_error_type = Some(ty);
                    user_error_lifetimes = lifetimes;
                }
                Some(_) => panic!("Error type defined multiple times"),
            },
        }
    }

    // println!("Final DFA=\n{}", dfa.as_ref().unwrap());

    // There should be a rule with name "Init"
    if dfas.get("Init").is_none() {
        panic!(
            "There should be a rule set named \"Init\". Current rules: {:?}",
            dfas.keys().collect::<Vec<&String>>()
        );
    }

    let (dfa, semantic_action_table) =
        number_semantic_actions::number_semantic_actions(dfa.unwrap());

    let dfa = dfa::simplify::simplify(dfa, &mut dfas);

    dfa::codegen::reify(
        dfa,
        semantic_action_table,
        user_state_type,
        user_error_type,
        user_error_lifetimes,
        dfas,
        type_name,
        token_type,
        public,
    )
    .into()
}
