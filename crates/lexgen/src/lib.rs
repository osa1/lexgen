//! Please see the [project README][1] for usage.
//!
//! [1]: https://github.com/osa1/lexgen

#![allow(
    clippy::collapsible_else_if,
    clippy::enum_variant_names,
    clippy::too_many_arguments,
    clippy::upper_case_acronyms,
    clippy::large_enum_variant
)]

mod ast;
mod builtin;
mod char_ranges;
mod collections;
mod dfa;
mod display;
mod nfa;
mod nfa_to_dfa;
mod range_map;
mod regex_to_nfa;
mod right_ctx;
mod semantic_action_table;

#[cfg(test)]
mod tests;

use ast::{Binding, Lexer, Regex, RegexCtx, Rule, RuleOrBinding, SingleRule, Var};
use collections::Map;
use dfa::{StateIdx as DfaStateIdx, DFA};
use nfa::NFA;
use nfa_to_dfa::nfa_to_dfa;
use right_ctx::RightCtxDFAs;
use semantic_action_table::{SemanticActionIdx, SemanticActionTable};

use std::collections::hash_map::Entry;

use proc_macro::TokenStream;
use syn::parse::Parser;

#[proc_macro]
pub fn lexer(input: TokenStream) -> TokenStream {
    let mut semantic_action_table = SemanticActionTable::new();

    let Lexer {
        attrs,
        visibility,
        type_name,
        user_state_type,
        token_type,
        rules: top_level_rules,
    } = match ast::make_lexer_parser(&mut semantic_action_table).parse(input) {
        Ok(lexer) => lexer,
        Err(error) => return TokenStream::from(error.to_compile_error()),
    };

    // Maps DFA names to their initial states in the final DFA
    let mut dfas: Map<String, dfa::StateIdx> = Default::default();

    // DFAs generated for right contexts
    let mut right_ctx_dfas = RightCtxDFAs::new();

    let mut bindings: Map<Var, Regex> = Default::default();

    let mut init_dfa: Option<DFA<DfaStateIdx, SemanticActionIdx>> = None;

    let mut user_error_type: Option<syn::Type> = None;

    let mut unnamed_nfa: NFA<SemanticActionIdx> = NFA::new();

    // Mixing named and unnamed rules is not allowed
    {
        let mut named = false;
        let mut unnamed = false;
        for rule in &top_level_rules {
            match rule {
                Rule::RuleOrBinding(RuleOrBinding::Rule { .. }) => unnamed = true,
                Rule::RuleSet { .. } => named = true,
                _ => {}
            }
        }
        if named && unnamed {
            panic!(
                "Unnamed rules cannot be mixed with named rules. Make sure to either \
                have all your rules in `rule ... {} ... {}` syntax, or remove `rule`s \
                entirely and have your rules at the top-level.",
                '{', '}',
            );
        }
    }

    for rule in top_level_rules {
        match rule {
            Rule::ErrorType { ty } => match user_error_type {
                None => {
                    user_error_type = Some(ty);
                }
                Some(_) => panic!("Error type defined multiple times"),
            },

            Rule::RuleOrBinding(RuleOrBinding::Binding(Binding { var, re })) => {
                match bindings.entry(var) {
                    Entry::Occupied(entry) => {
                        panic!("Variable {:?} is defined multiple times", entry.key().0);
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(re);
                    }
                }
            }

            Rule::RuleOrBinding(RuleOrBinding::Rule(SingleRule { lhs, rhs })) => {
                compile_single_rule(&mut unnamed_nfa, lhs, rhs, &bindings, &mut right_ctx_dfas);
            }

            Rule::RuleSet { name, rules } => {
                let dfa_idx = if name == "Init" {
                    let dfa = init_dfa.insert(compile_rule_set(
                        rules,
                        bindings.clone(),
                        &mut right_ctx_dfas,
                    ));

                    dfa.initial_state()
                } else {
                    let dfa = init_dfa
                        .as_mut()
                        .expect("First rule set should be named \"Init\"");

                    let dfa_ = compile_rule_set(rules, bindings.clone(), &mut right_ctx_dfas);

                    dfa.add_dfa(dfa_)
                };

                if dfas.insert(name.to_string(), dfa_idx).is_some() {
                    panic!("Rule set {:?} is defined multiple times", name.to_string());
                }
            }
        }
    }

    let dfa = match init_dfa {
        Some(init_dfa) => init_dfa,
        None => nfa_to_dfa(&unnamed_nfa),
    };

    let dfa = dfa::simplify::simplify(dfa, &mut dfas);

    dfa::codegen::reify(
        dfa,
        &right_ctx_dfas,
        semantic_action_table,
        user_state_type,
        user_error_type,
        dfas,
        type_name,
        token_type,
        visibility,
        attrs,
    )
    .into()
}

fn compile_single_rule(
    nfa: &mut NFA<SemanticActionIdx>,
    lhs: RegexCtx,
    rhs: SemanticActionIdx,
    bindings: &Map<Var, Regex>,
    right_ctx_dfas: &mut RightCtxDFAs<DfaStateIdx>,
) {
    let RegexCtx { re, right_ctx } = lhs;

    let right_ctx = right_ctx
        .as_ref()
        .map(|right_ctx| right_ctx_dfas.new_right_ctx(bindings, right_ctx));

    nfa.add_regex(bindings, &re, right_ctx, rhs);
}

fn compile_rule_set(
    rules: Vec<RuleOrBinding>,
    mut bindings: Map<Var, Regex>,
    right_ctx_dfas: &mut RightCtxDFAs<DfaStateIdx>,
) -> DFA<DfaStateIdx, SemanticActionIdx> {
    let mut nfa: NFA<SemanticActionIdx> = NFA::new();

    for rule in rules {
        match rule {
            RuleOrBinding::Rule(SingleRule { lhs, rhs }) => {
                compile_single_rule(&mut nfa, lhs, rhs, &bindings, right_ctx_dfas);
            }
            RuleOrBinding::Binding(Binding { var, re }) => match bindings.entry(var) {
                Entry::Occupied(entry) => {
                    panic!("Variable {:?} is defined multiple times", entry.key().0);
                }
                Entry::Vacant(entry) => {
                    entry.insert(re);
                }
            },
        }
    }

    nfa_to_dfa(&nfa)
}
