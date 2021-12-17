//! Please see the [project README][1] for usage.
//!
//! [1]: https://github.com/osa1/lexgen

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

use ast::{Lexer, Regex, RegexCtx, Rule, SingleRule, Var};
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
        public,
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

    let mut dfa: Option<DFA<DfaStateIdx, SemanticActionIdx>> = None;

    let mut user_error_type: Option<syn::Type> = None;

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
                    // TODO: Check that regex doesn't have right context
                    entry.insert(re.re);
                }
            },
            Rule::RuleSet { name, rules } => {
                if name == "Init" {
                    let dfa = dfa.insert(compile_rules(rules, &bindings, &mut right_ctx_dfas));
                    let initial_state = dfa.initial_state();

                    if dfas.insert(name.to_string(), initial_state).is_some() {
                        panic!("Rule set {:?} is defined multiple times", name.to_string());
                    }
                } else {
                    let dfa = dfa
                        .as_mut()
                        .expect("First rule set should be named \"Init\"");

                    let dfa_ = compile_rules(rules, &bindings, &mut right_ctx_dfas);

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

                let dfa = dfa.insert(compile_rules(rules, &bindings, &mut right_ctx_dfas));
                let initial_state = dfa.initial_state();
                dfas.insert("Init".to_owned(), initial_state);
            }
            Rule::ErrorType { ty } => match user_error_type {
                None => {
                    user_error_type = Some(ty);
                }
                Some(_) => panic!("Error type defined multiple times"),
            },
        }
    }

    // There should be a rule with name "Init"
    if dfas.get("Init").is_none() {
        panic!(
            "There should be a rule set named \"Init\". Current rules: {:?}",
            dfas.keys().collect::<Vec<&String>>()
        );
    }

    let dfa = dfa::simplify::simplify(dfa.unwrap(), &mut dfas);

    dfa::codegen::reify(
        dfa,
        &right_ctx_dfas,
        semantic_action_table,
        user_state_type,
        user_error_type,
        dfas,
        type_name,
        token_type,
        public,
    )
    .into()
}

fn compile_rules(
    rules: Vec<SingleRule>,
    bindings: &Map<Var, Regex>,
    right_ctx_dfas: &mut RightCtxDFAs<DfaStateIdx>,
) -> DFA<DfaStateIdx, SemanticActionIdx> {
    let mut nfa: NFA<SemanticActionIdx> = NFA::new();

    for SingleRule { lhs, rhs } in rules {
        let RegexCtx { re, right_ctx } = lhs;

        let right_ctx = right_ctx
            .as_ref()
            .map(|right_ctx| right_ctx_dfas.new_right_ctx(bindings, right_ctx));

        nfa.add_regex(bindings, &re, right_ctx, rhs);
    }

    nfa_to_dfa(&nfa)
}
