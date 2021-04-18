mod ast;
mod dfa;
mod display;
mod nfa;
mod nfa_to_dfa;
mod regex_to_nfa;

use ast::{Lexer, Regex, Rule, Var};
use nfa::NFA;

use fxhash::FxHashMap;
use proc_macro::TokenStream;

#[proc_macro]
pub fn lexer_gen(input: TokenStream) -> TokenStream {
    let Lexer {
        type_name,
        token_type,
        rules,
    } = syn::parse_macro_input!(input as Lexer);

    let mut nfa: NFA<syn::Expr> = NFA::new();

    let mut bindings: FxHashMap<Var, Regex> = Default::default();
    for rule in rules {
        match rule {
            Rule::Binding { var, re } => {
                if let Some(_) = bindings.insert(var.clone(), re) {
                    panic!("Variable {:?} is defined multiple times", var.0);
                }
            }
            Rule::Rule { lhs, rhs } => {
                println!("{:?}", lhs);
                nfa.add_regex(&bindings, &lhs, rhs);
            }
        }
    }

    println!("NFA:");
    println!("{}", nfa);

    let dfa = nfa_to_dfa::nfa_to_dfa(&nfa);
    println!("DFA:");
    println!("{}", dfa);

    dfa.reify(type_name, token_type).into()
}
