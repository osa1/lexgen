#![allow(dead_code)]

mod ast;
mod dfa;
mod display;
mod nfa;
mod nfa_to_dfa;
mod regex_to_nfa;

mod reify_test;

use proc_macro::TokenStream;

use ast::{Lexer, Rule};
use nfa::NFA;

#[proc_macro]
pub fn lexer_gen(input: TokenStream) -> TokenStream {
    let Lexer { rules } = syn::parse_macro_input!(input as Lexer);

    let (mut nfa, _): (NFA<syn::ExprClosure>, _) = NFA::new();

    for Rule { lhs, rhs } in rules {
        println!("{:?}", lhs);
        nfa.add_regex(&lhs, rhs);
    }

    println!("NFA:");
    println!("{}", nfa);

    let dfa = nfa_to_dfa::nfa_to_dfa(&nfa);
    println!("DFA:");
    println!("{}", dfa);

    TokenStream::new()
}
