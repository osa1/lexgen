#![allow(dead_code)]

mod ast;
mod dfa;
mod display;
mod nfa;
mod nfa_to_dfa;
mod regex_to_nfa;

mod reify_test;

use proc_macro::TokenStream;

use ast::Lexer;

#[proc_macro]
pub fn lexer_gen(input: TokenStream) -> TokenStream {
    let lexer: Lexer = syn::parse_macro_input!(input as Lexer);
    println!("{:#?}", lexer);

    TokenStream::new()
}
