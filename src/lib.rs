mod ast;
mod nfa;
mod regex_to_nfa;

use proc_macro::TokenStream;

use ast::Regex;

#[proc_macro]
pub fn lexer_gen(input: TokenStream) -> TokenStream {
    let re: Regex = syn::parse_macro_input!(input as Regex);
    println!("{:#?}", re);
    let nfa = regex_to_nfa::regex_to_nfa(&re);
    println!("{:#?}", nfa);

    TokenStream::new()
}
