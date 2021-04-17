//! Proc macro AST definition and parser implementations

use syn::parse::{Parse, ParseStream};

pub struct Lexer {
    pub type_name: syn::Ident,
    pub token_type: syn::Type,
    pub rules: Vec<Rule>,
}

pub struct Rule {
    pub lhs: Regex,
    pub rhs: syn::Expr,
}

impl std::fmt::Debug for Lexer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Lexer")
            .field("token_type", &"...")
            .field("rules", &self.rules)
            .finish()
    }
}

impl std::fmt::Debug for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Rule")
            .field("lhs", &self.lhs)
            .field("rhs", &"...")
            .finish()
    }
}

#[derive(Debug)]
pub enum Regex {
    Var(String),
    Char(char),
    String(String),
    CharSet(CharSet),
    ZeroOrMore(Box<Regex>),
    OneOrMore(Box<Regex>),
    ZeroOrOne(Box<Regex>),
    Concat(Box<Regex>, Box<Regex>),
    Or(Box<Regex>, Box<Regex>),
    // Diff(Box<Regex>, Box<Regex>),
}

#[derive(Debug)]
pub struct CharSet(pub Vec<CharOrRange>);

#[derive(Debug)]
pub enum CharOrRange {
    Char(char),
    Range(char, char),
}

/// Parses a `=>` terminated regex
impl Parse for Regex {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut re = parse_regex_1(input)?;

        while !input.peek(syn::token::FatArrow) {
            if input.peek(syn::token::Star) {
                let _ = input.parse::<syn::token::Star>()?;
                re = Regex::ZeroOrMore(Box::new(re));
            } else if input.peek(syn::token::Question) {
                let _ = input.parse::<syn::token::Question>()?;
                re = Regex::ZeroOrOne(Box::new(re));
            } else if input.peek(syn::token::Add) {
                let _ = input.parse::<syn::token::Add>()?;
                re = Regex::OneOrMore(Box::new(re));
            } else if input.peek(syn::token::Or) {
                let _ = input.parse::<syn::token::Or>()?;
                let re2 = Regex::parse(input)?;
                re = Regex::Or(Box::new(re), Box::new(re2));
            } else {
                let re2 = Regex::parse(input)?;
                re = Regex::Concat(Box::new(re), Box::new(re2));
            }
        }

        Ok(re)
    }
}

fn parse_regex_1(input: ParseStream) -> syn::Result<Regex> {
    if input.peek(syn::token::Paren) {
        let parenthesized;
        syn::parenthesized!(parenthesized in input);
        Regex::parse(&parenthesized)
    } else if input.peek(syn::Ident) {
        let ident = input.parse::<syn::Ident>()?;
        Ok(Regex::Var(ident.to_string()))
    } else if input.peek(syn::token::Dollar) {
        let _ = input.parse::<syn::token::Dollar>()?;
        todo!()
    } else if input.peek(syn::LitChar) {
        let char = input.parse::<syn::LitChar>()?;
        Ok(Regex::Char(char.value()))
    } else if input.peek(syn::LitStr) {
        let str = input.parse::<syn::LitStr>()?;
        Ok(Regex::String(str.value()))
    } else if input.peek(syn::token::Bracket) {
        let bracketed;
        syn::bracketed!(bracketed in input);
        let char_set = CharSet::parse(&bracketed)?;
        Ok(Regex::CharSet(char_set))
    } else {
        Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "Unable to parse regex",
        ))
    }
}

impl Parse for CharSet {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut chars = vec![];
        while !input.is_empty() {
            chars.push(CharOrRange::parse(input)?);
        }
        Ok(CharSet(chars))
    }
}

impl Parse for CharOrRange {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let char = input.parse::<syn::LitChar>()?.value();
        if input.peek(syn::token::Sub) {
            let _ = input.parse::<syn::token::Sub>()?;
            let char2 = input.parse::<syn::LitChar>()?.value();
            Ok(CharOrRange::Range(char, char2))
        } else {
            Ok(CharOrRange::Char(char))
        }
    }
}

impl Parse for Rule {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lhs = Regex::parse(input)?;
        input.parse::<syn::token::FatArrow>()?;
        let rhs = input.parse::<syn::Expr>()?;
        input.parse::<syn::token::Comma>()?;
        Ok(Rule { lhs, rhs })
    }
}

impl Parse for Lexer {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let type_name = input.parse::<syn::Ident>()?;
        input.parse::<syn::token::RArrow>()?;
        let token_type = input.parse::<syn::Type>()?;
        input.parse::<syn::token::Semi>()?;

        let mut rules = vec![];
        while !input.is_empty() {
            rules.push(Rule::parse(input)?);
        }

        Ok(Lexer {
            type_name,
            token_type,
            rules,
        })
    }
}
