//! Proc macro AST definition and parser implementations

use crate::semantic_action_table::{SemanticActionIdx, SemanticActionTable};

use syn::parse::ParseStream;

use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Var(pub String);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Builtin(pub String);

pub struct Lexer {
    pub public: bool,
    pub type_name: syn::Ident,
    pub user_state_type: Option<syn::Type>,
    pub token_type: syn::Type,
    pub rules: Vec<Rule>,
}

pub enum Rule {
    /// `let <ident> = <regex>;`
    Binding { var: Var, re: Regex },

    /// `type Error = UserError;`
    ErrorType {
        /// Type on the RHS, e.g. `UserError<'input>`
        ty: syn::Type,
    },

    /// A list of named rules at the top level: `rule <Ident> { <rules> },`
    RuleSet {
        name: syn::Ident,
        rules: Vec<SingleRule>,
    },

    /// Set of rules without a name
    UnnamedRules { rules: Vec<SingleRule> },
}

pub struct SingleRule {
    pub lhs: Regex,
    pub rhs: SemanticActionIdx,
}

#[derive(Debug, Clone)]
pub enum RuleRhs {
    None,
    Rhs { expr: syn::Expr, kind: RuleKind },
}

#[derive(Debug, Copy, Clone)]
pub enum RuleKind {
    /// Defined with `=`. RHS is not passed a `LexerHandle`, returns `Token`.
    Simple,

    /// Defined with `=?`. RHS is passed a `LexerHandle`, returns `LexerAction<Result<Token,
    /// Error>>`.
    Fallible,

    /// Defined with `=>`. RHS is passed a `LexerHandle`, returns `LexerAction<Token>`
    Infallible,
}

impl fmt::Debug for Lexer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Lexer")
            .field("public", &self.public)
            .field("type_name", &self.type_name.to_string())
            .field("token_type", &"...")
            .field("rules", &self.rules)
            .finish()
    }
}

impl fmt::Debug for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Rule::Binding { var, re } => f
                .debug_struct("Rule::Binding")
                .field("var", var)
                .field("re", re)
                .finish(),
            Rule::RuleSet { name, rules } => f
                .debug_struct("Rule::RuleSet")
                .field("name", &name.to_string())
                .field("rules", rules)
                .finish(),
            Rule::UnnamedRules { rules } => f
                .debug_struct("Rule::UnnamedRules")
                .field("rules", rules)
                .finish(),
            Rule::ErrorType { ty } => f.debug_struct("Rule::ErrorType").field("ty", ty).finish(),
        }
    }
}

impl fmt::Debug for SingleRule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SingleRule")
            .field("lhs", &self.lhs)
            .field("rhs", &"...")
            .finish()
    }
}

#[derive(Debug, Clone)]
pub enum Regex {
    Builtin(Builtin),
    Var(Var),
    Char(char),
    String(String),
    CharSet(CharSet),
    ZeroOrMore(Box<Regex>),
    OneOrMore(Box<Regex>),
    ZeroOrOne(Box<Regex>),
    Concat(Box<Regex>, Box<Regex>),
    Or(Box<Regex>, Box<Regex>),
    Any, // any character
    EndOfInput,
}

#[derive(Debug, Clone)]
pub struct CharSet(pub Vec<CharOrRange>);

#[derive(Debug, Clone, Copy)]
pub enum CharOrRange {
    Char(char),
    Range(char, char),
}

/// Parses a regex terminated with: `=>` (used in rules with RHSs), `,` (used in rules without
/// RHSs), or `;` (used in let bindings)
fn parse_regex(input: ParseStream) -> syn::Result<Regex> {
    parse_regex_0(input)
}

// re_0 -> re_1 | re_1 `|` re_1 (alternation)
fn parse_regex_0(input: ParseStream) -> syn::Result<Regex> {
    let mut re = parse_regex_1(input)?;

    while input.peek(syn::token::Or) {
        let _ = input.parse::<syn::token::Or>()?;
        let re2 = parse_regex_1(input)?;
        re = Regex::Or(Box::new(re), Box::new(re2)); // left associative
    }

    Ok(re)
}

// re_1 -> re_2 | re_2 re_2
fn parse_regex_1(input: ParseStream) -> syn::Result<Regex> {
    let mut re = parse_regex_2(input)?;

    // Parse concatenations
    while input.peek(syn::token::Paren)
        || input.peek(syn::token::Dollar)
        || input.peek(syn::LitChar)
        || input.peek(syn::LitStr)
        || input.peek(syn::token::Bracket)
        || input.peek(syn::token::Underscore)
    {
        let re2 = parse_regex_2(input)?;
        re = Regex::Concat(Box::new(re), Box::new(re2)); // left associative
    }

    Ok(re)
}

// re_2 -> re_3 | re_3* | re_3? | re_3+
fn parse_regex_2(input: ParseStream) -> syn::Result<Regex> {
    let mut re = parse_regex_3(input)?;

    loop {
        if input.peek(syn::token::Star) {
            let _ = input.parse::<syn::token::Star>()?;
            re = Regex::ZeroOrMore(Box::new(re));
        } else if input.peek(syn::token::Question) {
            let _ = input.parse::<syn::token::Question>()?;
            re = Regex::ZeroOrOne(Box::new(re));
        } else if input.peek(syn::token::Add) {
            let _ = input.parse::<syn::token::Add>()?;
            re = Regex::OneOrMore(Box::new(re));
        } else {
            break;
        }
    }

    Ok(re)
}

// re_3 -> ( re_0 ) | $ | $x | $$x | _ | 'x' | "..." | [...]
fn parse_regex_3(input: ParseStream) -> syn::Result<Regex> {
    if input.peek(syn::token::Paren) {
        let parenthesized;
        syn::parenthesized!(parenthesized in input);
        parse_regex(&parenthesized)
    } else if input.peek(syn::token::Dollar) {
        let _ = input.parse::<syn::token::Dollar>()?;
        if input.parse::<syn::token::Dollar>().is_ok() {
            let ident = input.parse::<syn::Ident>()?;
            Ok(Regex::Builtin(Builtin(ident.to_string())))
        } else {
            match input.parse::<syn::Ident>() {
                Ok(ident) => Ok(Regex::Var(Var(ident.to_string()))),
                Err(_) => Ok(Regex::EndOfInput),
            }
        }
    } else if input.peek(syn::LitChar) {
        let char = input.parse::<syn::LitChar>()?;
        Ok(Regex::Char(char.value()))
    } else if input.peek(syn::LitStr) {
        let str = input.parse::<syn::LitStr>()?;
        Ok(Regex::String(str.value()))
    } else if input.peek(syn::token::Bracket) {
        let bracketed;
        syn::bracketed!(bracketed in input);
        let char_set = parse_charset(&bracketed)?;
        Ok(Regex::CharSet(char_set))
    } else if input.parse::<syn::token::Underscore>().is_ok() {
        Ok(Regex::Any)
    } else {
        Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "Unable to parse regex",
        ))
    }
}

fn parse_charset(input: ParseStream) -> syn::Result<CharSet> {
    let mut chars = vec![];
    while !input.is_empty() {
        chars.push(parse_char_or_range(input)?);
    }
    Ok(CharSet(chars))
}

fn parse_char_or_range(input: ParseStream) -> syn::Result<CharOrRange> {
    let char = input.parse::<syn::LitChar>()?.value();
    if input.peek(syn::token::Sub) {
        let _ = input.parse::<syn::token::Sub>()?;
        let char2 = input.parse::<syn::LitChar>()?.value();
        Ok(CharOrRange::Range(char, char2))
    } else {
        Ok(CharOrRange::Char(char))
    }
}

fn parse_single_rule(
    input: ParseStream,
    semantic_action_table: &mut SemanticActionTable,
) -> syn::Result<SingleRule> {
    let lhs = parse_regex(input)?;

    let rhs = if input.parse::<syn::token::Comma>().is_ok() {
        RuleRhs::None
    } else if input.parse::<syn::token::FatArrow>().is_ok() {
        let expr = input.parse::<syn::Expr>()?;
        input.parse::<syn::token::Comma>()?;
        RuleRhs::Rhs {
            expr,
            kind: RuleKind::Infallible,
        }
    } else if input.parse::<syn::token::Eq>().is_ok() {
        let kind = if input.peek(syn::token::Question) {
            let _ = input.parse::<syn::token::Question>();
            RuleKind::Fallible
        } else {
            RuleKind::Simple
        };
        let expr = input.parse::<syn::Expr>()?;
        input.parse::<syn::token::Comma>()?;
        RuleRhs::Rhs { expr, kind }
    } else {
        panic!("Expected one of `,`, `=>`, `=?`, or `=` after a regex");
    };

    let rhs = semantic_action_table.add(rhs);

    Ok(SingleRule { lhs, rhs })
}

fn parse_rule(
    input: ParseStream,
    semantic_action_table: &mut SemanticActionTable,
) -> syn::Result<Rule> {
    if input.peek(syn::token::Let) {
        // Let binding
        input.parse::<syn::token::Let>()?;
        let var = input.parse::<syn::Ident>()?;
        input.parse::<syn::token::Eq>()?;
        let re = parse_regex(input)?;
        input.parse::<syn::token::Semi>()?;
        Ok(Rule::Binding {
            var: Var(var.to_string()),
            re,
        })
    } else if input.peek(syn::Ident) {
        // Name rules
        let ident = input.parse::<syn::Ident>()?;
        if ident != "rule" {
            return Err(syn::Error::new(
                ident.span(),
                "Unknown identifier, expected \"rule\", \"let\", or a regex",
            ));
        }
        let rule_name = input.parse::<syn::Ident>()?;
        let braced;
        syn::braced!(braced in input);
        let mut single_rules = vec![];
        while !braced.is_empty() {
            single_rules.push(parse_single_rule(&braced, semantic_action_table)?);
        }
        // Consume optional trailing comma
        let _ = input.parse::<syn::token::Comma>();
        Ok(Rule::RuleSet {
            name: rule_name,
            rules: single_rules,
        })
    } else if input.parse::<syn::token::Type>().is_ok() {
        let ident = input.parse::<syn::Ident>()?;
        if ident != "Error" {
            panic!("Error type syntax is: `type Error = ...;`");
        }
        input.parse::<syn::token::Eq>()?;
        let ty = input.parse::<syn::Type>()?;
        input.parse::<syn::token::Semi>()?;
        Ok(Rule::ErrorType { ty })
    } else {
        let mut single_rules = vec![];
        while !input.is_empty() {
            single_rules.push(parse_single_rule(input, semantic_action_table)?);
        }
        Ok(Rule::UnnamedRules {
            rules: single_rules,
        })
    }
}

pub fn make_lexer_parser<'a>(
    semantic_action_table: &'a mut SemanticActionTable,
) -> impl FnOnce(ParseStream) -> Result<Lexer, syn::Error> + 'a {
    |input: ParseStream| {
        let public = input.parse::<syn::token::Pub>().is_ok();
        let type_name = input.parse::<syn::Ident>()?;

        let user_state_type = if input.peek(syn::token::Paren) {
            let parenthesized;
            syn::parenthesized!(parenthesized in input);
            Some(parenthesized.parse::<syn::Type>()?)
        } else {
            None
        };

        input.parse::<syn::token::RArrow>()?;
        let token_type = input.parse::<syn::Type>()?;
        input.parse::<syn::token::Semi>()?;

        let mut rules = vec![];
        while !input.is_empty() {
            rules.push(parse_rule(input, semantic_action_table)?);
        }

        Ok(Lexer {
            public,
            type_name,
            user_state_type,
            token_type,
            rules,
        })
    }
}
