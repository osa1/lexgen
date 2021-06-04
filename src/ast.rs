//! Proc macro AST definition and parser implementations

use syn::parse::{Parse, ParseStream};

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
        /// Lifetimes on the LHS of the declaration, e.g. `type Error<'input> = ...`;
        lifetimes: Vec<syn::Lifetime>,
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
    pub lhs: RegexOrFail,
    pub rhs: RuleRhs,
}

#[derive(Debug)]
pub enum RegexOrFail {
    Regex(Regex),

    /// An `_` as the LHS of a rule. This rule only matches when none of the other rules in the
    /// same rule set match.
    Fail,
}

#[derive(Clone)]
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
            Rule::ErrorType { lifetimes, ty } => f
                .debug_struct("Rule::ErrorType")
                .field("lifetimes", lifetimes)
                .field("ty", ty)
                .finish(),
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
impl Parse for Regex {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut re = parse_regex_1(input)?;

        while !(input.peek(syn::token::FatArrow)
            || input.peek(syn::token::Eq)
            || input.peek(syn::token::Comma)
            || input.peek(syn::token::Semi)
            || input.is_empty())
        {
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
    } else if input.peek(syn::token::Dollar) {
        let _ = input.parse::<syn::token::Dollar>()?;
        if input.parse::<syn::token::Dollar>().is_ok() {
            let ident = input.parse::<syn::Ident>()?;
            Ok(Regex::Builtin(Builtin(ident.to_string())))
        } else {
            let ident = input.parse::<syn::Ident>()?;
            Ok(Regex::Var(Var(ident.to_string())))
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

impl Parse for RegexOrFail {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.parse::<syn::token::Underscore>().is_ok() {
            Ok(RegexOrFail::Fail)
        } else {
            Ok(RegexOrFail::Regex(input.parse::<Regex>()?))
        }
    }
}

impl Parse for SingleRule {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lhs = RegexOrFail::parse(input)?;

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

        Ok(SingleRule { lhs, rhs })
    }
}

impl Parse for Rule {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(syn::token::Let) {
            // Let binding
            input.parse::<syn::token::Let>()?;
            let var = input.parse::<syn::Ident>()?;
            input.parse::<syn::token::Eq>()?;
            let re = Regex::parse(input)?;
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
                single_rules.push(SingleRule::parse(&braced)?);
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
            let mut lifetimes: Vec<syn::Lifetime> = vec![];
            if input.parse::<syn::token::Lt>().is_ok() {
                while !input.peek(syn::token::Gt) {
                    lifetimes.push(input.parse()?);
                }
                input.parse::<syn::token::Gt>()?;
            }
            input.parse::<syn::token::Eq>()?;
            let ty = input.parse::<syn::Type>()?;
            input.parse::<syn::token::Semi>()?;
            Ok(Rule::ErrorType { ty, lifetimes })
        } else {
            let mut single_rules = vec![];
            while !input.is_empty() {
                single_rules.push(SingleRule::parse(input)?);
            }
            Ok(Rule::UnnamedRules {
                rules: single_rules,
            })
        }
    }
}

impl Parse for Lexer {
    fn parse(input: ParseStream) -> syn::Result<Self> {
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
            rules.push(Rule::parse(input)?);
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
