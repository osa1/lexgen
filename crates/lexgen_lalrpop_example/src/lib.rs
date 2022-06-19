//! This example shows how to use the generated lexer with [lalrpop](https://docs.rs/lalrpop/latest/lalrpop/)
//! by implementing an evaluator of an example language.
//!
//! The language has three type of expressions. The first type is a string expression which starts
//! and ends with `"`. The other types are string concatenation denoted by `+` and parenthesized
//! expression. Inside string expression, you can write caracters as normal language, and can also
//! interpolate another expression by surrounding the expression with `\(` and `)`.

use lalrpop_util::lalrpop_mod;

pub mod ast {
    #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
    pub enum StringFragment<'input> {
        /// Represents a sequence of normal characters or a string consists of a single
        /// escaped character in a string literal.
        String(&'input str),
        /// Represents an interpolated expression in a string literal.
        Expression(Expression<'input>),
    }

    #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
    pub enum Expression<'input> {
        /// Represents a string literal.
        String(Vec<StringFragment<'input>>),
        /// Represents `lhs + rhs`. It's possible to make this desugared into
        /// `"\(lhs)\(rhs)"` instead of having this variant.
        Concat(Box<Expression<'input>>, Box<Expression<'input>>),
    }

    impl StringFragment<'_> {
        fn eval_to(&self, w: &mut impl std::fmt::Write) -> std::fmt::Result {
            match self {
                StringFragment::String(s) => w.write_str(s),
                StringFragment::Expression(e) => e.eval_to(w),
            }
        }
    }

    impl Expression<'_> {
        fn eval_to(&self, w: &mut impl std::fmt::Write) -> std::fmt::Result {
            match self {
                Expression::String(v) => v.iter().try_for_each(|f| f.eval_to(w)),
                Expression::Concat(l, r) => [l, r].iter().try_for_each(|e| e.eval_to(w)),
            }
        }

        pub fn eval(&self) -> String {
            let mut ret = String::new();
            self.eval_to(&mut ret)
                .expect("Format into String shoudln't fail");
            ret
        }
    }
}

#[allow(clippy::manual_range_contains)]
pub mod lexer {
    use lexgen::lexer;
    pub type LexerError = lexgen_util::LexerError<String>;
    pub type Loc = lexgen_util::Loc;

    #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
    pub enum Token<'input> {
        /// Represents `+` (outside of string literals).
        Plus,
        /// Represents `(` of parenthesized expression (outside of string literals).
        LParen,
        /// Represents `)` of parenthesized expression (outside of string literals).
        RParen,
        /// Represents `"` at the begining of a string literal.
        StringStart,
        /// Represents a non-interpolated part of a string literal.
        /// Either a sequence of characters exactly same as a part of input,
        /// or an un-escaped character of a part of input.
        StringFragment(&'input str),
        /// Represents `"` at the end of a string literal.
        StringEnd,
        /// Represents `\(` that starts interpolation in a string literal.
        InterpolationStart,
        /// Represents `)` that ends interpolation in a string literal.
        InterpolationEnd,
    }

    pub struct LexerState {
        /// For each interpolation, we want to lex the `)` that ends the interpolation differently.
        /// To do so, we keep track of the balance of parenthesis in the expression, and treat the
        /// the first `)` that over-closed the expression as the interpolation end marker.
        /// Since we can nest string interpolations like `"\("\("a")")"`, we use stack to keep this
        /// balance.
        paren_nest: Vec<usize>,
    }

    impl Default for LexerState {
        fn default() -> Self {
            Self {
                paren_nest: vec![0],
            }
        }
    }

    lexer! {
        pub Lexer(LexerState) -> Token<'input>;
        type Error = String;

        let ws = [' ' '\t' '\n'] | "\r\n";

        rule Init {
            $ws,
            '+' = Token::Plus,
            '"' => |lexer| lexer.switch_and_return(LexerRule::InString, Token::StringStart),
            '(' =? |lexer| {
                match lexer.state().paren_nest.last_mut() {
                    Some(x) => {
                        *x += 1;
                        lexer.return_(Ok(Token::LParen))
                    },
                    None => {
                        lexer.return_(Err("Invalid state, maybe already failed?".to_string()))
                    }
                }
            },
            ')' =? |lexer| {
                match lexer.state().paren_nest.last_mut() {
                    Some(0) => {
                        lexer.state().paren_nest.pop();
                        if lexer.state().paren_nest.is_empty() {
                            lexer.return_(Err("Too many close parens".to_string()))
                        } else {
                            lexer.switch_and_return(LexerRule::InString, Ok(Token::InterpolationEnd))
                        }
                    },
                    Some(x) => {
                        *x -= 1;
                        lexer.return_(Ok(Token::RParen))
                    },
                    None => {
                        lexer.return_(Err("Invalid state, maybe already failed?".to_string()))
                    }
                }
            },
        }

        rule InString {
            "\\\"" = Token::StringFragment("\""),
            "\\n" = Token::StringFragment("\n"),
            "\\r" = Token::StringFragment("\r"),
            "\\t" = Token::StringFragment("\t"),
            "\\\\" = Token::StringFragment("\\"),
            '"' => |lexer| lexer.switch_and_return(LexerRule::Init, Token::StringEnd),
            "\\(" => |lexer| {
                lexer.state().paren_nest.push(0);
                lexer.switch_and_return(LexerRule::Init, Token::InterpolationStart)
            },
            (_ # ['\\' '"'])+ => |lexer| lexer.return_(Token::StringFragment(lexer.match_())),
        }
    }
}

lalrpop_mod!(#[allow(unused_imports, clippy::all)] pub parser, "/interpolation.rs");

#[cfg(test)]
mod test {
    use super::{
        ast::Expression,
        lexer::{Lexer, LexerError, Loc, Token},
        parser::ExpressionParser,
    };

    type Result<'input, T> =
        std::result::Result<T, lalrpop_util::ParseError<Loc, Token<'input>, LexerError>>;

    fn parse(code: &str) -> Result<Expression> {
        let lexer = Lexer::new(code);
        ExpressionParser::new().parse(lexer)
    }

    fn parse_and_eval(code: &str) -> Result<String> {
        parse(code).map(|e| e.eval())
    }

    #[test]
    fn test_basic() -> Result<'static, ()> {
        assert_eq!(parse_and_eval(r#""a" + "b" + "c""#)?, "abc");
        assert_eq!(parse_and_eval(r#""\n\t\\(" + "b" + "c""#)?, "\n\t\\(bc");
        Ok(())
    }

    #[test]
    fn test_invalid() {
        assert!(parse(r#""a" +"#).is_err());
        assert!(parse(r#""a" + ""#).is_err());
        assert!(parse(r#"("a" + "b" "#).is_err());
        assert!(parse(r#""a\(""#).is_err());
        assert!(parse(r#""a\(")""#).is_err());
        assert!(parse(r#""a\())""#).is_err());
        assert!(parse(r#"("a\())""#).is_err());
        assert!(parse(r#")"#).is_err());
    }

    #[test]
    fn test_associativity() -> Result<'static, ()> {
        assert_eq!(
            parse(r#""a" + "b" + "c" + "d""#)?,
            parse(r#"(("a" + "b") + "c") + "d""#)?
        );
        Ok(())
    }

    #[test]
    fn test_interpolation() -> Result<'static, ()> {
        assert_eq!(parse_and_eval(r#""ab\("c" + "d")""#)?, "abcd");
        assert_eq!(parse_and_eval(r#""ab\(("c") + ("d"))""#)?, "abcd");
        assert_eq!(parse_and_eval(r#""ab\(("c\("d")"))""#)?, "abcd");
        Ok(())
    }
}
