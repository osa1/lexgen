mod ast;
mod dfa;
mod display;
mod nfa;
mod nfa_to_dfa;
mod regex_to_nfa;

use ast::{Lexer, Regex, Rule, SingleRule, Var};
use dfa::DFA;
use nfa::NFA;
use nfa_to_dfa::nfa_to_dfa;

use fxhash::FxHashMap;
use proc_macro::TokenStream;

#[proc_macro]
pub fn lexer_gen(input: TokenStream) -> TokenStream {
    let Lexer {
        type_name,
        token_type,
        rules,
    } = syn::parse_macro_input!(input as Lexer);

    let mut named_dfas: FxHashMap<String, DFA<Option<syn::Expr>>> = Default::default();

    // First pass to collect default rules and build the NFA for the initial state. Default rules
    // are added to named rules.
    // TODO: Add the default NFA to the NFAs for named rules instead
    let mut default_nfa: NFA<Option<syn::Expr>> = NFA::new();
    let mut default_rules: Vec<&SingleRule> = vec![];
    let mut bindings: FxHashMap<Var, Regex> = Default::default();
    for rule in &rules {
        match rule {
            Rule::Binding { var, re } => {
                if let Some(_) = bindings.insert(var.clone(), re.clone()) {
                    panic!("Variable {:?} is defined multiple times", var.0);
                }
            }
            Rule::DefaultRule(single_rule) => {
                default_nfa.add_regex(&bindings, &single_rule.lhs, single_rule.rhs);
                default_rules.push(single_rule);
            }
            Rule::NamedRules { .. } => {}
        }
    }

    let default_dfa = nfa_to_dfa(&default_nfa);

    for rule in rules {
        match rule {
            Rule::Binding { .. } => {}
            Rule::DefaultRule(_) => {}
            Rule::NamedRules { name, rules } => {
                let mut nfa: NFA<Option<syn::Expr>> = NFA::new();
                for default_rule in &default_rules {
                    nfa.add_regex(&bindings, &default_rule.lhs, default_rule.rhs);
                }
                for rule in rules {
                    nfa.add_regex(&bindings, &rule.lhs, rule.rhs);
                }
                named_dfas.insert(name.to_string(), nfa_to_dfa(&nfa));
            }
        }
    }

    // let dfa = nfa_to_dfa::nfa_to_dfa(&nfa);
    // dfa::reify(&dfa, type_name, token_type).into()
    todo!()
}

#[cfg(test)]
mod tests {
    use crate::ast::{CharOrRange, CharSet, Regex, Var};
    use crate::nfa::NFA;
    use crate::nfa_to_dfa::nfa_to_dfa;

    use fxhash::FxHashMap;

    fn test_simulate<A: Clone + std::fmt::Debug + Eq>(
        nfa: &NFA<A>,
        test_cases: Vec<(&str, Option<A>)>,
    ) {
        for (str, expected) in &test_cases {
            assert_eq!(nfa.simulate(&mut str.chars()), expected.as_ref());
        }

        let dfa = nfa_to_dfa(nfa);

        for (str, expected) in &test_cases {
            assert_eq!(dfa.simulate(&mut str.chars()), expected.as_ref());
        }
    }

    #[test]
    fn simulate_char() {
        let re = Regex::Char('a');
        let mut nfa: NFA<()> = NFA::new();
        nfa.add_regex(&Default::default(), &re, ());
        test_simulate(
            &nfa,
            vec![("", None), ("aa", None), ("a", Some(())), ("b", None)],
        );
    }

    #[test]
    fn simulate_string() {
        let re = Regex::String("ab".to_owned());
        let mut nfa: NFA<()> = NFA::new();
        nfa.add_regex(&Default::default(), &re, ());
        test_simulate(
            &nfa,
            vec![("", None), ("a", None), ("ab", Some(())), ("abc", None)],
        );
    }

    #[test]
    fn simulate_char_set_char() {
        let re = Regex::CharSet(CharSet(vec![
            CharOrRange::Char('a'),
            CharOrRange::Char('b'),
        ]));
        let mut nfa: NFA<()> = NFA::new();
        nfa.add_regex(&Default::default(), &re, ());
        test_simulate(
            &nfa,
            vec![
                ("", None),
                ("a", Some(())),
                ("b", Some(())),
                ("ab", None),
                ("ba", None),
            ],
        );
    }

    #[test]
    fn simulate_char_set_range() {
        let re = Regex::CharSet(CharSet(vec![
            CharOrRange::Char('a'),
            CharOrRange::Char('b'),
            CharOrRange::Range('0', '9'),
        ]));
        let mut nfa: NFA<()> = NFA::new();
        nfa.add_regex(&Default::default(), &re, ());
        test_simulate(
            &nfa,
            vec![
                ("", None),
                ("a", Some(())),
                ("b", Some(())),
                ("0", Some(())),
                ("1", Some(())),
                ("9", Some(())),
                ("ba", None),
            ],
        );
    }

    #[test]
    fn simulate_zero_or_more() {
        let re = Regex::ZeroOrMore(Box::new(Regex::Char('a')));
        let mut nfa: NFA<()> = NFA::new();
        nfa.add_regex(&Default::default(), &re, ());
        test_simulate(
            &nfa,
            vec![
                ("", Some(())),
                ("a", Some(())),
                ("aa", Some(())),
                ("aab", None),
            ],
        );
    }

    #[test]
    fn simulate_one_or_more() {
        let re = Regex::OneOrMore(Box::new(Regex::Char('a')));
        let mut nfa: NFA<()> = NFA::new();
        nfa.add_regex(&Default::default(), &re, ());
        test_simulate(
            &nfa,
            vec![("", None), ("a", Some(())), ("aa", Some(())), ("aab", None)],
        );
    }

    #[test]
    fn simulate_zero_or_one() {
        let re = Regex::ZeroOrOne(Box::new(Regex::Char('a')));
        let mut nfa: NFA<()> = NFA::new();
        nfa.add_regex(&Default::default(), &re, ());
        test_simulate(&nfa, vec![("", Some(())), ("a", Some(())), ("aa", None)]);
    }

    #[test]
    fn simulate_concat() {
        let re = Regex::Concat(Box::new(Regex::Char('a')), Box::new(Regex::Char('b')));
        let mut nfa: NFA<()> = NFA::new();
        nfa.add_regex(&Default::default(), &re, ());
        test_simulate(
            &nfa,
            vec![
                ("", None),
                ("a", None),
                ("ab", Some(())),
                ("aba", None),
                ("abb", None),
            ],
        );
    }

    #[test]
    fn simulate_or() {
        let re = Regex::Or(Box::new(Regex::Char('a')), Box::new(Regex::Char('b')));
        let mut nfa: NFA<()> = NFA::new();
        nfa.add_regex(&Default::default(), &re, ());
        test_simulate(
            &nfa,
            vec![
                ("", None),
                ("a", Some(())),
                ("b", Some(())),
                ("aa", None),
                ("ab", None),
            ],
        );
    }

    #[test]
    fn or_one_or_more_char() {
        let re = Regex::Or(
            Box::new(Regex::OneOrMore(Box::new(Regex::Char('a')))),
            Box::new(Regex::Char('b')),
        );
        let mut nfa: NFA<()> = NFA::new();
        nfa.add_regex(&Default::default(), &re, ());
        test_simulate(
            &nfa,
            vec![
                ("b", Some(())),
                ("a", Some(())),
                ("aa", Some(())),
                ("", None),
            ],
        );
    }

    #[test]
    fn multiple_accepting_states_1() {
        let re1 = Regex::String("aaaa".to_owned());
        let re2 = Regex::String("aaab".to_owned());
        let mut nfa: NFA<usize> = NFA::new();
        nfa.add_regex(&Default::default(), &re1, 1);
        nfa.add_regex(&Default::default(), &re2, 2);
        test_simulate(
            &nfa,
            vec![
                ("aaaa", Some(1)),
                ("aaab", Some(2)),
                ("aaaba", None),
                ("aaac", None),
            ],
        );
    }

    #[test]
    fn multiple_accepting_states_2() {
        // Same test case as `nfa::test::or_one_or_more_char`
        let re1 = Regex::Or(
            Box::new(Regex::OneOrMore(Box::new(Regex::Char('a')))),
            Box::new(Regex::Char('b')),
        );
        let re2 = Regex::CharSet(CharSet(vec![CharOrRange::Range('0', '9')]));
        let mut nfa: NFA<usize> = NFA::new();
        nfa.add_regex(&Default::default(), &re1, 1);
        nfa.add_regex(&Default::default(), &re2, 2);
        test_simulate(
            &nfa,
            vec![
                ("b", Some(1)),
                ("a", Some(1)),
                ("aa", Some(1)),
                ("", None),
                ("0", Some(2)),
                ("5", Some(2)),
            ],
        );
    }

    #[test]
    fn variables() {
        let mut bindings: FxHashMap<Var, Regex> = Default::default();
        bindings.insert(
            Var("initial".to_owned()),
            Regex::CharSet(CharSet(vec![CharOrRange::Range('a', 'z')])),
        );
        bindings.insert(
            Var("subsequent".to_owned()),
            Regex::CharSet(CharSet(vec![
                CharOrRange::Range('a', 'z'),
                CharOrRange::Range('A', 'Z'),
                CharOrRange::Range('0', '9'),
                CharOrRange::Char('-'),
                CharOrRange::Char('_'),
            ])),
        );
        let re = Regex::Concat(
            Box::new(Regex::Var(Var("initial".to_owned()))),
            Box::new(Regex::ZeroOrMore(Box::new(Regex::Var(Var(
                "subsequent".to_owned()
            ))))),
        );
        let mut nfa: NFA<()> = NFA::new();
        nfa.add_regex(&bindings, &re, ());
        test_simulate(
            &nfa,
            vec![("a", Some(())), ("aA", Some(())), ("aA123-a", Some(()))],
        );
    }
}
