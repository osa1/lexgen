//! Please see the [project README][1] for usage.
//!
//! [1]: https://github.com/osa1/lexgen

mod ast;
mod dfa;
mod display;
mod nfa;
mod nfa_to_dfa;
mod regex_to_nfa;

use ast::{Lexer, Regex, Rule, Var};
use dfa::DFA;
use nfa::NFA;
use nfa_to_dfa::nfa_to_dfa;

use fxhash::FxHashMap;
use proc_macro::TokenStream;

#[proc_macro]
pub fn lexer(input: TokenStream) -> TokenStream {
    let Lexer {
        type_name,
        user_state_type,
        token_type,
        rules: top_level_rules,
    } = syn::parse_macro_input!(input as Lexer);

    // Maps DFA names to their initial states in the final DFA
    let mut dfas: FxHashMap<String, dfa::StateIdx> = Default::default();

    let mut bindings: FxHashMap<Var, Regex> = Default::default();

    let mut dfa: Option<DFA<Option<syn::Expr>>> = None;

    for rule in &top_level_rules {
        match rule {
            Rule::Binding { var, re } => {
                if let Some(_) = bindings.insert(var.clone(), re.clone()) {
                    panic!("Variable {:?} is defined multiple times", var.0);
                }
            }
            Rule::RuleSet { name, rules } => {
                if name.to_string() == "Init" {
                    if dfa.is_some() {
                        panic!("\"Init\" rule set can only be defined once");
                    }
                    let mut nfa: NFA<Option<syn::Expr>> = NFA::new();
                    for rule in rules {
                        nfa.add_regex(&bindings, &rule.lhs, rule.rhs.clone());
                    }

                    // println!("NFA=\n{}", nfa);

                    let dfa_ = nfa_to_dfa(&nfa);
                    let initial_state = dfa_.initial_state();

                    // println!("DFA=\n{}", dfa_);

                    dfa = Some(dfa_);
                    if let Some(_) = dfas.insert(name.to_string(), initial_state) {
                        panic!("Rule set {:?} is defined multiple times", name.to_string());
                    }
                } else {
                    let dfa = match dfa.as_mut() {
                        None => panic!("First rule set should be named \"Init\""),
                        Some(dfa) => dfa,
                    };
                    let mut nfa: NFA<Option<syn::Expr>> = NFA::new();
                    for rule in rules {
                        nfa.add_regex(&bindings, &rule.lhs, rule.rhs.clone());
                    }
                    let dfa_idx = dfa.add_dfa(nfa_to_dfa(&nfa));
                    if let Some(_) = dfas.insert(name.to_string(), dfa_idx) {
                        panic!("Rule set {:?} is defined multiple times", name.to_string());
                    }
                }
            }
            Rule::UnnamedRules { rules } => {
                let have_named_rules = top_level_rules
                    .iter()
                    .any(|rule| matches!(rule, Rule::RuleSet { .. }));

                if dfa.is_some() || have_named_rules {
                    panic!(
                        "Unnamed rules cannot be mixed with named rules. Make sure to either \
                        have all your rules in `rule ... {} ... {}` syntax, or remove `rule`s \
                        entirely and have your rules at the top-level.",
                        '{', '}',
                    );
                }

                let mut nfa: NFA<Option<syn::Expr>> = NFA::new();
                for rule in rules {
                    nfa.add_regex(&bindings, &rule.lhs, rule.rhs.clone());
                }

                let dfa_ = nfa_to_dfa(&nfa);
                let initial_state = dfa_.initial_state();

                dfa = Some(dfa_);
                dfas.insert("Init".to_owned(), initial_state);
            }
        }
    }

    // There should be a rule with name "Init"
    if let None = dfas.get("Init") {
        panic!(
            "There should be a rule set named \"Init\". Current rules: {:?}",
            dfas.keys().collect::<Vec<&String>>()
        );
    }

    dfa::reify(&dfa.unwrap(), user_state_type, &dfas, type_name, token_type).into()
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
        println!("NFA=\n{}", nfa);

        for (str, expected) in &test_cases {
            assert_eq!(
                nfa.simulate(&mut str.chars()),
                expected.as_ref(),
                "NFA simulation failed for string: {:?}",
                str
            );
        }

        let dfa = nfa_to_dfa(nfa);

        println!("DFA=\n{}", dfa);

        for (str, expected) in &test_cases {
            assert_eq!(
                dfa.simulate(&mut str.chars()),
                expected.as_ref(),
                "DFA simulation failed for string: {:?}",
                str
            );
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
    fn simulate_or_one_or_more_char() {
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
    fn simulate_multiple_accepting_states_1() {
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
    fn simulate_variables() {
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

    #[test]
    fn zero_or_more_concat_confusion_1() {
        let mut nfa: NFA<()> = NFA::new();

        let re = Regex::Concat(
            Box::new(Regex::ZeroOrMore(Box::new(Regex::Char('a')))),
            Box::new(Regex::Char('a')),
        );

        nfa.add_regex(&Default::default(), &re, ());

        test_simulate(&nfa, vec![("a", Some(())), ("aa", Some(()))]);
    }

    #[test]
    fn zero_or_more_concat_confusion_2() {
        let mut nfa: NFA<()> = NFA::new();

        let re = Regex::Concat(
            Box::new(Regex::ZeroOrMore(Box::new(Regex::Char('a')))),
            Box::new(Regex::String("ab".to_owned())),
        );

        nfa.add_regex(&Default::default(), &re, ());

        test_simulate(&nfa, vec![("ab", Some(())), ("aab", Some(()))]);
    }

    #[test]
    fn zero_or_more_concat_confusion_3() {
        let mut nfa: NFA<()> = NFA::new();

        let re = Regex::Concat(
            Box::new(Regex::Concat(
                Box::new(Regex::Char('a')),
                Box::new(Regex::ZeroOrMore(Box::new(Regex::Char('a')))),
            )),
            Box::new(Regex::Char('a')),
        );

        nfa.add_regex(&Default::default(), &re, ());

        test_simulate(&nfa, vec![("a", None), ("aa", Some(())), ("aaa", Some(()))]);
    }

    // #[test]
    // fn zero_or_more_wildcard_confusion_1() {
    //     let mut nfa: NFA<()> = NFA::new();

    //     let re = Regex::Concat(
    //         Box::new(Regex::ZeroOrMore(Box::new(Regex::Wildcard))),
    //         Box::new(Regex::String("aa".to_owned())),
    //     );

    //     nfa.add_regex(&Default::default(), &re, ());

    //     test_simulate(&nfa, vec![("aa", Some(()))]);
    // }

    // #[test]
    // fn simulate_wildcards() {
    //     let mut nfa: NFA<()> = NFA::new();

    //     let re = Regex::Concat(
    //         Box::new(Regex::String("/*".to_owned())),
    //         Box::new(Regex::Concat(
    //             Box::new(Regex::ZeroOrMore(Box::new(Regex::Wildcard))),
    //             Box::new(Regex::String("*/".to_owned())),
    //         )),
    //     );

    //     nfa.add_regex(&Default::default(), &re, ());

    //     test_simulate(
    //         &nfa,
    //         vec![
    //             ("/**/", Some(())),
    //             ("/* */", Some(())),
    //             ("/***/", Some(())),
    //             ("/* test */", Some(())),
    //             ("/**test**/", Some(())),
    //             ("/*/", None),
    //             ("/**", None),
    //             ("/** /", None),
    //             ("/ **/", None),
    //         ],
    //     );
    // }

    #[test]
    fn simulate_fail() {
        let mut nfa: NFA<usize> = NFA::new();

        nfa.add_regex(&Default::default(), &Regex::String("ab".to_owned()), 1);
        nfa.add_regex(&Default::default(), &Regex::Fail, 2);

        test_simulate(&nfa, vec![("a", None), ("ab", Some(1))]);
    }

    #[test]
    fn range_and_char_confusion() {
        let mut nfa: NFA<usize> = NFA::new();

        nfa.add_regex(&Default::default(), &Regex::String("ab".to_owned()), 1);
        nfa.add_regex(
            &Default::default(),
            &Regex::OneOrMore(Box::new(Regex::CharSet(CharSet(vec![CharOrRange::Range(
                'a', 'z',
            )])))),
            2,
        );

        test_simulate(&nfa, vec![("ab", Some(1)), ("ac", Some(2))]);
    }
}
