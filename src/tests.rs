use crate::ast::{CharOrRange, CharSet, Regex, Var};
use crate::nfa::simulate::{Error, SimulationOutput, Value};
use crate::nfa::NFA;
use crate::nfa_to_dfa::nfa_to_dfa;

use fxhash::FxHashMap;

fn test_simulate<'input, A: Copy + std::fmt::Debug + Eq>(
    nfa: &NFA<A>,
    test_cases: Vec<(&'input str, SimulationOutput<'input, A>)>,
) {
    println!("NFA=\n{}", nfa);

    for (str, expected) in &test_cases {
        assert_eq!(
            &nfa.simulate(str),
            expected,
            "NFA simulation failed for string: {:?}",
            str
        );
    }

    let dfa = nfa_to_dfa(nfa);

    println!("DFA=\n{}", dfa);

    for (str, expected) in &test_cases {
        assert_eq!(
            &dfa.simulate(str),
            expected,
            "DFA simulation failed for string: {:?}",
            str
        );
    }
}

#[test]
fn simulate_char() {
    let re = Regex::Char('a');
    let mut nfa: NFA<usize> = NFA::new();
    nfa.add_regex(&Default::default(), &re, 1);
    test_simulate(
        &nfa,
        vec![
            // TODO: EOF handling
            // (
            //     "",
            //     SimulationOutput {
            //         values: vec![],
            //         error: None,
            //     },
            // ),
            (
                "aa",
                SimulationOutput {
                    values: vec![
                        Value {
                            value: 1,
                            matched_str: "a",
                        },
                        Value {
                            value: 1,
                            matched_str: "a",
                        },
                    ],
                    error: None,
                },
            ),
            (
                "b",
                SimulationOutput {
                    values: vec![],
                    error: Some(Error { loc: 0 }),
                },
            ),
        ],
    );
}

#[test]
fn simulate_string() {
    let re = Regex::String("ab".to_owned());
    let mut nfa: NFA<usize> = NFA::new();
    nfa.add_regex(&Default::default(), &re, 1);
    test_simulate(
        &nfa,
        vec![
            // TODO
            // (
            //     "",
            //     SimulationOutput {
            //         values: vec![],
            //         error: None,
            //     },
            // ),
            (
                "a",
                SimulationOutput {
                    values: vec![],
                    error: Some(Error { loc: 0 }),
                },
            ),
            (
                "ab",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "ab",
                    }],
                    error: None,
                },
            ),
            (
                "abc",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "ab",
                    }],
                    error: Some(Error { loc: 2 }),
                },
            ),
        ],
    );
}

#[test]
fn simulate_char_set_char() {
    let re = Regex::CharSet(CharSet(vec![
        CharOrRange::Char('a'),
        CharOrRange::Char('b'),
    ]));
    let mut nfa: NFA<usize> = NFA::new();
    nfa.add_regex(&Default::default(), &re, 1);
    test_simulate(
        &nfa,
        vec![
            // TODO
            // (
            //     "",
            //     SimulationOutput {
            //         values: vec![],
            //         error: None,
            //     }
            // ),
            (
                "a",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "a",
                    }],
                    error: None,
                },
            ),
            (
                "b",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "b",
                    }],
                    error: None,
                },
            ),
            (
                "ab",
                SimulationOutput {
                    values: vec![
                        Value {
                            value: 1,
                            matched_str: "a",
                        },
                        Value {
                            value: 1,
                            matched_str: "b",
                        },
                    ],
                    error: None,
                },
            ),
            (
                "ba",
                SimulationOutput {
                    values: vec![
                        Value {
                            value: 1,
                            matched_str: "b",
                        },
                        Value {
                            value: 1,
                            matched_str: "a",
                        },
                    ],
                    error: None,
                },
            ),
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
    let mut nfa: NFA<usize> = NFA::new();
    nfa.add_regex(&Default::default(), &re, 1);
    test_simulate(
        &nfa,
        vec![
            // TODO
            // (
            //     "",
            //     None
            // ),
            (
                "a",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "a",
                    }],
                    error: None,
                },
            ),
            (
                "b",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "b",
                    }],
                    error: None,
                },
            ),
            (
                "0",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "0",
                    }],
                    error: None,
                },
            ),
            (
                "1",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "1",
                    }],
                    error: None,
                },
            ),
            (
                "9",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "9",
                    }],
                    error: None,
                },
            ),
            (
                "ba",
                SimulationOutput {
                    values: vec![
                        Value {
                            value: 1,
                            matched_str: "b",
                        },
                        Value {
                            value: 1,
                            matched_str: "a",
                        },
                    ],
                    error: None,
                },
            ),
        ],
    );
}

#[test]
fn simulate_zero_or_more() {
    let re = Regex::ZeroOrMore(Box::new(Regex::Char('a')));
    let mut nfa: NFA<usize> = NFA::new();
    nfa.add_regex(&Default::default(), &re, 1);
    test_simulate(
        &nfa,
        vec![
            // TODO
            // (
            //     "",
            //     Some(())
            // ),
            (
                "a",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "a",
                    }],
                    error: None,
                },
            ),
            (
                "aa",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "aa",
                    }],
                    error: None,
                },
            ),
            (
                "aab",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "aa",
                    }],
                    error: Some(Error { loc: 2 }),
                },
            ),
        ],
    );
}

#[test]
fn simulate_one_or_more() {
    let re = Regex::OneOrMore(Box::new(Regex::Char('a')));
    let mut nfa: NFA<usize> = NFA::new();
    nfa.add_regex(&Default::default(), &re, 1);
    test_simulate(
        &nfa,
        vec![
            // TODO
            // (
            //     "",
            //     None
            // ),
            (
                "a",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "a",
                    }],
                    error: None,
                },
            ),
            (
                "aa",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "aa",
                    }],
                    error: None,
                },
            ),
            (
                "aab",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "aa",
                    }],
                    error: Some(Error { loc: 2 }),
                },
            ),
        ],
    );
}

#[test]
fn simulate_zero_or_one() {
    let re = Regex::ZeroOrOne(Box::new(Regex::Char('a')));
    let mut nfa: NFA<usize> = NFA::new();
    nfa.add_regex(&Default::default(), &re, 1);
    test_simulate(
        &nfa,
        vec![
            // TODO
            // (
            //     "",
            //     None
            // ),
            (
                "a",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "a",
                    }],
                    error: None,
                },
            ),
            (
                "aa",
                SimulationOutput {
                    values: vec![
                        Value {
                            value: 1,
                            matched_str: "a",
                        },
                        Value {
                            value: 1,
                            matched_str: "a",
                        },
                    ],
                    error: None,
                },
            ),
        ],
    );
}

#[test]
fn simulate_concat() {
    let re = Regex::Concat(Box::new(Regex::Char('a')), Box::new(Regex::Char('b')));
    let mut nfa: NFA<usize> = NFA::new();
    nfa.add_regex(&Default::default(), &re, 1);
    test_simulate(
        &nfa,
        vec![
            // TODO
            // (
            //     "",
            //     None
            // ),
            (
                "a",
                SimulationOutput {
                    values: vec![],
                    error: Some(Error { loc: 0 }),
                },
            ),
            (
                "ab",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "ab",
                    }],
                    error: None,
                },
            ),
            (
                "aba",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "ab",
                    }],
                    error: Some(Error { loc: 2 }),
                },
            ),
        ],
    );
}

#[test]
fn simulate_or() {
    let re = Regex::Or(Box::new(Regex::Char('a')), Box::new(Regex::Char('b')));
    let mut nfa: NFA<usize> = NFA::new();
    nfa.add_regex(&Default::default(), &re, 1);
    test_simulate(
        &nfa,
        vec![
            // TODO
            // (
            //     "",
            //     None
            // ),
            (
                "a",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "a",
                    }],
                    error: None,
                },
            ),
            (
                "b",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "b",
                    }],
                    error: None,
                },
            ),
            (
                "ab",
                SimulationOutput {
                    values: vec![
                        Value {
                            value: 1,
                            matched_str: "a",
                        },
                        Value {
                            value: 1,
                            matched_str: "b",
                        },
                    ],
                    error: None,
                },
            ),
        ],
    );
}

#[test]
fn simulate_or_one_or_more_char() {
    let re = Regex::Or(
        Box::new(Regex::OneOrMore(Box::new(Regex::Char('a')))),
        Box::new(Regex::Char('b')),
    );
    let mut nfa: NFA<usize> = NFA::new();
    nfa.add_regex(&Default::default(), &re, 1);
    test_simulate(
        &nfa,
        vec![
            // TODO
            // (
            //     "",
            //     None
            // ),
            (
                "a",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "a",
                    }],
                    error: None,
                },
            ),
            (
                "b",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "b",
                    }],
                    error: None,
                },
            ),
            (
                "aa",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "aa",
                    }],
                    error: None,
                },
            ),
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
            (
                "aaaa",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "aaaa",
                    }],
                    error: None,
                },
            ),
            (
                "aaab",
                SimulationOutput {
                    values: vec![Value {
                        value: 2,
                        matched_str: "aaab",
                    }],
                    error: None,
                },
            ),
            (
                "aaac",
                SimulationOutput {
                    values: vec![],
                    error: Some(Error { loc: 0 }),
                },
            ),
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
            (
                "b",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "b",
                    }],
                    error: None,
                },
            ),
            (
                "a",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "a",
                    }],
                    error: None,
                },
            ),
            (
                "aa",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "aa",
                    }],
                    error: None,
                },
            ),
            // TODO: EOF handling
            // (
            //     "",
            //     SimulationOutput {
            //         values: vec![],
            //         error: None,
            //     },
            // ),
            (
                "0",
                SimulationOutput {
                    values: vec![Value {
                        value: 2,
                        matched_str: "0",
                    }],
                    error: None,
                },
            ),
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
    let mut nfa: NFA<usize> = NFA::new();
    nfa.add_regex(&bindings, &re, 1);

    test_simulate(
        &nfa,
        vec![
            (
                "a",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "a",
                    }],
                    error: None,
                },
            ),
            (
                "aA",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "aA",
                    }],
                    error: None,
                },
            ),
            (
                "aA123-a",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "aA123-a",
                    }],
                    error: None,
                },
            ),
        ],
    );
}

#[test]
fn zero_or_more_concat_confusion_1() {
    let mut nfa: NFA<usize> = NFA::new();

    let re = Regex::Concat(
        Box::new(Regex::ZeroOrMore(Box::new(Regex::Char('a')))),
        Box::new(Regex::Char('a')),
    );

    nfa.add_regex(&Default::default(), &re, 1);

    test_simulate(
        &nfa,
        vec![
            (
                "a",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "a",
                    }],
                    error: None,
                },
            ),
            (
                "aa",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "aa",
                    }],
                    error: None,
                },
            ),
        ],
    );
}

#[test]
fn zero_or_more_concat_confusion_2() {
    let mut nfa: NFA<usize> = NFA::new();

    let re = Regex::Concat(
        Box::new(Regex::ZeroOrMore(Box::new(Regex::Char('a')))),
        Box::new(Regex::String("ab".to_owned())),
    );

    nfa.add_regex(&Default::default(), &re, 1);

    test_simulate(
        &nfa,
        vec![
            (
                "ab",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "ab",
                    }],
                    error: None,
                },
            ),
            (
                "aab",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "aab",
                    }],
                    error: None,
                },
            ),
        ],
    );
}

#[test]
fn zero_or_more_concat_confusion_3() {
    let mut nfa: NFA<usize> = NFA::new();

    let re = Regex::Concat(
        Box::new(Regex::Concat(
            Box::new(Regex::Char('a')),
            Box::new(Regex::ZeroOrMore(Box::new(Regex::Char('a')))),
        )),
        Box::new(Regex::Char('a')),
    );

    nfa.add_regex(&Default::default(), &re, 1);

    test_simulate(
        &nfa,
        vec![
            (
                "a",
                SimulationOutput {
                    values: vec![],
                    error: Some(Error { loc: 0 }),
                },
            ),
            (
                "aa",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "aa",
                    }],
                    error: None,
                },
            ),
            (
                "aaa",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "aaa",
                    }],
                    error: None,
                },
            ),
        ],
    );
}

#[test]
fn simulate_fail() {
    let mut nfa: NFA<usize> = NFA::new();

    nfa.add_regex(&Default::default(), &Regex::String("ab".to_owned()), 1);
    nfa.set_fail_action(2);

    test_simulate(
        &nfa,
        vec![
            (
                "a",
                SimulationOutput {
                    values: vec![],
                    error: Some(Error { loc: 0 }),
                },
            ),
            (
                "ab",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "ab",
                    }],
                    error: None,
                },
            ),
        ],
    );
}

#[test]
fn simulate_multiple_accepting_states_3() {
    let mut nfa: NFA<usize> = NFA::new();

    nfa.add_regex(&Default::default(), &Regex::String("aaa".to_owned()), 1);
    nfa.add_regex(&Default::default(), &Regex::String("aaa".to_owned()), 2);
    nfa.add_regex(&Default::default(), &Regex::String("aa".to_owned()), 3);
    nfa.set_fail_action(4);

    test_simulate(
        &nfa,
        vec![
            (
                "aaa",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "aaa",
                    }],
                    error: None,
                },
            ),
            (
                "aa",
                SimulationOutput {
                    values: vec![Value {
                        value: 3,
                        matched_str: "aa",
                    }],
                    error: None,
                },
            ),
            // TODO: fail actions
            // (
            //     "a",
            //     SimulationOutput {
            //         values: vec![Value {
            //             value: 4,
            //             matched_str: "a",
            //         }],
            //         error: None,
            //     },
            // ),
        ],
    );
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

    test_simulate(
        &nfa,
        vec![
            (
                "ab",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "ab",
                    }],
                    error: None,
                },
            ),
            (
                "ac",
                SimulationOutput {
                    values: vec![Value {
                        value: 2,
                        matched_str: "ac",
                    }],
                    error: None,
                },
            ),
        ],
    );
}

#[test]
fn overlapping_ranges() {
    let mut nfa: NFA<usize> = NFA::new();

    nfa.add_regex(
        &Default::default(),
        &Regex::Concat(
            Box::new(Regex::CharSet(CharSet(vec![CharOrRange::Range('a', 'b')]))),
            Box::new(Regex::Char('1')),
        ),
        1,
    );
    nfa.add_regex(
        &Default::default(),
        &Regex::Concat(
            Box::new(Regex::CharSet(CharSet(vec![CharOrRange::Range('a', 'c')]))),
            Box::new(Regex::Char('2')),
        ),
        2,
    );

    test_simulate(
        &nfa,
        vec![
            (
                "a1",
                SimulationOutput {
                    values: vec![Value {
                        value: 1,
                        matched_str: "a1",
                    }],
                    error: None,
                },
            ),
            (
                "a2",
                SimulationOutput {
                    values: vec![Value {
                        value: 2,
                        matched_str: "a2",
                    }],
                    error: None,
                },
            ),
        ],
    );
}
