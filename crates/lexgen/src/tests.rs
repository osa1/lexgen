use crate::ast::{CharOrRange, CharSet, Regex, Var};
use crate::collections::Map;
use crate::nfa::simulate::{ErrorLoc, Matches};
use crate::nfa::NFA;
use crate::nfa_to_dfa::nfa_to_dfa;
use crate::right_ctx::RightCtxDFAs;

fn test_simulate<'input, A: Copy + std::fmt::Debug + Eq>(
    nfa: &NFA<A>,
    test_cases: Vec<(&'input str, Matches<'input, A>, Option<ErrorLoc>)>,
) {
    test_simulate_right_ctx(nfa, &Default::default(), test_cases)
}

fn test_simulate_right_ctx<'input, A: Copy + std::fmt::Debug + Eq>(
    nfa: &NFA<A>,
    right_ctx_dfas: &RightCtxDFAs,
    test_cases: Vec<(&'input str, Matches<'input, A>, Option<ErrorLoc>)>,
) {
    println!("NFA=\n{}", nfa);

    let dfa = nfa_to_dfa(nfa);

    println!("DFA=\n{}", dfa);

    for (str, expected_matches, expected_error) in test_cases {
        let expected = (expected_matches, expected_error);

        assert_eq!(
            &nfa.simulate(str, right_ctx_dfas),
            &expected,
            "NFA simulation failed for string: {:?}",
            str
        );

        assert_eq!(
            dfa.simulate(str, right_ctx_dfas),
            expected,
            "DFA simulation failed for string: {:?}",
            str
        );
    }
}

#[test]
fn simulate_backtracking() {
    let mut nfa: NFA<usize> = NFA::new();

    nfa.add_regex(
        &Default::default(),
        &Regex::Concat(
            Box::new(Regex::OneOrMore(Box::new(Regex::Char('a')))),
            Box::new(Regex::Char('b')),
        ),
        None,
        1,
    );

    nfa.add_regex(&Default::default(), &Regex::Char('a'), None, 2);

    test_simulate(
        &nfa,
        vec![
            ("a", vec![("a", 2)], None),
            ("aa", vec![("a", 2), ("a", 2)], None),
            ("aab", vec![("aab", 1)], None),
        ],
    );
}

#[test]
fn issue_16() {
    let mut nfa: NFA<usize> = NFA::new();

    nfa.add_regex(
        &Default::default(),
        &Regex::String("xyzxyz".to_owned()),
        None,
        1,
    );
    nfa.add_regex(
        &Default::default(),
        &Regex::String("xyz".to_owned()),
        None,
        2,
    );
    nfa.add_regex(
        &Default::default(),
        &Regex::String("xya".to_owned()),
        None,
        3,
    );

    test_simulate(
        &nfa,
        vec![
            ("xyzxya", vec![("xyz", 2), ("xya", 3)], None),
            ("xyzxyz", vec![("xyzxyz", 1)], None),
        ],
    );
}

#[test]
fn stuck_1() {
    let nfa: NFA<usize> = NFA::new();
    test_simulate(&nfa, vec![("a", vec![], Some(0))]);
}

#[test]
fn stuck_2() {
    let mut nfa: NFA<usize> = NFA::new();

    nfa.add_regex(
        &Default::default(),
        &Regex::String("ab".to_owned()),
        None,
        1,
    );

    test_simulate(&nfa, vec![("aba", vec![("ab", 1)], Some(2))]);
}

#[test]
fn stuck_3() {
    let mut nfa: NFA<usize> = NFA::new();

    nfa.add_regex(
        &Default::default(),
        &Regex::String("aaab".to_owned()),
        None,
        1,
    );
    nfa.add_regex(&Default::default(), &Regex::String("a".to_owned()), None, 2);

    test_simulate(&nfa, vec![("aaabb", vec![("aaab", 1)], Some(4))]);
}

#[test]
fn simulate_char() {
    let re = Regex::Char('a');
    let mut nfa: NFA<usize> = NFA::new();
    nfa.add_regex(&Default::default(), &re, None, 1);

    test_simulate(
        &nfa,
        vec![
            ("aa", vec![("a", 1), ("a", 1)], None),
            ("b", vec![], Some(0)),
        ],
    );
}

#[test]
fn simulate_string() {
    let re = Regex::String("ab".to_owned());
    let mut nfa: NFA<usize> = NFA::new();
    nfa.add_regex(&Default::default(), &re, None, 1);

    test_simulate(
        &nfa,
        vec![
            ("a", vec![], Some(0)),
            ("ab", vec![("ab", 1)], None),
            ("abc", vec![("ab", 1)], Some(2)),
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
    nfa.add_regex(&Default::default(), &re, None, 1);

    test_simulate(
        &nfa,
        vec![
            ("a", vec![("a", 1)], None),
            ("b", vec![("b", 1)], None),
            ("ab", vec![("a", 1), ("b", 1)], None),
            ("ba", vec![("b", 1), ("a", 1)], None),
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
    nfa.add_regex(&Default::default(), &re, None, 1);

    test_simulate(
        &nfa,
        vec![("ab09", vec![("a", 1), ("b", 1), ("0", 1), ("9", 1)], None)],
    );
}

#[test]
fn simulate_zero_or_more() {
    let re = Regex::ZeroOrMore(Box::new(Regex::Char('a')));
    let mut nfa: NFA<usize> = NFA::new();
    nfa.add_regex(&Default::default(), &re, None, 1);

    test_simulate(
        &nfa,
        vec![
            // TODO
            // ("", vec![], None),
            ("a", vec![("a", 1)], None),
            ("aa", vec![("aa", 1)], None),
            ("aab", vec![("aa", 1)], Some(2)),
        ],
    );
}

#[test]
fn simulate_one_or_more() {
    let re = Regex::OneOrMore(Box::new(Regex::Char('a')));
    let mut nfa: NFA<usize> = NFA::new();
    nfa.add_regex(&Default::default(), &re, None, 1);

    test_simulate(
        &nfa,
        vec![
            ("", vec![], Some(0)),
            ("a", vec![("a", 1)], None),
            ("aa", vec![("aa", 1)], None),
            ("aab", vec![("aa", 1)], Some(2)),
        ],
    );
}

#[test]
fn simulate_zero_or_one() {
    let re = Regex::ZeroOrOne(Box::new(Regex::Char('a')));
    let mut nfa: NFA<usize> = NFA::new();
    nfa.add_regex(&Default::default(), &re, None, 1);

    test_simulate(
        &nfa,
        vec![
            ("", vec![], Some(0)),
            ("a", vec![("a", 1)], None),
            ("aa", vec![("a", 1), ("a", 1)], None),
            ("aab", vec![("a", 1), ("a", 1)], Some(2)),
        ],
    );
}

#[test]
fn simulate_concat() {
    let re = Regex::Concat(Box::new(Regex::Char('a')), Box::new(Regex::Char('b')));
    let mut nfa: NFA<usize> = NFA::new();
    nfa.add_regex(&Default::default(), &re, None, 1);

    test_simulate(
        &nfa,
        vec![
            ("a", vec![], Some(0)),
            ("ab", vec![("ab", 1)], None),
            ("aba", vec![("ab", 1)], Some(2)),
        ],
    );
}

#[test]
fn simulate_or() {
    let re = Regex::Or(Box::new(Regex::Char('a')), Box::new(Regex::Char('b')));
    let mut nfa: NFA<usize> = NFA::new();
    nfa.add_regex(&Default::default(), &re, None, 1);

    test_simulate(
        &nfa,
        vec![
            ("a", vec![("a", 1)], None),
            ("b", vec![("b", 1)], None),
            ("ab", vec![("a", 1), ("b", 1)], None),
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
    nfa.add_regex(&Default::default(), &re, None, 1);

    test_simulate(
        &nfa,
        vec![
            ("a", vec![("a", 1)], None),
            ("b", vec![("b", 1)], None),
            ("aa", vec![("aa", 1)], None),
        ],
    );
}

#[test]
fn simulate_multiple_accepting_states_1() {
    let re1 = Regex::String("aaaa".to_owned());
    let re2 = Regex::String("aaab".to_owned());
    let mut nfa: NFA<usize> = NFA::new();
    nfa.add_regex(&Default::default(), &re1, None, 1);
    nfa.add_regex(&Default::default(), &re2, None, 2);

    test_simulate(
        &nfa,
        vec![
            ("aaaa", vec![("aaaa", 1)], None),
            ("aaab", vec![("aaab", 2)], None),
            ("aaac", vec![], Some(0)),
        ],
    );
}

#[test]
fn multiple_accepting_states_2() {
    let re1 = Regex::Or(
        Box::new(Regex::OneOrMore(Box::new(Regex::Char('a')))),
        Box::new(Regex::Char('b')),
    );
    let re2 = Regex::CharSet(CharSet(vec![CharOrRange::Range('0', '9')]));
    let mut nfa: NFA<usize> = NFA::new();
    nfa.add_regex(&Default::default(), &re1, None, 1);
    nfa.add_regex(&Default::default(), &re2, None, 2);

    test_simulate(
        &nfa,
        vec![
            ("b", vec![("b", 1)], None),
            ("a", vec![("a", 1)], None),
            ("aa", vec![("aa", 1)], None),
            ("0", vec![("0", 2)], None),
        ],
    );
}

#[test]
fn simulate_variables() {
    let mut bindings: Map<Var, Regex> = Default::default();
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
    nfa.add_regex(&bindings, &re, None, 1);

    test_simulate(
        &nfa,
        vec![
            ("a", vec![("a", 1)], None),
            ("aA", vec![("aA", 1)], None),
            ("aA123-a", vec![("aA123-a", 1)], None),
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

    nfa.add_regex(&Default::default(), &re, None, 1);

    test_simulate(
        &nfa,
        vec![("a", vec![("a", 1)], None), ("aa", vec![("aa", 1)], None)],
    );
}

#[test]
fn zero_or_more_concat_confusion_2() {
    let mut nfa: NFA<usize> = NFA::new();

    let re = Regex::Concat(
        Box::new(Regex::ZeroOrMore(Box::new(Regex::Char('a')))),
        Box::new(Regex::String("ab".to_owned())),
    );

    nfa.add_regex(&Default::default(), &re, None, 1);

    test_simulate(
        &nfa,
        vec![
            ("ab", vec![("ab", 1)], None),
            ("aab", vec![("aab", 1)], None),
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

    nfa.add_regex(&Default::default(), &re, None, 1);

    test_simulate(
        &nfa,
        vec![
            ("a", vec![], Some(0)),
            ("aa", vec![("aa", 1)], None),
            ("aaa", vec![("aaa", 1)], None),
        ],
    );
}

#[test]
fn simulate_any_1() {
    let mut nfa: NFA<usize> = NFA::new();

    nfa.add_regex(
        &Default::default(),
        &Regex::String("ab".to_owned()),
        None,
        1,
    );
    nfa.add_regex(&Default::default(), &Regex::Any, None, 2);

    test_simulate(
        &nfa,
        vec![
            ("a", vec![("a", 2)], None),
            ("ab", vec![("ab", 1)], None),
            ("abc", vec![("ab", 1), ("c", 2)], None),
        ],
    );
}

#[test]
fn simulate_any_2() {
    let mut nfa: NFA<usize> = NFA::new();

    nfa.add_regex(
        &Default::default(),
        &Regex::Concat(
            Box::new(Regex::Char('\'')),
            Box::new(Regex::Concat(
                Box::new(Regex::Any),
                Box::new(Regex::Char('\'')),
            )),
        ),
        None,
        1,
    );

    test_simulate(&nfa, vec![("'a'", vec![("'a'", 1)], None)]);
}

#[test]
fn simulate_end_of_input_1() {
    let mut nfa: NFA<usize> = NFA::new();

    // C-style single-line comment syntax: "//" _* ('\n' | $)
    nfa.add_regex(
        &Default::default(),
        &Regex::Concat(
            Box::new(Regex::String("//".to_owned())),
            Box::new(Regex::Concat(
                Box::new(Regex::ZeroOrMore(Box::new(Regex::Any))),
                Box::new(Regex::Or(
                    Box::new(Regex::Char('\n')),
                    Box::new(Regex::EndOfInput),
                )),
            )),
        ),
        None,
        1,
    );

    test_simulate(
        &nfa,
        vec![
            ("//", vec![("//", 1)], None),
            ("//  \n", vec![("//  \n", 1)], None),
            ("//  ", vec![("//  ", 1)], None),
        ],
    );
}

#[test]
fn simulate_end_of_input_2() {
    let mut nfa: NFA<usize> = NFA::new();

    nfa.add_regex(&Default::default(), &Regex::EndOfInput, None, 1);
    nfa.add_regex(
        &Default::default(),
        &Regex::ZeroOrMore(Box::new(Regex::Any)),
        None,
        2,
    );

    // TODO: EndOfInput never matches?
    test_simulate(&nfa, vec![("a", vec![("a", 2)], None)]);
}

#[test]
fn simulate_multiple_accepting_states_3() {
    let mut nfa: NFA<usize> = NFA::new();

    nfa.add_regex(
        &Default::default(),
        &Regex::String("aaa".to_owned()),
        None,
        1,
    );
    nfa.add_regex(
        &Default::default(),
        &Regex::String("aaa".to_owned()),
        None,
        2,
    );
    nfa.add_regex(
        &Default::default(),
        &Regex::String("aa".to_owned()),
        None,
        3,
    );

    test_simulate(
        &nfa,
        vec![
            ("aaa", vec![("aaa", 1)], None),
            ("aa", vec![("aa", 3)], None),
        ],
    );
}

#[test]
fn range_and_char_confusion() {
    let mut nfa: NFA<usize> = NFA::new();

    nfa.add_regex(
        &Default::default(),
        &Regex::String("ab".to_owned()),
        None,
        1,
    );
    nfa.add_regex(
        &Default::default(),
        &Regex::OneOrMore(Box::new(Regex::CharSet(CharSet(vec![CharOrRange::Range(
            'a', 'z',
        )])))),
        None,
        2,
    );

    test_simulate(
        &nfa,
        vec![("ab", vec![("ab", 1)], None), ("ac", vec![("ac", 2)], None)],
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
        None,
        1,
    );
    nfa.add_regex(
        &Default::default(),
        &Regex::Concat(
            Box::new(Regex::CharSet(CharSet(vec![CharOrRange::Range('a', 'c')]))),
            Box::new(Regex::Char('2')),
        ),
        None,
        2,
    );

    test_simulate(
        &nfa,
        vec![("a1", vec![("a1", 1)], None), ("a2", vec![("a2", 2)], None)],
    );
}

#[test]
fn right_context_1() {
    let mut nfa: NFA<usize> = NFA::new();
    let mut right_ctxs = RightCtxDFAs::new();

    let right_ctx = right_ctxs.new_right_ctx(&Default::default(), &Regex::Char('a'));
    nfa.add_regex(&Default::default(), &Regex::Char('a'), Some(right_ctx), 1);

    test_simulate_right_ctx(&nfa, &right_ctxs, vec![("aa", vec![("a", 1)], Some(1))]);
    test_simulate_right_ctx(&nfa, &right_ctxs, vec![("ab", vec![], Some(0))]);
}

#[test]
fn right_context_2() {
    let mut nfa: NFA<usize> = NFA::new();
    let mut right_ctxs = RightCtxDFAs::new();

    let right_ctx = right_ctxs.new_right_ctx(&Default::default(), &Regex::Any);
    nfa.add_regex(&Default::default(), &Regex::Char('a'), Some(right_ctx), 1);

    test_simulate_right_ctx(&nfa, &right_ctxs, vec![("aa", vec![("a", 1)], Some(1))]);
    test_simulate_right_ctx(&nfa, &right_ctxs, vec![("ab", vec![("a", 1)], Some(1))]);
    test_simulate_right_ctx(&nfa, &right_ctxs, vec![("a", vec![], Some(0))]);
}

#[test]
fn right_context_3() {
    let mut nfa: NFA<usize> = NFA::new();
    let mut right_ctxs = RightCtxDFAs::new();

    let right_ctx = right_ctxs.new_right_ctx(&Default::default(), &Regex::EndOfInput);
    nfa.add_regex(&Default::default(), &Regex::Char('a'), Some(right_ctx), 1);

    test_simulate_right_ctx(&nfa, &right_ctxs, vec![("a", vec![("a", 1)], None)]);
    test_simulate_right_ctx(&nfa, &right_ctxs, vec![("ab", vec![], Some(0))]);
}

#[test]
fn right_context_4() {
    let mut nfa: NFA<usize> = NFA::new();
    let mut right_ctxs = RightCtxDFAs::new();

    let right_ctx = right_ctxs.new_right_ctx(&Default::default(), &Regex::Char('a'));
    nfa.add_regex(&Default::default(), &Regex::Char('a'), Some(right_ctx), 1);

    let right_ctx = right_ctxs.new_right_ctx(&Default::default(), &Regex::EndOfInput);
    nfa.add_regex(&Default::default(), &Regex::Char('a'), Some(right_ctx), 2);

    test_simulate_right_ctx(
        &nfa,
        &right_ctxs,
        vec![("aa", vec![("a", 1), ("a", 2)], None)],
    );
}
