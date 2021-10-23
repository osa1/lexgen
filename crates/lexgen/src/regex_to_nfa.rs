use crate::ast::{CharOrRange, Regex, Var};
use crate::builtin::BUILTIN_RANGES;
use crate::nfa::{StateIdx, NFA};

use std::convert::TryFrom;

use fxhash::FxHashMap;

pub fn add_re<A>(
    nfa: &mut NFA<A>,
    bindings: &FxHashMap<Var, Regex>,
    re: &Regex,
    current: StateIdx,
    cont: StateIdx,
) {
    match re {
        Regex::Builtin(builtin_name) => {
            let builtin = BUILTIN_RANGES.iter().find_map(|(name, builtin_)| {
                if *name == builtin_name.0 {
                    Some(*builtin_)
                } else {
                    None
                }
            });

            let builtin =
                builtin.unwrap_or_else(|| panic!("Unknown builtin regex: {}", builtin_name.0));

            for (range_start, range_end) in builtin.get_ranges() {
                nfa.add_range_transition(
                    current,
                    char::try_from(*range_start).unwrap(),
                    char::try_from(*range_end).unwrap(),
                    cont,
                );
            }
        }

        Regex::Var(var) => {
            let re = bindings
                .get(var)
                .unwrap_or_else(|| panic!("Unbound variable {:?}", var.0));

            add_re(nfa, bindings, re, current, cont);
        }

        Regex::Char(char) => {
            nfa.add_char_transition(current, *char, cont);
        }

        Regex::String(str) => {
            let mut iter = str.chars().peekable();
            let mut current = current;
            while let Some(char) = iter.next() {
                let next = if iter.peek().is_some() {
                    nfa.new_state()
                } else {
                    cont
                };
                nfa.add_char_transition(current, char, next);
                current = next;
            }
        }

        Regex::CharSet(set) => {
            for char in &set.0 {
                match char {
                    CharOrRange::Char(char) => {
                        nfa.add_char_transition(current, *char, cont);
                    }
                    CharOrRange::Range(range_start, range_end) => {
                        nfa.add_range_transition(current, *range_start, *range_end, cont);
                    }
                }
            }
        }

        Regex::ZeroOrMore(re) => {
            let re_init = nfa.new_state();
            let re_cont = nfa.new_state();
            add_re(nfa, bindings, re, re_init, re_cont);
            nfa.add_empty_transition(current, cont);
            nfa.add_empty_transition(current, re_init);
            nfa.add_empty_transition(re_cont, cont);
            nfa.add_empty_transition(re_cont, re_init);
        }

        Regex::OneOrMore(re) => {
            let re_init = nfa.new_state();
            let re_cont = nfa.new_state();
            add_re(nfa, bindings, re, re_init, re_cont);
            nfa.add_empty_transition(current, re_init);
            nfa.add_empty_transition(re_cont, cont);
            nfa.add_empty_transition(re_cont, re_init);
        }

        Regex::ZeroOrOne(re) => {
            let re_init = nfa.new_state();
            add_re(nfa, bindings, re, re_init, cont);
            nfa.add_empty_transition(current, cont);
            nfa.add_empty_transition(current, re_init);
        }

        Regex::Concat(re1, re2) => {
            let re1_cont = nfa.new_state();
            add_re(nfa, bindings, re1, current, re1_cont);
            add_re(nfa, bindings, re2, re1_cont, cont);
        }

        Regex::Or(re1, re2) => {
            let re1_init = nfa.new_state();
            let re2_init = nfa.new_state();
            add_re(nfa, bindings, re1, re1_init, cont);
            add_re(nfa, bindings, re2, re2_init, cont);
            nfa.add_empty_transition(current, re1_init);
            nfa.add_empty_transition(current, re2_init);
        }

        Regex::Any => {
            nfa.add_any_transition(current, cont);
        }

        Regex::EndOfInput => {
            nfa.add_end_of_input_transition(current, cont);
        }
    }
}
