use crate::ast::{Builtin, CharSet, Regex, Var};
use crate::builtin::{BuiltinCharRange, BUILTIN_RANGES};
use crate::collections::Map;
use crate::nfa::{StateIdx, NFA};
use crate::range_map::RangeMap;

pub fn add_re<A>(
    nfa: &mut NFA<A>,
    bindings: &Map<Var, Regex>,
    re: &Regex,
    current: StateIdx,
    cont: StateIdx,
) {
    match re {
        Regex::Builtin(builtin_name) => {
            let builtin = get_builtin_regex(builtin_name);

            for (range_start, range_end) in builtin.get_ranges() {
                nfa.add_range_transition(current, *range_start, *range_end, cont);
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

        Regex::CharSet(charset) => {
            let range_map = charset_to_range_map(bindings, charset);
            nfa.add_range_transitions(current, &range_map, cont);
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

fn get_builtin_regex(builtin: &Builtin) -> BuiltinCharRange {
    BUILTIN_RANGES
        .iter()
        .find_map(|(name, builtin_)| {
            if *name == builtin.0 {
                Some(*builtin_)
            } else {
                None
            }
        })
        .unwrap_or_else(|| panic!("Unknown builtin regex: {}", builtin.0))
}

fn charset_to_range_map(bindings: &Map<Var, Regex>, charset: &CharSet) -> RangeMap<()> {
    match charset {
        CharSet::Var(var) => {
            let re = bindings
                .get(var)
                .unwrap_or_else(|| panic!("Unbound variable {:?}", var.0));

            regex_to_range_map(bindings, re)
        }

        CharSet::Char(char) => {
            let mut map = RangeMap::new();
            map.insert(*char as u32, *char as u32, (), merge_values);
            map
        }

        CharSet::Range(range_start, range_end) => {
            let mut map = RangeMap::new();
            map.insert(*range_start as u32, *range_end as u32, (), merge_values);
            map
        }

        CharSet::Or(charset1, charset2) => {
            let mut map1 = charset_to_range_map(bindings, charset1);
            let map2 = charset_to_range_map(bindings, charset2);

            for range in map2.into_iter() {
                map1.insert(range.start, range.end, range.value, merge_values);
            }

            map1
        }

        CharSet::Diff(charset1, charset2) => {
            let mut map1 = charset_to_range_map(bindings, charset1);
            let map2 = charset_to_range_map(bindings, charset2);

            map1.remove_ranges(&map2);

            map1
        }
    }
}

fn regex_to_range_map(bindings: &Map<Var, Regex>, re: &Regex) -> RangeMap<()> {
    match re {
        Regex::Var(var) => {
            let re = bindings
                .get(var)
                .unwrap_or_else(|| panic!("Unbound variable {:?}", var.0));

            regex_to_range_map(bindings, re)
        }

        Regex::Builtin(builtin_name) => {
            let builtin = get_builtin_regex(builtin_name);

            let mut map = RangeMap::new();

            // TODO: Quadratic behavior below, `RangeMap::insert` is O(number of ranges).
            for (range_start, range_end) in builtin.get_ranges() {
                map.insert(*range_start, *range_end, (), merge_values);
            }

            map
        }

        Regex::CharSet(charset) => charset_to_range_map(bindings, charset),

        // TODO: Some of the cases below could be handled
        Regex::Char(char) => panic!("Regex {:?} cannot be converted to charset", char),
        Regex::String(str) => panic!("Regex {:?} cannot be converted to charset", str),
        Regex::ZeroOrMore(_) => panic!("Regex with '*' cannot be converted to charset"),
        Regex::OneOrMore(_) => panic!("Regex with '+' cannot be converted to charset"),
        Regex::ZeroOrOne(_) => panic!("Regex with '?' cannot be converted to charset"),
        Regex::Concat(_, _) => panic!("Regex with concatenation cannot be converted to charset"),
        Regex::Or(_, _) => panic!("Regex with `|` cannot be converted to charset"),
        Regex::Any => panic!("Regex '_' cannot be converted to charset"),
        Regex::EndOfInput => panic!("Regex '$' cannot be converted to charset"),
    }
}

fn merge_values(_val1: &mut (), _val2: ()) {}
