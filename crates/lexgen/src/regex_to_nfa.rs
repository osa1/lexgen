use crate::ast::{Builtin, CharOrRange, Regex, Var};
use crate::builtin::{BuiltinCharRange, BUILTIN_RANGES};
use crate::collections::Map;
use crate::nfa::{StateIdx, NFA};
use crate::range_map::{Range, RangeMap};

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

            let ranges: Vec<Range<()>> = builtin
                .get_ranges()
                .iter()
                .copied()
                .map(|(start, end)| Range {
                    start,
                    end,
                    value: (),
                })
                .collect();

            let map = RangeMap::from_non_overlapping_sorted_ranges(ranges);

            nfa.add_range_transitions(current, map, cont);
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

        Regex::NotCharSet(set) => {
            let mut ranges = RangeMap::new();

            for char in &set.0 {
                match char {
                    CharOrRange::Char(char) => {
                        ranges.insert(*char as u32, *char as u32, (), merge_values);
                    }
                    CharOrRange::Range(range_start, range_end) => {
                        ranges.insert(*range_start as u32, *range_end as u32, (), merge_values);
                    }
                }
            }

            let mut map = RangeMap::new();

            map.insert(0, char::MAX as u32, (), merge_values);
            map.remove_ranges(&ranges);

            nfa.add_range_transitions(current, map, cont);
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

        Regex::Diff(_, _) => {
            let map = regex_to_range_map(bindings, re);
            nfa.add_range_transitions(current, map, cont);
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

fn regex_to_range_map(bindings: &Map<Var, Regex>, re: &Regex) -> RangeMap<()> {
    match re {
        Regex::Builtin(builtin) => {
            let builtin = get_builtin_regex(builtin);
            let ranges: Vec<Range<()>> = builtin
                .get_ranges()
                .iter()
                .copied()
                .map(|(start, end)| Range {
                    start,
                    end,
                    value: (),
                })
                .collect();
            RangeMap::from_non_overlapping_sorted_ranges(ranges)
        }

        Regex::Var(var) => {
            let re = bindings
                .get(var)
                .unwrap_or_else(|| panic!("Unbound variable {:?}", var.0));

            regex_to_range_map(bindings, re)
        }

        Regex::Char(char) => {
            let mut map = RangeMap::new();
            map.insert(*char as u32, *char as u32, (), merge_values);
            map
        }

        Regex::NotCharSet(_) => panic!("`^` cannot be used in char sets (`#`)"),

        Regex::String(_) => panic!("strings cannot be used in char sets (`#`)"),

        Regex::CharSet(char_set) => {
            let mut map = RangeMap::new();

            // TODO: Quadratic behavior below, `RangeMap::insert` is O(number of ranges)
            for char_or_range in char_set.0.iter() {
                match char_or_range {
                    CharOrRange::Char(char) => {
                        map.insert(*char as u32, *char as u32, (), merge_values);
                    }
                    CharOrRange::Range(start, end) => {
                        map.insert(*start as u32, *end as u32, (), merge_values);
                    }
                }
            }

            map
        }

        Regex::ZeroOrMore(_) => {
            panic!("`*` cannot be used in char sets (`#`)");
        }

        Regex::OneOrMore(_) => {
            panic!("`+` cannot be used in char sets (`#`)");
        }

        Regex::ZeroOrOne(_) => {
            panic!("`?` cannot be used in char sets (`#`)");
        }

        Regex::Concat(_, _) => {
            panic!("concatenation (`<re1> <re2>`) cannot be used in char sets (`#`)");
        }

        Regex::Or(re1, re2) => {
            let mut map1 = regex_to_range_map(bindings, re1);
            let map2 = regex_to_range_map(bindings, re2);

            map1.insert_ranges(map2.into_iter(), merge_values);

            map1
        }

        Regex::Any => {
            let mut map = RangeMap::new();
            map.insert(0, char::MAX as u32, (), merge_values);
            map
        }

        Regex::EndOfInput => panic!("`$` cannot be used in char sets (`#`)"),

        Regex::Diff(re1, re2) => {
            let mut map1 = regex_to_range_map(bindings, re1);
            let map2 = regex_to_range_map(bindings, re2);
            map1.remove_ranges(&map2);
            map1
        }
    }
}

fn merge_values(_val1: &mut (), _val2: ()) {}
