use std::ops::RangeInclusive;

use crate::DFA;

const CHAR_GAP: RangeInclusive<u32> = 0xD800..=0xDFFF;

/// Move range ends out of char gaps
pub fn normalise_ranges<T, A>(dfa: &mut DFA<T, A>) {
    for state in dfa.states.iter_mut() {
        for range in state.range_transitions.iter_mut() {
            if CHAR_GAP.contains(&range.start) {
                range.start = CHAR_GAP.end() + 1;
            }

            if CHAR_GAP.contains(&range.end) {
                range.end = CHAR_GAP.start() - 1;
            }
        }
    }
}
