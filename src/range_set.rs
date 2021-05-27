use std::cmp::{max, min};

/// A set of inclusive ranges, with insertion and iteration operations. Insertion allows
/// overlapping ranges. When two ranges overlap, value of the overlapping parts is the union of
/// values of the overlapping ranges.
pub struct RangeSet<A> {
    // NB. internally we don't have any overlaps. Overlapping ranges are split into smaller
    // non-overlapping ranges.
    ranges: Vec<Range<A>>,
}

impl<A> RangeSet<A> {
    fn new() -> RangeSet<A> {
        RangeSet {
            ranges: vec![Range {
                start: 0,
                end: usize::MAX,
                values: vec![],
            }],
        }
    }

    fn iter(&self) -> impl Iterator<Item = &Range<A>> {
        self.ranges.iter()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Range<A> {
    start: usize,
    // Inclusive
    end: usize,
    values: Vec<A>,
}

impl<A> Range<A> {
    fn new(start: usize, end: usize, value: A) -> Range<A> {
        Range::with_values(start, end, vec![value])
    }

    fn with_values(start: usize, end: usize, values: Vec<A>) -> Range<A> {
        Range { start, end, values }
    }

    fn add_value(&mut self, value: A) {
        self.values.push(value)
    }
}

impl<A: Clone> RangeSet<A> {
    fn insert(&mut self, new_range_start: usize, new_range_end: usize, value: A) {
        // Scan all ranges and split as necessary

        let mut range_idx = 0;
        while let Some(range) = self.ranges.get_mut(range_idx) {
            if range.start > new_range_end {
                break;
            }

            if range.end < new_range_start {
                range_idx += 1;
                continue;
            }

            // Handle the overlapping range in [range.start, range.end]
            let overlap_start = max(range.start, new_range_start);
            let overlap_end = min(range.end, new_range_end);

            // Fast path for the special case when the entire range overlaps
            if overlap_start == range.start && overlap_end == range.end {
                range.add_value(value.clone());
                range_idx += 1;
                continue;
            }

            // Otherwise split from overlap_start, or overlap_end, or both
            if overlap_start > range.start && overlap_end < range.end {
                let old_values = range.values.clone();
                let mut new_values = old_values.clone();
                new_values.push(value.clone());

                let old_end = range.end;
                range.end = overlap_start - 1;

                self.ranges.insert(
                    range_idx + 1,
                    Range::with_values(overlap_start, overlap_end, new_values),
                );
                self.ranges.insert(
                    range_idx + 2,
                    Range::with_values(overlap_end + 1, old_end, old_values),
                );

                range_idx += 3;
                continue;
            }

            if overlap_start > range.start {
                assert_eq!(overlap_end, range.end);

                let mut new_values = range.values.clone();
                new_values.push(value.clone());

                range.end = overlap_start - 1;

                self.ranges.insert(
                    range_idx + 1,
                    Range::with_values(overlap_start, overlap_end, new_values),
                );

                range_idx += 2;
                continue;
            }

            if overlap_end < range.end {
                assert_eq!(overlap_start, range.start);

                let mut new_values = range.values.clone();
                new_values.push(value.clone());

                range.start = overlap_end + 1;

                self.ranges.insert(
                    range_idx,
                    Range::with_values(overlap_start, overlap_end, new_values),
                );

                range_idx += 2;
                continue;
            }

            todo!()
        }
    }
}

#[cfg(test)]
fn to_tuple<A: Clone>(range: &Range<A>) -> (usize, usize, Vec<A>) {
    (range.start, range.end, range.values.clone())
}

#[cfg(test)]
fn to_vec<A: Clone>(set: &RangeSet<A>) -> Vec<(usize, usize, Vec<A>)> {
    set.iter().map(to_tuple).collect()
}

#[test]
fn add_non_overlapping() {
    let mut ranges: RangeSet<u32> = RangeSet::new();

    ranges.insert(0, 10, 1);
    ranges.insert(20, 30, 0);

    assert_eq!(
        to_vec(&ranges),
        vec![
            (0, 10, vec![1]),
            (11, 19, vec![]),
            (20, 30, vec![0]),
            (31, usize::MAX, vec![])
        ]
    );
}

#[test]
fn add_non_overlapping_reverse() {
    let mut ranges: RangeSet<u32> = RangeSet::new();

    ranges.insert(20, 30, 0);
    ranges.insert(0, 10, 1);

    assert_eq!(
        to_vec(&ranges),
        vec![
            (0, 10, vec![1]),
            (11, 19, vec![]),
            (20, 30, vec![0]),
            (31, usize::MAX, vec![])
        ]
    );
}

#[test]
fn add_overlapping_1() {
    let mut ranges: RangeSet<u32> = RangeSet::new();

    ranges.insert(0, 10, 0);
    ranges.insert(10, 20, 1);

    assert_eq!(
        to_vec(&ranges),
        vec![
            (0, 9, vec![0]),
            (10, 10, vec![0, 1]),
            (11, 20, vec![1]),
            (21, usize::MAX, vec![])
        ]
    );
}

#[test]
fn add_overlapping_1_reverse() {
    let mut ranges: RangeSet<u32> = RangeSet::new();

    ranges.insert(10, 20, 1);
    ranges.insert(0, 10, 0);

    assert_eq!(
        to_vec(&ranges),
        vec![
            (0, 9, vec![0]),
            (10, 10, vec![1, 0]),
            (11, 20, vec![1]),
            (21, usize::MAX, vec![])
        ]
    );
}

#[test]
fn add_overlapping_2() {
    let mut ranges: RangeSet<u32> = RangeSet::new();

    ranges.insert(50, 100, 0);

    assert_eq!(
        to_vec(&ranges),
        vec![
            (0, 49, vec![]),
            (50, 100, vec![0]),
            (101, usize::MAX, vec![]),
        ]
    );

    ranges.insert(40, 60, 1);

    assert_eq!(
        to_vec(&ranges),
        vec![
            (0, 39, vec![]),
            (40, 49, vec![1]),
            (50, 60, vec![0, 1]),
            (61, 100, vec![0]),
            (101, usize::MAX, vec![]),
        ]
    );

    ranges.insert(90, 110, 2);

    assert_eq!(
        to_vec(&ranges),
        vec![
            (0, 39, vec![]),
            (40, 49, vec![1]),
            (50, 60, vec![0, 1]),
            (61, 89, vec![0]),
            (90, 100, vec![0, 2]),
            (101, 110, vec![2]),
            (111, usize::MAX, vec![]),
        ]
    );

    ranges.insert(70, 80, 3);

    assert_eq!(
        to_vec(&ranges),
        vec![
            (0, 39, vec![]),
            (40, 49, vec![1]),
            (50, 60, vec![0, 1]),
            (61, 69, vec![0]),
            (70, 80, vec![0, 3]),
            (81, 89, vec![0]),
            (90, 100, vec![0, 2]),
            (101, 110, vec![2]),
            (111, usize::MAX, vec![]),
        ]
    );
}
