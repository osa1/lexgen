use std::cmp::{max, min};

/// A map of inclusive ranges, with insertion and iteration operations. Insertion allows
/// overlapping ranges. When two ranges overlap, value of the overlapping parts is the union of
/// values of the overlapping ranges.
#[derive(Debug)]
pub struct RangeMap<A> {
    // NB. internally we don't have any overlaps. Overlapping ranges are split into smaller
    // non-overlapping ranges.
    ranges: Vec<Range<A>>,
}

impl<A> Default for RangeMap<A> {
    fn default() -> Self {
        RangeMap::new()
    }
}

impl<A> RangeMap<A> {
    fn new() -> RangeMap<A> {
        RangeMap {
            ranges: vec![Range {
                start: 0,
                end: u32::MAX,
                values: vec![],
            }],
        }
    }

    #[cfg(test)]
    fn iter_all(&self) -> impl Iterator<Item = &Range<A>> {
        self.ranges.iter()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Range<A>> {
        self.ranges.iter().filter(|r| !r.values.is_empty())
    }

    pub fn into_iter(self) -> impl Iterator<Item = Range<A>> {
        self.ranges.into_iter().filter(|r| !r.values.is_empty())
    }

    pub fn is_empty(&self) -> bool {
        self.iter().next().is_none()
    }

    pub fn filter_map<F, B>(self, f: F) -> RangeMap<B>
    where
        F: Fn(A) -> Option<B>,
    {
        RangeMap {
            ranges: self
                .ranges
                .into_iter()
                .map(|Range { start, end, values }| Range {
                    start,
                    end,
                    values: values.into_iter().filter_map(&f).collect(),
                })
                .collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Range<A> {
    pub start: u32,
    // Inclusive
    pub end: u32,
    pub values: Vec<A>,
}

impl<A> Range<A> {
    fn with_values(start: u32, end: u32, values: Vec<A>) -> Range<A> {
        Range { start, end, values }
    }

    fn add_values(&mut self, values: impl Iterator<Item = A>) {
        self.values.extend(values)
    }
}

impl<A: Clone> RangeMap<A> {
    pub fn insert_values(&mut self, new_range_start: u32, new_range_end: u32, values: &[A]) {
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
                range.add_values(values.iter().cloned());
                range_idx += 1;
                continue;
            }

            // Otherwise split from overlap_start, or overlap_end, or both
            if overlap_start > range.start && overlap_end < range.end {
                let old_values = range.values.clone();
                let mut new_values = old_values.clone();
                new_values.extend(values.iter().cloned());

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
                new_values.extend(values.iter().cloned());

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
                new_values.extend(values.iter().cloned());

                range.start = overlap_end + 1;

                self.ranges.insert(
                    range_idx,
                    Range::with_values(overlap_start, overlap_end, new_values),
                );

                range_idx += 2;
                continue;
            }

            unreachable!()
        }
    }

    pub fn insert(&mut self, new_range_start: u32, new_range_end: u32, value: A) {
        self.insert_values(new_range_start, new_range_end, std::slice::from_ref(&value))
    }
}

#[cfg(test)]
fn to_tuple<A: Clone>(range: &Range<A>) -> (u32, u32, Vec<A>) {
    (range.start, range.end, range.values.clone())
}

#[cfg(test)]
fn to_vec<A: Clone>(map: &RangeMap<A>) -> Vec<(u32, u32, Vec<A>)> {
    map.iter_all().map(to_tuple).collect()
}

#[cfg(test)]
fn to_vec_skip_spaces<A: Clone>(map: &RangeMap<A>) -> Vec<(u32, u32, Vec<A>)> {
    map.iter().map(to_tuple).collect()
}

#[test]
fn overlap_left() {
    let mut ranges: RangeMap<u32> = RangeMap::new();

    ranges.insert(10, 20, 0);
    ranges.insert(5, 15, 1);

    assert_eq!(
        to_vec_skip_spaces(&ranges),
        vec![(5, 9, vec![1]), (10, 15, vec![0, 1]), (16, 20, vec![0])]
    );

    ranges.insert(5, 5, 2);

    assert_eq!(
        to_vec_skip_spaces(&ranges),
        vec![
            (5, 5, vec![1, 2]),
            (6, 9, vec![1]),
            (10, 15, vec![0, 1]),
            (16, 20, vec![0]),
        ]
    );

    let mut ranges: RangeMap<u32> = RangeMap::new();

    ranges.insert(10, 20, 0);
    ranges.insert(10, 15, 1);

    assert_eq!(
        to_vec_skip_spaces(&ranges),
        vec![(10, 15, vec![0, 1]), (16, 20, vec![0])]
    );
}

#[test]
fn overlap_right() {
    let mut ranges: RangeMap<u32> = RangeMap::new();

    ranges.insert(5, 15, 1);
    ranges.insert(10, 20, 0);

    assert_eq!(
        to_vec_skip_spaces(&ranges),
        vec![(5, 9, vec![1]), (10, 15, vec![1, 0]), (16, 20, vec![0])]
    );

    ranges.insert(20, 20, 2);

    assert_eq!(
        to_vec_skip_spaces(&ranges),
        vec![
            (5, 9, vec![1]),
            (10, 15, vec![1, 0]),
            (16, 19, vec![0]),
            (20, 20, vec![0, 2]),
        ]
    );

    let mut ranges: RangeMap<u32> = RangeMap::new();

    ranges.insert(10, 15, 1);
    ranges.insert(10, 20, 0);

    assert_eq!(
        to_vec_skip_spaces(&ranges),
        vec![(10, 15, vec![1, 0]), (16, 20, vec![0])]
    );
}

#[test]
fn add_non_overlapping() {
    let mut ranges: RangeMap<u32> = RangeMap::new();

    ranges.insert(0, 10, 1);
    ranges.insert(20, 30, 0);

    assert_eq!(
        to_vec(&ranges),
        vec![
            (0, 10, vec![1]),
            (11, 19, vec![]),
            (20, 30, vec![0]),
            (31, u32::MAX, vec![])
        ]
    );
}

#[test]
fn add_non_overlapping_reverse() {
    let mut ranges: RangeMap<u32> = RangeMap::new();

    ranges.insert(20, 30, 0);
    ranges.insert(0, 10, 1);

    assert_eq!(
        to_vec(&ranges),
        vec![
            (0, 10, vec![1]),
            (11, 19, vec![]),
            (20, 30, vec![0]),
            (31, u32::MAX, vec![])
        ]
    );
}

#[test]
fn add_overlapping_1() {
    let mut ranges: RangeMap<u32> = RangeMap::new();

    ranges.insert(0, 10, 0);
    ranges.insert(10, 20, 1);

    assert_eq!(
        to_vec(&ranges),
        vec![
            (0, 9, vec![0]),
            (10, 10, vec![0, 1]),
            (11, 20, vec![1]),
            (21, u32::MAX, vec![])
        ]
    );
}

#[test]
fn add_overlapping_1_reverse() {
    let mut ranges: RangeMap<u32> = RangeMap::new();

    ranges.insert(10, 20, 1);
    ranges.insert(0, 10, 0);

    assert_eq!(
        to_vec(&ranges),
        vec![
            (0, 9, vec![0]),
            (10, 10, vec![1, 0]),
            (11, 20, vec![1]),
            (21, u32::MAX, vec![])
        ]
    );
}

#[test]
fn add_overlapping_2() {
    let mut ranges: RangeMap<u32> = RangeMap::new();

    ranges.insert(50, 100, 0);

    assert_eq!(
        to_vec(&ranges),
        vec![(0, 49, vec![]), (50, 100, vec![0]), (101, u32::MAX, vec![]),]
    );

    ranges.insert(40, 60, 1);

    assert_eq!(
        to_vec(&ranges),
        vec![
            (0, 39, vec![]),
            (40, 49, vec![1]),
            (50, 60, vec![0, 1]),
            (61, 100, vec![0]),
            (101, u32::MAX, vec![]),
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
            (111, u32::MAX, vec![]),
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
            (111, u32::MAX, vec![]),
        ]
    );
}

#[test]
fn large_range_multiple_overlaps() {
    let mut ranges: RangeMap<u32> = RangeMap::new();

    ranges.insert(10, 20, 0);
    ranges.insert(21, 30, 1);
    ranges.insert(5, 35, 2);

    assert_eq!(
        to_vec_skip_spaces(&ranges),
        vec![
            (5, 9, vec![2]),
            (10, 20, vec![0, 2]),
            (21, 30, vec![1, 2]),
            (31, 35, vec![2]),
        ]
    );
}

#[test]
fn overlap_middle() {
    let mut ranges: RangeMap<u32> = RangeMap::new();

    ranges.insert(10, 20, 0);
    ranges.insert(15, 15, 1);

    assert_eq!(
        to_vec_skip_spaces(&ranges),
        vec![(10, 14, vec![0]), (15, 15, vec![0, 1]), (16, 20, vec![0])]
    );
}

#[test]
fn overlap_exact() {
    let mut ranges: RangeMap<u32> = RangeMap::new();

    ranges.insert(10, 20, 0);
    ranges.insert(10, 20, 1);

    assert_eq!(to_vec_skip_spaces(&ranges), vec![(10, 20, vec![0, 1])]);
}
