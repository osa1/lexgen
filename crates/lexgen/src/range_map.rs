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
        RangeMap { ranges: vec![] }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Range<A>> {
        self.ranges.iter()
    }

    pub fn into_iter(self) -> impl Iterator<Item = Range<A>> {
        self.ranges.into_iter()
    }

    pub fn is_empty(&self) -> bool {
        self.ranges.is_empty()
    }

    pub fn filter_map<F, B>(self, mut f: F) -> RangeMap<B>
    where
        F: FnMut(A) -> Option<B>,
    {
        RangeMap {
            ranges: self
                .ranges
                .into_iter()
                .map(|Range { start, end, values }| Range {
                    start,
                    end,
                    values: values.into_iter().filter_map(&mut f).collect(),
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
    pub fn contains(&self, char: char) -> bool {
        char as u32 >= self.start && char as u32 <= self.end
    }
}

impl<A: Clone> RangeMap<A> {
    /// O(n) where n is the number of existing ranges in the map
    pub fn insert_values(&mut self, mut new_range_start: u32, new_range_end: u32, values: &[A]) {
        let old_ranges = std::mem::replace(&mut self.ranges, vec![]);
        let mut new_ranges = Vec::with_capacity(old_ranges.len() + 2);

        let mut range_iter = old_ranges.into_iter();

        while let Some(range) = range_iter.next() {
            if range.end < new_range_start {
                new_ranges.push(range);
            } else if range.start > new_range_end {
                new_ranges.push(Range {
                    start: new_range_start,
                    end: new_range_end,
                    values: values.to_owned(),
                });
                new_ranges.push(range);
                new_ranges.extend(range_iter);
                self.ranges = new_ranges;
                return;
            } else {
                let overlap = max(new_range_start, range.start)..=min(new_range_end, range.end);

                // (1) push new_range before the overlap
                // (2) push old_range before the overlap
                // (3) push overlapping part
                // (4) push old_range after the overlap
                // (5) push new_range after the overlap
                //
                //
                // 1 and 2, 4 and 5 can't happen at once. 5 needs to be handled in the next
                // iteration as there may be other overlapping ranges with new_range after the
                // current overlap. In all other cases, we copy rest of the ranges and return.

                // (1)
                if new_range_start < *overlap.start() {
                    new_ranges.push(Range {
                        start: new_range_start,
                        end: *overlap.start() - 1,
                        values: values.to_owned(),
                    });
                }
                // (2)
                else if range.start < *overlap.start() {
                    new_ranges.push(Range {
                        start: range.start,
                        end: overlap.start() - 1,
                        values: range.values.clone(),
                    });
                }

                // (3)
                let mut overlap_values = range.values.clone();
                overlap_values.extend(values.iter().cloned());
                new_ranges.push(Range {
                    start: *overlap.start(),
                    end: *overlap.end(),
                    values: overlap_values,
                });

                // (4)
                if range.end > *overlap.end() {
                    new_ranges.push(Range {
                        start: *overlap.end() + 1,
                        end: range.end,
                        values: range.values,
                    });
                }
                // (5)
                else if new_range_end > *overlap.end() {
                    new_range_start = *overlap.end() + 1;
                    continue;
                }

                new_ranges.extend(range_iter);
                self.ranges = new_ranges;
                return;
            }
        }

        let push_new_range = match new_ranges.last() {
            None => true,
            Some(last_range) => last_range.end < new_range_start,
        };

        if push_new_range {
            new_ranges.push(Range {
                start: new_range_start,
                end: new_range_end,
                values: values.to_owned(),
            });
        }

        self.ranges = new_ranges;
    }

    /// O(n) where n is the number of existing ranges in the map
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
    map.iter().map(to_tuple).collect()
}

#[test]
fn overlap_left() {
    let mut ranges: RangeMap<u32> = RangeMap::new();

    ranges.insert(10, 20, 0);
    ranges.insert(5, 15, 1);

    assert_eq!(
        to_vec(&ranges),
        vec![(5, 9, vec![1]), (10, 15, vec![0, 1]), (16, 20, vec![0])]
    );

    ranges.insert(5, 5, 2);

    assert_eq!(
        to_vec(&ranges),
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
        to_vec(&ranges),
        vec![(10, 15, vec![0, 1]), (16, 20, vec![0])]
    );
}

#[test]
fn overlap_right() {
    let mut ranges: RangeMap<u32> = RangeMap::new();

    ranges.insert(5, 15, 1);

    assert_eq!(to_vec(&ranges), vec![(5, 15, vec![1])]);

    ranges.insert(10, 20, 0);

    assert_eq!(
        to_vec(&ranges),
        vec![(5, 9, vec![1]), (10, 15, vec![1, 0]), (16, 20, vec![0])]
    );

    ranges.insert(20, 20, 2);

    assert_eq!(
        to_vec(&ranges),
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
        to_vec(&ranges),
        vec![(10, 15, vec![1, 0]), (16, 20, vec![0])]
    );
}

#[test]
fn add_non_overlapping() {
    let mut ranges: RangeMap<u32> = RangeMap::new();

    ranges.insert(0, 10, 1);
    ranges.insert(20, 30, 0);

    assert_eq!(to_vec(&ranges), vec![(0, 10, vec![1]), (20, 30, vec![0]),]);
}

#[test]
fn add_non_overlapping_reverse() {
    let mut ranges: RangeMap<u32> = RangeMap::new();

    ranges.insert(20, 30, 0);
    ranges.insert(0, 10, 1);

    assert_eq!(to_vec(&ranges), vec![(0, 10, vec![1]), (20, 30, vec![0]),]);
}

#[test]
fn add_overlapping_1() {
    let mut ranges: RangeMap<u32> = RangeMap::new();

    ranges.insert(0, 10, 0);
    ranges.insert(10, 20, 1);

    assert_eq!(
        to_vec(&ranges),
        vec![(0, 9, vec![0]), (10, 10, vec![0, 1]), (11, 20, vec![1]),]
    );
}

#[test]
fn add_overlapping_1_reverse() {
    let mut ranges: RangeMap<u32> = RangeMap::new();

    ranges.insert(10, 20, 1);
    ranges.insert(0, 10, 0);

    assert_eq!(
        to_vec(&ranges),
        vec![(0, 9, vec![0]), (10, 10, vec![1, 0]), (11, 20, vec![1]),]
    );
}

#[test]
fn add_overlapping_2() {
    let mut ranges: RangeMap<u32> = RangeMap::new();

    ranges.insert(50, 100, 0);

    assert_eq!(to_vec(&ranges), vec![(50, 100, vec![0])]);

    ranges.insert(40, 60, 1);

    assert_eq!(
        to_vec(&ranges),
        vec![(40, 49, vec![1]), (50, 60, vec![0, 1]), (61, 100, vec![0]),]
    );

    ranges.insert(90, 110, 2);

    assert_eq!(
        to_vec(&ranges),
        vec![
            (40, 49, vec![1]),
            (50, 60, vec![0, 1]),
            (61, 89, vec![0]),
            (90, 100, vec![0, 2]),
            (101, 110, vec![2]),
        ]
    );

    ranges.insert(70, 80, 3);

    assert_eq!(
        to_vec(&ranges),
        vec![
            (40, 49, vec![1]),
            (50, 60, vec![0, 1]),
            (61, 69, vec![0]),
            (70, 80, vec![0, 3]),
            (81, 89, vec![0]),
            (90, 100, vec![0, 2]),
            (101, 110, vec![2]),
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
        to_vec(&ranges),
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
        to_vec(&ranges),
        vec![(10, 14, vec![0]), (15, 15, vec![0, 1]), (16, 20, vec![0])]
    );
}

#[test]
fn overlap_exact() {
    let mut ranges: RangeMap<u32> = RangeMap::new();

    ranges.insert(10, 20, 0);
    ranges.insert(10, 20, 1);

    assert_eq!(to_vec(&ranges), vec![(10, 20, vec![0, 1])]);
}
