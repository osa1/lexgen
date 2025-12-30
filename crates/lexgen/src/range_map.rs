use std::cmp::{max, min, Ordering};
use std::mem::take;

use smallvec::{smallvec, SmallVec};

/// A map of inclusive ranges, with insertion and iteration operations. Insertion allows
/// overlapping ranges. When two ranges overlap, value of the overlapping parts is the union of
/// values of the overlapping ranges.
#[derive(Debug)]
pub struct RangeMap<A> {
    // NB. internally we don't have any overlaps. Overlapping ranges are split into smaller
    // non-overlapping ranges.
    ranges: Ranges<A>,
}

pub type Ranges<A> = SmallVec<[Range<A>; 1]>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Range<A> {
    pub start: char,
    // Inclusive
    pub end: char,
    pub value: A,
}

impl<A> Default for RangeMap<A> {
    fn default() -> Self {
        RangeMap::new()
    }
}

impl<A> RangeMap<A> {
    pub fn new() -> RangeMap<A> {
        RangeMap {
            ranges: smallvec![],
        }
    }

    /// Create a range map from given non-overlapping and sorted ranges. These properties
    /// (non-overlapping, sorted) are checked in debug mode but not in release mode.
    ///
    /// O(1)
    pub fn from_non_overlapping_sorted_ranges(ranges: Ranges<A>) -> RangeMap<A> {
        #[cfg(debug_assertions)]
        for ranges in ranges.windows(2) {
            let range1 = &ranges[0];
            let range2 = &ranges[1];
            assert!(range1.end < range2.start);
        }

        RangeMap { ranges }
    }

    pub fn len(&self) -> usize {
        self.ranges.len()
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

    pub fn map<F, B>(self, mut f: F) -> RangeMap<B>
    where
        F: FnMut(A) -> B,
    {
        RangeMap {
            ranges: self
                .ranges
                .into_iter()
                .map(|Range { start, end, value }| Range {
                    start,
                    end,
                    value: f(value),
                })
                .collect(),
        }
    }
}

impl<A: Clone> RangeMap<A> {
    /// O(N+M) where N is the number of current ranges and M is the number of inserted ranges
    pub fn insert_ranges<F, I>(&mut self, mut ranges2_iter: I, merge: F)
    where
        F: Fn(&mut A, A),
        I: Iterator<Item = Range<A>>,
    {
        let mut new_ranges: Vec<Range<A>> = vec![];
        let old_ranges = take(&mut self.ranges);

        let mut ranges1_iter = old_ranges.into_iter();

        let mut range1 = ranges1_iter.next();
        let mut range2 = ranges2_iter.next();

        loop {
            match (&mut range1, &mut range2) {
                (Some(ref mut range1_), Some(ref mut range2_)) => {
                    // - No overlap: push the range that comes first, increment its iterator
                    //
                    // - Overlap: push at most 3 ranges:
                    //
                    //   1. Part of range 1 or 2 that comes before the overlap. Update the range.
                    //      Do not increment iterators.
                    //   2. Part of range 1 and 2 that overlap. Push merged range, update both
                    //      ranges. Increment iterators for the ranges that are now empty.
                    //   3. Part of range 1 or 2 that comes after the overlap. Increment the
                    //      iterator for the range.

                    if range1_.end < range2_.start {
                        // No overlap, range 1 comes first
                        new_ranges.push(range1_.clone());
                        range1 = ranges1_iter.next();
                    } else if range2_.end < range1_.start {
                        // No overlap, range 2 comes first
                        new_ranges.push(range2_.clone());
                        range2 = ranges2_iter.next();
                    } else {
                        let overlap =
                            max(range1_.start, range2_.start)..=min(range1_.end, range2_.end);

                        match range1_.start.cmp(&range2_.start) {
                            Ordering::Less => {
                                // Range 1 comes first
                                new_ranges.push(Range {
                                    start: range1_.start,
                                    end: char::from_u32(*overlap.start() as u32 - 1).unwrap(),
                                    value: range1_.value.clone(),
                                });
                                range1_.start = *overlap.start();
                            }
                            Ordering::Greater => {
                                // Range 2 comes first
                                new_ranges.push(Range {
                                    start: range2_.start,
                                    end: char::from_u32(*overlap.start() as u32 - 1).unwrap(),
                                    value: range2_.value.clone(),
                                });
                                range2_.start = *overlap.start();
                            }
                            Ordering::Equal => {
                                // Ranges start at the same point
                                let mut merged_value = range1_.value.clone();
                                merge(&mut merged_value, range2_.value.clone());
                                let merged_range = Range {
                                    start: *overlap.start(),
                                    end: *overlap.end(),
                                    value: merged_value,
                                };
                                new_ranges.push(merged_range);

                                match range1_.end.cmp(&range2_.end) {
                                    Ordering::Less => {
                                        range1 = ranges1_iter.next();
                                        range2_.start =
                                            char::from_u32(*overlap.end() as u32 + 1).unwrap();
                                    }
                                    Ordering::Greater => {
                                        range2 = ranges2_iter.next();
                                        range1_.start =
                                            char::from_u32(*overlap.end() as u32 + 1).unwrap();
                                    }
                                    Ordering::Equal => {
                                        range1 = ranges1_iter.next();
                                        range2 = ranges2_iter.next();
                                    }
                                }
                            }
                        }
                    }
                }
                (Some(range1_), None) => {
                    new_ranges.push(range1_.clone());
                    new_ranges.extend(ranges1_iter);
                    break;
                }
                (None, Some(range2_)) => {
                    new_ranges.push(range2_.clone());
                    new_ranges.extend(ranges2_iter);
                    break;
                }
                (None, None) => break,
            }
        }

        self.ranges = new_ranges.into();
    }

    /// O(n) where n is the number of existing ranges in the map
    pub fn insert<F>(&mut self, mut new_range_start: char, new_range_end: char, value: A, merge: F)
    where
        F: Fn(&mut A, A),
    {
        let old_ranges = take(&mut self.ranges);
        let mut new_ranges = Vec::with_capacity(old_ranges.len() + 2);

        let mut range_iter = old_ranges.into_iter();

        while let Some(range) = range_iter.next() {
            if range.end < new_range_start {
                new_ranges.push(range);
            } else if range.start > new_range_end {
                new_ranges.push(Range {
                    start: new_range_start,
                    end: new_range_end,
                    value,
                });
                new_ranges.push(range);
                new_ranges.extend(range_iter);
                self.ranges = new_ranges.into();
                return;
            } else {
                let overlap = max(new_range_start, range.start)..=min(new_range_end, range.end);

                // (1) push new_range before the overlap
                // (2) push old_range before the overlap
                // (3) push overlapping part
                // (4) push old_range after the overlap
                // (5) push new_range after the overlap
                //
                // 1 and 2, 4 and 5 can't happen at once. 5 needs to be handled in the next
                // iteration as there may be other overlapping ranges with new_range after the
                // current overlap. In all other cases, we copy rest of the ranges and return.

                // (1)
                if new_range_start < *overlap.start() {
                    new_ranges.push(Range {
                        start: new_range_start,
                        end: char::from_u32(*overlap.start() as u32 - 1).unwrap(),
                        value: value.clone(),
                    });
                }
                // (2)
                else if range.start < *overlap.start() {
                    new_ranges.push(Range {
                        start: range.start,
                        end: char::from_u32(*overlap.start() as u32 - 1).unwrap(),
                        value: range.value.clone(),
                    });
                }

                // (3)
                let mut overlap_values = range.value.clone();
                merge(&mut overlap_values, value.clone());
                new_ranges.push(Range {
                    start: *overlap.start(),
                    end: *overlap.end(),
                    value: overlap_values,
                });

                // (4)
                if range.end > *overlap.end() {
                    new_ranges.push(Range {
                        start: char::from_u32(*overlap.end() as u32 + 1).unwrap(),
                        end: range.end,
                        value: range.value,
                    });
                }
                // (5)
                else if new_range_end > *overlap.end() {
                    new_range_start = char::from_u32(*overlap.end() as u32 + 1).unwrap();
                    continue;
                }

                new_ranges.extend(range_iter);
                self.ranges = new_ranges.into();
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
                value,
            });
        }

        self.ranges = new_ranges.into();
    }

    /// O(N+M) where N is the number of current ranges and M is the number of removed ranges
    pub fn remove_ranges<B>(&mut self, other: &RangeMap<B>) {
        let old_ranges = take(&mut self.ranges);
        let mut new_ranges: Vec<Range<A>> = Vec::with_capacity(old_ranges.len());

        let mut removed_ranges_iter = other.ranges.iter();
        let mut removed_range = removed_ranges_iter.next();

        let mut old_ranges_iter = old_ranges.into_iter();
        let mut old_range = old_ranges_iter.next();

        loop {
            match (&mut old_range, removed_range) {
                (Some(ref mut old_range_), Some(removed_range_)) => {
                    if old_range_.end < removed_range_.start {
                        new_ranges.push(old_range_.clone());
                        old_range = old_ranges_iter.next();
                    } else if removed_range_.end < old_range_.start {
                        removed_range = removed_ranges_iter.next();
                    } else {
                        let overlap = max(old_range_.start, removed_range_.start)
                            ..=min(old_range_.end, removed_range_.end);

                        // Three cases to consider:
                        //
                        // (1) overlap starts from the left end of old range:
                        //     update A, increment B
                        //
                        // (2) overlap ends at the right end of old range:
                        //     push left of overlap, increment A
                        //
                        // (3) overlap is in the middle of old range:
                        //     push left of overlap, update A

                        // (1)
                        if *overlap.start() == old_range_.start {
                            old_range_.start = char::from_u32(*overlap.end() as u32 + 1).unwrap();
                            removed_range = removed_ranges_iter.next();
                        }
                        // (2)
                        else if *overlap.end() == old_range_.end {
                            let new_range = Range {
                                start: old_range_.start,
                                end: char::from_u32(*overlap.start() as u32 - 1).unwrap(),
                                value: old_range_.value.clone(),
                            };
                            new_ranges.push(new_range);
                            old_range = old_ranges_iter.next();
                        }
                        // (3)
                        else {
                            let new_range = Range {
                                start: old_range_.start,
                                end: char::from_u32(*overlap.start() as u32 - 1).unwrap(),
                                value: old_range_.value.clone(),
                            };
                            new_ranges.push(new_range);
                            old_range_.start = char::from_u32(*overlap.end() as u32 + 1).unwrap();
                        }
                    }
                }
                (Some(old_range_), None) => {
                    new_ranges.push(old_range_.clone());
                    new_ranges.extend(old_ranges_iter);
                    break;
                }
                (None, Some(_removed_range_)) => break,
                (None, None) => break,
            }
        }

        self.ranges = new_ranges.into();
    }
}

#[cfg(test)]
fn to_tuple<A: Clone>(range: &Range<Vec<A>>) -> (u32, u32, Vec<A>) {
    (range.start as u32, range.end as u32, range.value.clone())
}

#[cfg(test)]
fn to_vec<A: Clone>(map: &RangeMap<Vec<A>>) -> Vec<(u32, u32, Vec<A>)> {
    map.iter().map(to_tuple).collect()
}

#[cfg(test)]
fn insert<A: Clone>(map: &mut RangeMap<Vec<A>>, range_start: u32, range_end: u32, value: A) {
    let mut map2: RangeMap<Vec<A>> = RangeMap::new();
    map2.insert(
        char::from_u32(range_start).unwrap(),
        char::from_u32(range_end).unwrap(),
        vec![value],
        |_, _| panic!(),
    );

    map.insert_ranges(map2.into_iter(), |values_1, values_2| {
        values_1.extend(values_2)
    });
}

#[cfg(test)]
fn remove<A: Clone>(map: &mut RangeMap<Vec<A>>, removed_ranges: &[(u32, u32)]) {
    let mut removed_range_map: RangeMap<()> = RangeMap::new();
    for (removed_range_start, removed_range_end) in removed_ranges {
        removed_range_map.insert(
            char::from_u32(*removed_range_start).unwrap(),
            char::from_u32(*removed_range_end).unwrap(),
            (),
            |_, _| panic!(),
        );
    }

    map.remove_ranges(&removed_range_map);
}

#[test]
fn overlap_left() {
    let mut ranges: RangeMap<Vec<u32>> = RangeMap::new();

    insert(&mut ranges, 10, 20, 0);
    insert(&mut ranges, 5, 15, 1);

    assert_eq!(
        to_vec(&ranges),
        vec![(5, 9, vec![1]), (10, 15, vec![0, 1]), (16, 20, vec![0])]
    );

    insert(&mut ranges, 5, 5, 2);

    assert_eq!(
        to_vec(&ranges),
        vec![
            (5, 5, vec![1, 2]),
            (6, 9, vec![1]),
            (10, 15, vec![0, 1]),
            (16, 20, vec![0]),
        ]
    );

    let mut ranges: RangeMap<Vec<u32>> = RangeMap::new();

    insert(&mut ranges, 10, 20, 0);
    insert(&mut ranges, 10, 15, 1);

    assert_eq!(
        to_vec(&ranges),
        vec![(10, 15, vec![0, 1]), (16, 20, vec![0])]
    );
}

#[test]
fn overlap_right() {
    let mut ranges: RangeMap<Vec<u32>> = RangeMap::new();

    insert(&mut ranges, 5, 15, 1);

    assert_eq!(to_vec(&ranges), vec![(5, 15, vec![1])]);

    insert(&mut ranges, 10, 20, 0);

    assert_eq!(
        to_vec(&ranges),
        vec![(5, 9, vec![1]), (10, 15, vec![1, 0]), (16, 20, vec![0])]
    );

    insert(&mut ranges, 20, 20, 2);

    assert_eq!(
        to_vec(&ranges),
        vec![
            (5, 9, vec![1]),
            (10, 15, vec![1, 0]),
            (16, 19, vec![0]),
            (20, 20, vec![0, 2]),
        ]
    );

    let mut ranges: RangeMap<Vec<u32>> = RangeMap::new();

    insert(&mut ranges, 10, 15, 1);
    insert(&mut ranges, 10, 20, 0);

    assert_eq!(
        to_vec(&ranges),
        vec![(10, 15, vec![1, 0]), (16, 20, vec![0])]
    );
}

#[test]
fn add_non_overlapping() {
    let mut ranges: RangeMap<Vec<u32>> = RangeMap::new();

    insert(&mut ranges, 0, 10, 1);
    insert(&mut ranges, 20, 30, 0);

    assert_eq!(to_vec(&ranges), vec![(0, 10, vec![1]), (20, 30, vec![0]),]);
}

#[test]
fn add_non_overlapping_reverse() {
    let mut ranges: RangeMap<Vec<u32>> = RangeMap::new();

    insert(&mut ranges, 20, 30, 0);
    insert(&mut ranges, 0, 10, 1);

    assert_eq!(to_vec(&ranges), vec![(0, 10, vec![1]), (20, 30, vec![0]),]);
}

#[test]
fn add_overlapping_1() {
    let mut ranges: RangeMap<Vec<u32>> = RangeMap::new();

    insert(&mut ranges, 0, 10, 0);
    insert(&mut ranges, 10, 20, 1);

    assert_eq!(
        to_vec(&ranges),
        vec![(0, 9, vec![0]), (10, 10, vec![0, 1]), (11, 20, vec![1]),]
    );
}

#[test]
fn add_overlapping_1_reverse() {
    let mut ranges: RangeMap<Vec<u32>> = RangeMap::new();

    insert(&mut ranges, 10, 20, 1);
    insert(&mut ranges, 0, 10, 0);

    assert_eq!(
        to_vec(&ranges),
        vec![(0, 9, vec![0]), (10, 10, vec![1, 0]), (11, 20, vec![1]),]
    );
}

#[test]
fn add_overlapping_2() {
    let mut ranges: RangeMap<Vec<u32>> = RangeMap::new();

    insert(&mut ranges, 50, 100, 0);

    assert_eq!(to_vec(&ranges), vec![(50, 100, vec![0])]);

    insert(&mut ranges, 40, 60, 1);

    assert_eq!(
        to_vec(&ranges),
        vec![(40, 49, vec![1]), (50, 60, vec![0, 1]), (61, 100, vec![0]),]
    );

    insert(&mut ranges, 90, 110, 2);

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

    insert(&mut ranges, 70, 80, 3);

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
    let mut ranges: RangeMap<Vec<u32>> = RangeMap::new();

    insert(&mut ranges, 10, 20, 0);
    insert(&mut ranges, 21, 30, 1);
    insert(&mut ranges, 5, 35, 2);

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
    let mut ranges: RangeMap<Vec<u32>> = RangeMap::new();

    insert(&mut ranges, 10, 20, 0);
    insert(&mut ranges, 15, 15, 1);

    assert_eq!(
        to_vec(&ranges),
        vec![(10, 14, vec![0]), (15, 15, vec![0, 1]), (16, 20, vec![0])]
    );
}

#[test]
fn overlap_exact() {
    let mut ranges: RangeMap<Vec<u32>> = RangeMap::new();

    insert(&mut ranges, 10, 20, 0);
    insert(&mut ranges, 10, 20, 1);

    assert_eq!(to_vec(&ranges), vec![(10, 20, vec![0, 1])]);
}

#[test]
fn remove_no_overlap() {
    let mut ranges: RangeMap<Vec<u32>> = RangeMap::new();

    insert(&mut ranges, 10, 20, 0);
    insert(&mut ranges, 30, 40, 1);

    remove(&mut ranges, &[(0, 9), (21, 29), (41, 50)]);

    assert_eq!(to_vec(&ranges), vec![(10, 20, vec![0]), (30, 40, vec![1])]);
}

#[test]
fn remove_overlap_left() {
    let mut ranges: RangeMap<Vec<u32>> = RangeMap::new();

    insert(&mut ranges, 10, 20, 0);
    insert(&mut ranges, 30, 40, 1);

    remove(&mut ranges, &[(10, 15), (30, 35)]);

    assert_eq!(to_vec(&ranges), vec![(16, 20, vec![0]), (36, 40, vec![1])]);
}

#[test]
fn remove_overlap_right() {
    let mut ranges: RangeMap<Vec<u32>> = RangeMap::new();

    insert(&mut ranges, 10, 20, 0);
    insert(&mut ranges, 30, 40, 1);

    remove(&mut ranges, &[(15, 20), (35, 40)]);

    assert_eq!(to_vec(&ranges), vec![(10, 14, vec![0]), (30, 34, vec![1])]);
}

#[test]
fn remove_overlap_middle() {
    let mut ranges: RangeMap<Vec<u32>> = RangeMap::new();

    insert(&mut ranges, 10, 20, 0);
    insert(&mut ranges, 30, 40, 1);

    remove(&mut ranges, &[(12, 15), (32, 35)]);

    assert_eq!(
        to_vec(&ranges),
        vec![
            (10, 11, vec![0]),
            (16, 20, vec![0]),
            (30, 31, vec![1]),
            (36, 40, vec![1])
        ]
    );
}
