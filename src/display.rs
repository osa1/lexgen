use std::collections::{BTreeSet, HashSet};
use std::fmt::{self, Display, Formatter};

pub(crate) struct BTreeSetDisplay<'a, A: Display>(pub(crate) &'a BTreeSet<A>);

pub(crate) struct HashSetDisplay<'a, A: Display, S>(pub(crate) &'a HashSet<A, S>);

impl<'a, A: Display> Display for BTreeSetDisplay<'a, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        display_set(f, self.0.len(), &mut self.0.iter())
    }
}

impl<'a, A: Display, S> Display for HashSetDisplay<'a, A, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        display_set(f, self.0.len(), &mut self.0.iter())
    }
}

fn display_set<A: Display>(
    f: &mut Formatter<'_>,
    n_elems: usize,
    elems: &mut dyn Iterator<Item = A>,
) -> fmt::Result {
    write!(f, "{{")?;

    for (elem_idx, elem) in elems.enumerate() {
        write!(f, "{}", elem)?;
        if elem_idx != n_elems - 1 {
            write!(f, ", ")?;
        }
    }

    write!(f, "}}")
}
