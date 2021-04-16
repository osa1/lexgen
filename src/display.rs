use std::collections::BTreeSet;
use std::fmt::{self, Display, Formatter};

pub(crate) struct BTreeSetDisplay<'a, A: Display>(pub(crate) &'a BTreeSet<A>);

impl<'a, A: Display> Display for BTreeSetDisplay<'a, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;

        let n_elems = self.0.len();
        for (elem_idx, elem) in self.0.iter().enumerate() {
            write!(f, "{}", elem)?;
            if elem_idx != n_elems - 1 {
                write!(f, ", ")?;
            }
        }

        write!(f, "}}")
    }
}
