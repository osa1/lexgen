use crate::collections::Map;

use std::collections::hash_map::Entry;

pub struct SearchTableSet {
    tables: Map<Vec<(char, char)>, syn::Ident>,
}

impl SearchTableSet {
    pub fn new() -> SearchTableSet {
        SearchTableSet {
            tables: Default::default(),
        }
    }

    pub fn add_table(&mut self, ranges: Vec<(char, char)>) -> syn::Ident {
        let n_tables = self.tables.len();
        match self.tables.entry(ranges) {
            Entry::Occupied(entry) => entry.get().clone(),
            Entry::Vacant(entry) => {
                let ident = syn::Ident::new(
                    &format!("RANGE_TABLE_{}", n_tables),
                    proc_macro2::Span::call_site(),
                );
                entry.insert(ident.clone());
                ident
            }
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&[(char, char)], &syn::Ident)> {
        self.tables
            .iter()
            .map(|(ranges, ident)| (ranges.as_slice(), ident))
    }

    pub fn is_empty(&self) -> bool {
        self.tables.is_empty()
    }
}
