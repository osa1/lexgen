use crate::ast::RuleRhs;

pub struct SemanticActionTable {
    table: Vec<SemanticAction>,
}

pub struct SemanticAction {
    pub action: RuleRhs,
    pub use_count: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SemanticActionIdx(usize);

impl SemanticActionTable {
    pub fn new() -> Self {
        Self { table: vec![] }
    }

    pub fn add(&mut self, action: RuleRhs) -> SemanticActionIdx {
        let idx = self.table.len();
        self.table.push(SemanticAction {
            action,
            use_count: 0,
        });
        SemanticActionIdx(idx)
    }

    pub fn record_use(&mut self, idx: SemanticActionIdx) {
        self.table[idx.0].use_count += 1;
    }

    pub fn get_num_uses(&self, idx: SemanticActionIdx) -> usize {
        self.table[idx.0].use_count
    }

    pub fn iter(&self) -> impl Iterator<Item = (SemanticActionIdx, &SemanticAction)> {
        self.table
            .iter()
            .enumerate()
            .map(|(idx, expr)| (SemanticActionIdx(idx), expr))
    }
}

impl SemanticActionIdx {
    pub fn symbol(&self) -> syn::Ident {
        syn::Ident::new(
            &format!("SEMANTIC_ACTION_{}", self.0),
            proc_macro2::Span::call_site(),
        )
    }
}
