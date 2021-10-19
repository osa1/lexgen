use crate::ast::RuleRhs;

pub struct SemanticActionTable {
    table: Vec<RuleRhs>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SemanticActionIdx(usize);

impl SemanticActionTable {
    pub fn new() -> Self {
        Self { table: vec![] }
    }

    pub fn add(&mut self, action: RuleRhs) -> SemanticActionIdx {
        let idx = self.table.len();
        self.table.push(action);
        SemanticActionIdx(idx)
    }

    pub fn iter(&self) -> impl Iterator<Item = (SemanticActionIdx, &RuleRhs)> {
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
