pub struct SemanticActionTable {
    table: Vec<syn::Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SemanticActionIdx(usize);

impl SemanticActionTable {
    pub fn new() -> Self {
        Self { table: vec![] }
    }

    pub fn add(&mut self, expr: syn::Expr) -> SemanticActionIdx {
        let idx = self.table.len();
        self.table.push(expr);
        SemanticActionIdx(idx)
    }

    pub fn iter(&self) -> impl Iterator<Item = (SemanticActionIdx, &syn::Expr)> {
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
