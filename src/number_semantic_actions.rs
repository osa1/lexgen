use crate::ast::RuleRhs;
use crate::dfa::DFA;
pub use crate::semantic_action_table::{SemanticActionIdx, SemanticActionTable};

pub fn number_semantic_actions<S>(
    dfa: DFA<S, RuleRhs>,
) -> (DFA<S, SemanticActionIdx>, SemanticActionTable) {
    let mut tbl = SemanticActionTable::new();

    let state_indices: DFA<S, SemanticActionIdx> = dfa
        .into_state_indices()
        .map(|(state_idx, state)| {
            let state = state.map_semantic_action(|rhs| tbl.add(rhs));
            (state_idx, state)
        })
        .collect();

    (state_indices, tbl)
}
