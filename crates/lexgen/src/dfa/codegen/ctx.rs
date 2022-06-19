use super::search_table::SearchTableSet;
use super::StateIdx;
use super::DFA;
use crate::ast::RuleRhs;
use crate::collections::Map;
use crate::dfa::simplify::Trans;
use crate::semantic_action_table::{SemanticActionIdx, SemanticActionTable};

/// Code generation state
pub struct CgCtx {
    /// Maps semantic action indices to expressions. Used to generate semantic action functions.
    semantic_action_table: SemanticActionTable,

    /// Name of the lexer: `MyLexer` in `lexer! { MyLexer -> MyToken; }`
    lexer_name: syn::Ident,

    /// Type of the values the lexer will produce: `MyToken` in `lexer! { MyLexer -> MyToken; }`
    token_type: syn::Type,

    /// Type of the user error, when available. `<type>` in `type Error = ...`.
    user_error_type: Option<syn::Type>,

    /// Maps user-written rule names (e.g. `rule MyRule { ... }`) to their initial states in the
    /// final DFA.
    rule_states: Map<String, StateIdx>,

    /// Sorted vector of states with only one predecessor. These states will be inlined in the
    /// predecessor states and won't appear in the final code. Inlining these states significantly
    /// improves code size and runtime performance.
    ///
    /// This vector is used to map non-inlined states to their final state indices in the generated
    /// code. For example, if this vector is `[5]`, state 5 is skipped, and states after 5 are
    /// decremented 1, so state 6 becomes 5 etc.
    inlined_states: Vec<StateIdx>,

    /// Mutable parts of the codegen state
    codegen_state: CgState,
}

struct CgState {
    /// Binary search tables generated so far
    search_tables: SearchTableSet,
}

impl CgCtx {
    pub fn new(
        dfa: &DFA<Trans<SemanticActionIdx>, SemanticActionIdx>,
        semantic_action_table: SemanticActionTable,
        lexer_name: syn::Ident,
        token_type: syn::Type,
        user_error_type: Option<syn::Type>,
        rule_states: Map<String, StateIdx>,
    ) -> CgCtx {
        let inlined_states: Vec<StateIdx> = dfa
            .states
            .iter()
            .enumerate()
            .filter_map(|(state_idx, state)| {
                if state.predecessors.len() == 1 {
                    Some(StateIdx(state_idx))
                } else {
                    None
                }
            })
            .collect();

        CgCtx {
            semantic_action_table,
            lexer_name,
            token_type,
            user_error_type,
            rule_states,
            inlined_states,
            codegen_state: CgState {
                search_tables: SearchTableSet::new(),
            },
        }
    }

    pub fn lexer_name(&self) -> &syn::Ident {
        &self.lexer_name
    }

    /// Renumber a state index taking inlined states into account.
    pub fn renumber_state(&self, state: StateIdx) -> StateIdx {
        match self.inlined_states.binary_search(&state) {
            Ok(idx) | Err(idx) => state.map(|state_idx| state_idx - idx),
        }
    }

    pub fn n_inlined_states(&self) -> usize {
        self.inlined_states.len()
    }

    pub fn token_type(&self) -> &syn::Type {
        &self.token_type
    }

    pub fn user_error_type(&self) -> Option<&syn::Type> {
        self.user_error_type.as_ref()
    }

    pub fn add_search_table(&mut self, ranges: Vec<(char, char)>) -> syn::Ident {
        self.codegen_state.search_tables.add_table(ranges)
    }

    pub fn take_search_tables(&mut self) -> SearchTableSet {
        std::mem::replace(&mut self.codegen_state.search_tables, SearchTableSet::new())
    }

    pub fn rule_states(&self) -> &Map<String, StateIdx> {
        &self.rule_states
    }

    pub fn iter_semantic_actions(&self) -> impl Iterator<Item = (SemanticActionIdx, &RuleRhs)> {
        self.semantic_action_table.iter()
    }

    pub fn semantic_action_fn_ident(&self, action: SemanticActionIdx) -> syn::Ident {
        syn::Ident::new(
            &format!("{}_ACTION_{}", self.lexer_name, action.as_usize()),
            self.lexer_name.span(),
        )
    }
}
