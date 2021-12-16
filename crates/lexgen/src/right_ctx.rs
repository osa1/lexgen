//! Stuff related to right contexts
//!
//! A right context is a limited version of lookahead. A rule can have at most one right context.
//! When a rule has right context, after the regex for the rule matches, we run the DFA for the
//! right context with cloned input stream. Only if it matches we consider the the rule as a match.
//! This provides a simple "lookahead" support, which should be good enough when lexing programming
//! languages.

use crate::ast::{Regex, Var};
use crate::collections::Map;
use crate::dfa::{StateIdx, DFA};
use crate::nfa::NFA;
use crate::nfa_to_dfa::nfa_to_dfa;

#[derive(Debug, Default)]
pub struct RightCtxDFAs {
    dfas: Vec<DFA<StateIdx, ()>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct RightCtxIdx(usize);

impl RightCtxIdx {
    pub fn as_usize(&self) -> usize {
        self.0
    }
}

impl RightCtxDFAs {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn new_right_ctx(&mut self, bindings: &Map<Var, Regex>, right_ctx: &Regex) -> RightCtxIdx {
        let idx = self.dfas.len();

        let mut nfa: NFA<()> = NFA::new();
        nfa.add_regex(bindings, right_ctx, None, ());

        let dfa = nfa_to_dfa(&nfa);
        self.dfas.push(dfa);

        RightCtxIdx(idx)
    }

    pub fn get(&self, right_ctx: &RightCtxIdx) -> &DFA<StateIdx, ()> {
        &self.dfas[right_ctx.as_usize()]
    }
}
