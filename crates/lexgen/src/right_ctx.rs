//! Stuff related to right contexts
//!
//! A right context is a limited version of lookahead. A rule can have at most one right context.
//! When a rule has right context, after the regex for the rule matches, we run the DFA for the
//! right context with cloned input stream. Only if it matches we consider the the rule as a match.
//! This provides a simple "lookahead" support, which should be good enough when lexing programming
//! languages.

use crate::ast::{Regex, Var};
use crate::collections::Map;
// use crate::dfa::simplify::{simplify, Trans};
use crate::dfa::{StateIdx, DFA};
use crate::nfa::NFA;
use crate::nfa_to_dfa::nfa_to_dfa;

#[derive(Debug)]
pub struct RightCtxDFAs<S> {
    dfas: Vec<DFA<S, ()>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct RightCtxIdx(usize);

impl RightCtxIdx {
    pub fn as_usize(&self) -> usize {
        self.0
    }
}

impl<A> RightCtxDFAs<A> {
    pub fn new() -> Self {
        RightCtxDFAs { dfas: vec![] }
    }

    pub fn iter(&self) -> impl Iterator<Item = (RightCtxIdx, &DFA<A, ()>)> {
        self.dfas
            .iter()
            .enumerate()
            .map(|(i, dfa)| (RightCtxIdx(i), dfa))
    }

    pub fn into_iter(self) -> impl Iterator<Item = (RightCtxIdx, DFA<A, ()>)> {
        self.dfas
            .into_iter()
            .enumerate()
            .map(|(i, dfa)| (RightCtxIdx(i), dfa))
    }
}

impl RightCtxDFAs<StateIdx> {
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

    /*
    pub fn simplify(self) -> RightCtxDFAs<Trans<()>> {
        RightCtxDFAs {
            dfas: self
                .dfas
                .into_iter()
                .map(|dfa| simplify::<(), ()>(dfa, &mut Default::default()))
                .collect(),
        }
    }
    */
}
