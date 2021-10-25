use lexgen_util::Loc;

pub fn ignore_pos<A, E, L>(ret: Option<Result<(L, A, L), E>>) -> Option<Result<A, E>> {
    ret.map(|res| res.map(|(_, a, _)| a))
}

pub fn next<A, E, L>(iter: &mut dyn Iterator<Item = Result<(L, A, L), E>>) -> Option<Result<A, E>> {
    ignore_pos(iter.next())
}

pub fn loc(line: u32, col: u32, byte_idx: usize) -> Loc {
    Loc {
        line,
        col,
        byte_idx,
    }
}
