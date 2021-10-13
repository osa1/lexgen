pub fn ignore_pos<A, E>(ret: Option<Result<(usize, A, usize), E>>) -> Option<Result<A, E>> {
    ret.map(|res| res.map(|(_, a, _)| a))
}

pub fn next<A, E>(
    iter: &mut dyn Iterator<Item = Result<(usize, A, usize), E>>,
) -> Option<Result<A, E>> {
    ignore_pos(iter.next())
}
