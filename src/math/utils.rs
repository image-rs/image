use std::cmp::PartialOrd;

pub fn clamp<N>(a: N, min: N, max: N) -> N
where N: PartialOrd {
    match () {
        () if a < min => min,
        () if a > max => max,
        _ => a
    }
}
