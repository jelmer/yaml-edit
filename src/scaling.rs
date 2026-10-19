//! A check that an operation takes time linear in the size of its input.

use std::time::{Duration, Instant};

/// Assert that `run` on the input `setup` builds for size `4 * n` takes
/// well under the 16 times as long as for size `n` that a quadratic
/// algorithm would.
///
/// Each sample runs the input often enough to take a while, so that timer
/// resolution does not matter, and the fastest of several samples of each
/// size, alternating between the two, is compared. A busy machine only
/// makes some samples slower, so it should not fail the check.
pub(crate) fn assert_scales_linearly<T>(
    what: &str,
    n: usize,
    setup: impl Fn(usize) -> T,
    run: impl Fn(&T),
) {
    let inputs = [setup(n), setup(4 * n)];
    let time = |input: &T, repeats: u32| {
        let start = Instant::now();
        for _ in 0..repeats {
            run(input);
        }
        start.elapsed()
    };
    let once = time(&inputs[0], 1);
    let repeats = (Duration::from_millis(10).as_nanos() / once.as_nanos().max(1)).clamp(1, 1000);
    let repeats = u32::try_from(repeats).expect("clamped to 1000");
    let mut fastest = [Duration::MAX; 2];
    for _ in 0..7 {
        for (input, fastest) in inputs.iter().zip(&mut fastest) {
            *fastest = (*fastest).min(time(input, repeats));
        }
    }
    let [small, large] = fastest;
    assert!(
        large < small * 10,
        "{what} looks superlinear: size {n} took {small:?}, size {} took {large:?}",
        4 * n
    );
}
