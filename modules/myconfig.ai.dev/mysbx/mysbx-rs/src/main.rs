// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
fn main() {
    // `mysbx run --dry-run -- ls | wc -l` (cli.md D9) and the general
    // Unix expectation of `| head`: a closed pipe must end us quietly
    // with SIGPIPE, not panic with "failed printing to stdout: Broken
    // pipe" — the Rust runtime ignores SIGPIPE by default, so restore
    // the default action before anything prints.
    unsafe {
        libc_sigpipe_dfl();
    }
    std::process::exit(mysbx::run(std::env::args().skip(1).collect()));
}

/// Restore `SIGPIPE` to its default action, with zero dependencies:
/// `signal(2)` via the raw libc symbol (the same approach the std
/// library uses internally). No-op on non-unix (the crate targets
/// unix).
#[cfg(unix)]
unsafe fn libc_sigpipe_dfl() {
    #[allow(non_snake_case)]
    extern "C" {
        fn signal(signum: i32, handler: usize) -> usize;
    }
    const SIGPIPE: i32 = 13;
    const SIG_DFL: usize = 0;
    signal(SIGPIPE, SIG_DFL);
}
