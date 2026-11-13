// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
fn main() {
    // `mysbx run --dry-run -- ls | wc -l` (cli.md D9) and the general
    // Unix expectation of `| head`: a closed pipe must end us quietly
    // with SIGPIPE, not panic with "failed printing to stdout: Broken
    // pipe" — the Rust runtime ignores SIGPIPE by default, so restore
    // the default action before anything prints. The libc binding
    // itself lives once in `mysbx::sig`.
    #[cfg(unix)]
    unsafe {
        mysbx::sig::to_default(mysbx::sig::PIPE);
    }
    std::process::exit(mysbx::run(std::env::args().skip(1).collect()));
}
