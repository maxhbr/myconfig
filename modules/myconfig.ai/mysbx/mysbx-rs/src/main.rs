// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
fn main() {
    std::process::exit(mysbx::run(std::env::args().skip(1).collect()));
}
