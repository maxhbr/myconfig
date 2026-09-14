# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# CI checks for the Rust `mysbx` CLI (../docs/TODOs/mvp-6-packaging.md):
#
#   mysbx-tests   cargo test — the full behavioural suite in
#                 ../mysbx-rs/tests/ (golden argv tests, layer merge,
#                 repo discovery, CLI subprocess flows).
#
#   mysbx-generated-config-test
#                 module-EVALUATION test of the user configuration layer
#                 ../default.nix generates (review-4 item 4) — the cargo
#                 suite hand-writes its `config.toml` and cannot see a
#                 regression in the generator. See ./config-eval-test.nix.
#
#   mysbx-completions
#                 the fish tab completion shipped by the package
#                 (../mysbx-rs/completions/mysbx.fish, installed by
#                 ./mysbx.nix): installed byte-for-byte, parses as fish,
#                 and every subcommand and option of usage.txt is
#                 completed. Same pattern as the gvisor tier's
#                 `agent-gvisor-completions` check.
#
# Wired into `nix flake check` for `x86_64-linux` in `flake.nix`, following
# ../../sandboxes/myconfig.ai.gvisor-agent-sandbox/nix/checks.nix.
#
# Deliberately NOT a check here (mvp-6, "Explicitly not in this item"):
# bubblewrap is not on the test PATH. The two real-execution tests in
# tests/cli.rs are written to *skip* when no runnable bwrap is found, and
# executing bwrap inside `nix flake check` would need nested user
# namespaces — environment-dependent, so the argv golden tests are the CI
# gate and real execution stays the operator's manual acceptance step.
{
  self,
  inputs,
  system,
}:
let
  pkgs = inputs.nixpkgs.legacyPackages.${system};

  # The full package (./mysbx.nix), whose `passthru.crate` is the bare
  # rustPlatform.buildRustPackage — the tests set their own `MYSBX_*`
  # variables and must not see the wrapper's pins.
  pkg = pkgs.callPackage ../nix/mysbx.nix { };
  crate = pkg.passthru.crate;
  # Known and accepted (same property as the gvisor tier's check): CI
  # tests this crate from the locked `inputs.nixpkgs`, which can differ
  # slightly from the host-eval nixpkgs the wrapped binary on a host
  # was built with. The crate is dependency-free, so the drift surface
  # is the toolchain, not the library set.
in
{
  # The crate itself, with `doCheck = true`: `cargo test` in the build
  # sandbox. Same pattern as the gvisor tier's `agent-gvisor-tests`.
  # The generator, evaluated: what a host actually gets in
  # `~/.config/mysbx/config.toml` (review-4 item 4).
  mysbx-generated-config-test = import ./config-eval-test.nix { inherit inputs system; };

  mysbx-completions =
    let
      completion = ../mysbx-rs/completions/mysbx.fish;
    in
    pkgs.runCommand "mysbx-completions"
      {
        nativeBuildInputs = with pkgs; [
          fish
          gnugrep
        ];
      }
      ''
        fail() {
          echo "mysbx-completions: $*" >&2
          exit 1
        }

        installed="${pkg}/share/fish/vendor_completions.d/mysbx.fish"
        test -f "$installed" || fail "not installed at: $installed"

        # the installed file is the maintained source, byte for byte
        cmp ${completion} "$installed" || fail "installed completion differs from ${completion}"

        # it must parse as fish
        fish --no-execute "$installed" || fail "fish -n rejects the completion"

        # every dispatch word of usage.txt is offered as a subcommand —
        # the verbs of the dispatcher (src/lib.rs) plus the closed
        # session sub-verb group (src/sessionverbs.rs, D7) and the
        # closed worktree sub-verb group (src/worktreeverbs.rs,
        # docs/design/worktree.md W1)
        for sub in run gui init edit fetch merge push diff version help session worktree; do
          grep -q -- "-a $sub" "$installed" || fail "no completion for subcommand: $sub"
        done
        for sub in list destroy; do
          grep -q -- "-a $sub" "$installed" || fail "no completion for session sub-verb: $sub"
        done
        for sub in list diff hunk; do
          grep -q -- "-a $sub" "$installed" || fail "no completion for worktree sub-verb: $sub"
        done

        # the worktree handles come from the __worktrees registry
        # (docs/design/worktree.md W2)
        grep -q 'mysbx_worktrees' "$installed" || fail "no worktree-registry lookup"

        # every option of usage.txt is completed (`-l <name>`, i.e. the
        # `--<name>` long form), plus the verb-tail flags usage.txt
        # documents only inside the command descriptions
        # (`init --approve-git-dirs`, `merge --no-ff|--ff|--squash`,
        # `session destroy --force`) and the two short flags
        for opt in \
          dry-run \
          verbose \
          multiplexer \
          session \
          ro \
          rw \
          result \
          timeout \
          help \
          version \
          approve-git-dirs \
          no-ff \
          ff \
          squash \
          force \
        ; do
          grep -q -- "-l $opt" "$installed" || fail "no completion for option: --$opt"
        done
        grep -q -- "-s h" "$installed" || fail "no completion for -h"
        grep -q -- "-s V" "$installed" || fail "no completion for -V"

        # the multiplexer values are the closed set of config.rs NAMES
        grep -q -- "-a 'tmux workmux herdr aoe orca none'" "$installed" \
          || fail "the multiplexer completions are not the closed set of NAMES"

        # session names come from the clones/ registry (workspace.md D2)
        grep -q 'mysbx_sessions' "$installed" || fail "no session-registry lookup"

        touch "$out"
      '';

  mysbx-tests = crate.overrideAttrs (old: {
    doCheck = true;
    # The CLI tests drive the built binary as a subprocess with a
    # hand-rolled fixed environment (tests/cli.rs::spawn); `TMPDIR` and a
    # writable HOME suffice. `cargo`/`rustc` come from the stdenv set up
    # by buildRustPackage.
    #
    # The session-clone tests (tests/cli.rs::git_repo, workspace.md
    # D1-D5) drive the REAL git — the creation decision probes refs
    # and HEAD, which no stub can model — so the test phase needs it
    # on PATH, and real git needs a committer identity and a locked
    # config (same pattern as the gvisor tier's agent-gvisor-tests).
    nativeCheckInputs = (old.nativeCheckInputs or [ ]) ++ [ pkgs.git ];
    preCheck = ''
      export HOME=$TMPDIR
      export GIT_CONFIG_GLOBAL=/dev/null
      export GIT_CONFIG_SYSTEM=/dev/null
      export GIT_AUTHOR_NAME=mysbx-tests
      export GIT_AUTHOR_EMAIL=mysbx-tests@invalid
      export GIT_COMMITTER_NAME=mysbx-tests
      export GIT_COMMITTER_EMAIL=mysbx-tests@invalid
    '';
  });
}
