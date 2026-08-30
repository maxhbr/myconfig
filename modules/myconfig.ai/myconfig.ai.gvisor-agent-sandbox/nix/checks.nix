# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# CI checks for the Rust `agent-gvisor` rewrite (../docs/spec.md):
#
#   agent-gvisor-tests        cargo test — the full parity suite in
#                             ../rust/tests/ (podman argv, state layout,
#                             exact error messages, shell quoting, home
#                             seeding), against recording git/podman stubs
#                             generated at test time.
#   agent-gvisor-cli-harness end-to-end CLI flows (doctor, a full session
#                             cycle, list) through the same stubs, driven
#                             by ../tests/agent-gvisor-cli-harness.sh.
#   agent-gvisor-completions the fish tab completion shipped by the
#                             production package: installed at the vendor
#                             path, identical to the maintained source,
#                             parsed by fish -n, and covering every
#                             subcommand and every option documented in
#                             ../rust/src/usage.txt.
#
# The first two build the crate WITHOUT the production wrapper: the wrapper
# prepends the real git/podman to PATH, which would shadow the stubs.
{
  self,
  inputs,
  system,
}:
let
  pkgs = inputs.nixpkgs.legacyPackages.${system};

  crate = pkgs.rustPlatform.buildRustPackage {
    pname = "agent-gvisor";
    version = "0.1.0";
    src = ../rust;
    # Zero dependencies by design, so the lockfile is trivial and no
    # `outputHashes` can ever be needed.
    cargoLock.lockFile = ../rust/Cargo.lock;
    doCheck = true;
  };
in
{
  agent-gvisor-tests = crate;

  agent-gvisor-cli-harness =
    pkgs.runCommand "agent-gvisor-cli-harness"
      {
        nativeBuildInputs = with pkgs; [
          bash
          coreutils
          gnugrep
        ];
      }
      ''
        BIN=${crate}/bin/agent-gvisor \
        STUBS=${../rust/tests/stubs} \
          bash ${../tests/agent-gvisor-cli-harness.sh}
        touch "$out"
      '';

  agent-gvisor-completions =
    let
      # The production package (./agent-gvisor.nix) installs the
      # completion in postInstall. Build it here with neutral defaults so
      # the check needs neither the multi-hundred-MB sandbox image nor the
      # gvisor (runsc) build — only the completion file is under test.
      pkg = pkgs.callPackage ./agent-gvisor.nix {
        agent-gvisor-image = null;
        defaultImage = "localhost/agent-dev:latest";
        defaultRuntime = "runsc";
      };
      completion = ../rust/completions/agent-gvisor.fish;
      usage = ../rust/src/usage.txt;
    in
    pkgs.runCommand "agent-gvisor-completions"
      {
        nativeBuildInputs = with pkgs; [
          fish
          gnugrep
        ];
      }
      ''
        fail() {
          echo "agent-gvisor-completions: $*" >&2
          exit 1
        }

        installed="${pkg}/share/fish/vendor_completions.d/agent-gvisor.fish"
        test -f "$installed" || fail "not installed at: $installed"

        # the installed file is the maintained source, byte for byte
        cmp ${completion} "$installed" || fail "installed completion differs from ${completion}"

        # it must parse as fish
        fish --no-execute "$installed" || fail "fish -n rejects the completion"

        # every dispatch word (docs/spec.md §1) is offered as a subcommand
        for sub in start list status run logs shell stop merge destroy doctor help; do
          grep -q -- "-a $sub" "$installed" || fail "no completion for subcommand: $sub"
        done

        # every option documented in usage.txt is completed (`-l <name>`,
        # i.e. the `--<name>` long form), plus the two that usage.txt only
        # mentions outside its option tables (--name, --delete-branch)
        for opt in $(
          sed -n 's/^  --\([a-z0-9-]*\).*/\1/p' ${usage}
          echo name
          echo delete-branch
        ); do
          grep -q -- "-l $opt" "$installed" || fail "no completion for option: --$opt"
        done

        # session names come from the registry (docs/spec.md §4/§8)
        grep -q 'AGENT_GVISOR_STATE' "$installed" || fail "no session-registry lookup"

        touch "$out"
      '';
}
