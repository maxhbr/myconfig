# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# CI checks for the Rust `agent-gvisor` CLI (../docs/spec.md):
#
#   agent-gvisor-tests        cargo test — the full behavioural suite in
#                             ../rust/tests/ (podman argv, state layout,
#                             exact error messages, shell quoting, home
#                             seeding), against recording git/podman stubs
#                             generated at test time.
#   agent-gvisor-cli-harness end-to-end CLI flows (doctor, a full session
#                             cycle, list) through the same stubs, driven
#                             by ../tests/agent-gvisor-cli-harness.sh.
#   agent-gvisor-init        the in-container entrypoint wrapper
#                             (./agent-gvisor-init.sh): the `--nix`
#                             preflight must fail closed when the store
#                             volume or the Nix state directories are not
#                             writable, driven by
#                             ../tests/agent-gvisor-init-harness.sh.
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
    # ../rust/tests/branch_lifecycle.rs drives the REAL git (the stubs cannot
    # model refs, fast-forwards and tags), so the test phase needs it on
    # PATH; without it every scenario there dies in `git_in` with ENOENT.
    nativeCheckInputs = [ pkgs.git ];
    # …and real git needs a committer identity and a writable HOME, which the
    # build sandbox has neither of (outside the sandbox the tests silently
    # borrowed the developer's ~/.gitconfig).
    preCheck = ''
      export HOME=$TMPDIR
      export GIT_CONFIG_GLOBAL=/dev/null
      export GIT_CONFIG_SYSTEM=/dev/null
      export GIT_AUTHOR_NAME=agent-gvisor-tests
      export GIT_AUTHOR_EMAIL=agent-gvisor-tests@invalid
      export GIT_COMMITTER_NAME=agent-gvisor-tests
      export GIT_COMMITTER_EMAIL=agent-gvisor-tests@invalid
    '';
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

  agent-gvisor-init =
    pkgs.runCommand "agent-gvisor-init"
      {
        nativeBuildInputs = with pkgs; [
          bash
          coreutils
          gnugrep
          gnused
        ];
      }
      ''
        INIT=${./agent-gvisor-init.sh} \
          bash ${../tests/agent-gvisor-init-harness.sh}
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
        for sub in start list status run logs shell stop merge fetch push destroy workmux doctor help; do
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
