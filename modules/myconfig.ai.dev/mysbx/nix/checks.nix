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

  # The pi integration's mounts, evaluated against the REAL reference
  # host (`test-f13` enables both mysbx and pi-coding-agent, hosts/
  # host.f13/ai.f13.nix): since bd myconfig-576 the mounts bind from a
  # self-contained store tree of dereferenced copies (built by the
  # shared `mkSandboxConfig` helper, ../../nix/sandbox-config.nix, from
  # `mysbxSandboxConfig` in
  # ../../programs/programs.pi-coding-agent/default.nix), because the
  # podman-gvisor backend mounts nothing from the host /nix/store and the
  # raw home-manager symlink tree dangles inside the container. The
  # assertion pin runs at EVAL time (a throw builds no derivation), and
  # the check derivation REALISES the pi sandbox-config tree + greps its
  # tree,
  # because only the build can prove the copies exist as real files
  # (the eval-level shape is necessary, not sufficient: a `path =
  # "/nix/store/…"` mount whose tree still contains symlinks would pass
  # the eval assertions and dangle in the container exactly as before).
  #
  # Not in ./config-eval-test.nix: that minimal evaluation imports the
  # mysbx module alone, and the pi module reads options across the whole
  # `myconfig.ai.dev` umbrella (workmux, skills, …) — evaluating it
  # standalone means re-importing half of `modules/` by hand. Using the
  # reference host is the same pattern as ../../../tests/microvm.nix
  # (`self.nixosConfigurations.test-f13`).
  mysbx-pi-mounts-test =
    let
      lib = inputs.nixpkgs.lib;
      cfg = self.nixosConfigurations.test-f13.config;
      piMounts = builtins.filter (
        m: lib.hasPrefix "/mysbx-home/.pi" m.dest || m.dest == "/mysbx-home/.agents/skills"
      ) cfg.myconfig.ai.dev.mysbx.config.mounts;
      # The store tree every pi mount binds from. Derived from the
      # extensions mount's path (NOT via `builtins.match` — that strips
      # the string context, and the tree would not become an input of
      # this check derivation, so the build could not see it).
      extMountPath = (builtins.head piMounts).path;
      sandboxTree = lib.removeSuffix "/.pi/agent/extensions" extMountPath;
      expectedDests = [
        "/mysbx-home/.pi/agent/extensions"
        "/mysbx-home/.pi/agent/agents"
        "/mysbx-home/.pi/agent/prompts"
        "/mysbx-home/.pi/agent/themes"
        "/mysbx-home/.pi/agent/keybindings.json"
        "/mysbx-home/.agents/skills"
      ];
      evalAssertions = [
        {
          assertion = builtins.length piMounts == builtins.length expectedDests;
          message = "mysbx-pi-mounts-test: expected ${toString (builtins.length expectedDests)} pi mounts, got ${toString (builtins.length piMounts)}";
        }
        {
          assertion = builtins.all (
            m: lib.hasPrefix "/nix/store/" m.path && !lib.hasPrefix "~" m.path
          ) piMounts;
          message = "mysbx-pi-mounts-test: every pi mount path must be an absolute store path (bd myconfig-576), got: ${
            lib.concatStringsSep ", " (map (m: m.path) piMounts)
          }";
        }
        {
          assertion = builtins.all (m: lib.hasPrefix sandboxTree m.path) piMounts;
          message = "mysbx-pi-mounts-test: every pi mount path must sit inside the pi-sandbox-config derivation";
        }
        {
          assertion = builtins.all (m: m.mode == "ro") piMounts;
          message = "mysbx-pi-mounts-test: every pi mount must stay read-only";
        }
      ];
      failures = builtins.filter (a: !a.assertion) evalAssertions;
      evalGate =
        if failures != [ ] then
          throw "mysbx-pi-mounts-test: ${toString (builtins.length failures)} eval assertion(s) failed:\n  - ${
            lib.concatMapStringsSep "\n  - " (f: f.message) failures
          }"
        else
          "ok";
    in
    pkgs.runCommand "mysbx-pi-mounts-test"
      {
        inherit evalGate sandboxTree;
        nativeBuildInputs = with pkgs; [
          gnugrep
          findutils
        ];
      }
      ''
        fail() {
          echo "mysbx-pi-mounts-test: $*" >&2
          exit 1
        }

        [ "$evalGate" = ok ] || { echo "mysbx-pi-mounts-test: eval gate: $evalGate" >&2; exit 1; }

        # The mounted subtrees must exist in the copied tree ...
        for sub in .pi/agent/extensions .pi/agent/agents .pi/agent/prompts .pi/agent/themes .pi/agent/keybindings.json .agents/skills; do
          test -e "$sandboxTree/$sub" || fail "$sub is missing from the pi-sandbox-config tree"
        done

        # ... and be REAL files: a symlink anywhere below the mounted
        # subtrees dangles inside the podman-gvisor container, which
        # has no /nix/store (bd myconfig-576). The whole tree is
        # asserted, not just the mount points — the mounts are the
        # parents of everything pi reads.
        if [ -n "$(find "$sandboxTree" -type l -print -quit)" ]; then
          find "$sandboxTree" -type l >&2
          fail "the pi-sandbox-config tree contains symlinks"
        fi

        mkdir "$out"
      '';

  mysbx-completions =
    let
      completion = ../mysbx-rs/completions/mysbx.fish;
      usage = ../mysbx-rs/src/usage.txt;
      lib = ../mysbx-rs/src/lib.rs;
    in
    pkgs.runCommand "mysbx-completions"
      {
        inherit usage lib;
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

        # Every dispatch word of usage.txt is offered as a subcommand —
        # the verbs of the dispatcher (src/lib.rs) plus the closed
        # session sub-verb group (src/sessionverbs.rs, D7) and the
        # closed worktree sub-verb group (src/worktreeverbs.rs,
        # docs/design/worktree.md W1). The EXPECTED set is EXTRACTED
        # from usage.txt, not hand-listed here: the check is the sync
        # CONTRACT between usage.txt and the completion, so a verb or
        # flag added to usage.txt fails the build until the completion
        # offers it.
        verbs=$(sed -n '/^Commands:/,/^Options:/p' "$usage" \
          | grep -oE '^  [a-z][a-z-]*' | tr -d ' ' | sort -u)
        for sub in $verbs; do
          grep -q -- "-a $sub" "$installed" || fail "no completion for subcommand: $sub"
        done
        session_verbs=$(sed -n '/^Commands:/,/^Options:/p' "$usage" \
          | sed -n 's/^  session \([a-z][a-z-]*\).*/\1/p' | sort -u)
        for sub in $session_verbs; do
          grep -q -- "-a $sub" "$installed" || fail "no completion for session sub-verb: $sub"
        done
        worktree_verbs=$(sed -n '/^Commands:/,/^Options:/p' "$usage" \
          | sed -n 's/^  worktree \([a-z][a-z-]*\).*/\1/p' | sort -u)
        for sub in $worktree_verbs; do
          grep -q -- "-a $sub" "$installed" || fail "no completion for worktree sub-verb: $sub"
        done
        # usage.txt documents every verb twice — the Usage header and
        # the Commands section — so a verb of the dispatcher that
        # usage.txt does not name cannot be extracted above; assert the
        # two agree to keep the extraction honest
        dispatchers=$(sed -n 's/^        Some("\([a-z][a-z-]*\)") .*/\1/p' "$lib" | sort -u)
        [ "$verbs" = "$dispatchers" ] \
          || fail "usage.txt verbs and src/lib.rs dispatcher differ:$verbs | $dispatchers"

        # the worktree handles come from the __worktrees registry
        # (docs/design/worktree.md W2)
        grep -q 'mysbx_worktrees' "$installed" || fail "no worktree-registry lookup"

        # every option usage.txt names (`-l <name>`, i.e. the `--<name>`
        # long form) is completed — the Options headers plus the
        # verb-tail flags documented only inside the command
        # descriptions (`init --approve-git-dirs`,
        # `merge --no-ff|--ff|--squash`, `session destroy --force`,
        # `gvisor-load-image --force|--test|--image`) — extracted,
        # like the verbs above
        opts=$(grep -oE -- '--[a-z][a-z-]*' "$usage" | sed 's/^--//' | sort -u)
        for opt in $opts; do
          grep -q -- "-l $opt" "$installed" || fail "no completion for option: --$opt"
        done
        for short in $(sed -n '/^Options:/,$p' "$usage" | sed -n 's/^  -\([A-Za-z]\),.*/\1/p' | sort -u); do
          grep -q -- "-s $short" "$installed" || fail "no completion for -$short"
        done

        # the multiplexer values are the closed set of config.rs NAMES
        grep -q -- "-a 'tmux workmux herdr aoe orca none'" "$installed" \
          || fail "the multiplexer completions are not the closed set of NAMES"

        # session names come from the clones/ registry (workspace.md D2)
        grep -q 'mysbx_sessions' "$installed" || fail "no session-registry lookup"

        touch "$out"
      '';

  # bd myconfig-bf2: the `MYSBX_NIX_CONF` pin must be a file named
  # `nix.conf` inside a directory, because the nono backend's exec
  # environment sets `NIX_CONF_DIR` to the pin's PARENT — a bare store
  # file made that parent `/nix/store`, and nix read no configuration
  # at all (no flakes inside the sandbox). Two gates: an eval-time
  # assertion on the path shape, and a build-time one where the REAL
  # pinned nix resolves its configuration from the REAL pinned
  # directory — `nix config show experimental-features` must report
  # `flakes`, which is only possible when the file was actually
  # loaded.
  mysbx-nix-conf-pin-test =
    let
      lib = inputs.nixpkgs.lib;
      pin = pkg.passthru.sandboxNixConfDir;
      shapeOk = lib.hasSuffix "/nix.conf" pkg.passthru.sandboxNixConfPin;
      evalGate =
        if shapeOk then
          "ok"
        else
          throw "mysbx-nix-conf-pin-test: MYSBX_NIX_CONF must be <dir>/nix.conf, got ${pkg.passthru.sandboxNixConfPin}";
    in
    pkgs.runCommand "mysbx-nix-conf-pin-test"
      {
        inherit evalGate pin;
        nativeBuildInputs = [ pkgs.nix ];
      }
      ''
        fail() {
          echo "mysbx-nix-conf-pin-test: $*" >&2
          exit 1
        }

        [ "$evalGate" = ok ] || { echo "mysbx-nix-conf-pin-test: eval gate: $evalGate" >&2; exit 1; }

        # the pin the wrapper sets: a directory whose `nix.conf` exists
        test -f "$pin/nix.conf" \
          || fail "the pin's parent has no nix.conf: $pin"

        # the REAL nix resolves the REAL configuration from the
        # directory the nono backend would set NIX_CONF_DIR to —
        # the exact acceptance of bd myconfig-bf2, checked in an
        # empty environment so no host config can mask a miss
        features=$(env -i \
          PATH="$PATH" \
          HOME="$TMPDIR" \
          NIX_CONF_DIR="$pin" \
          nix config show experimental-features)
        case "$features" in
          *flakes*) ;;
          *) fail "nix did not load the pinned nix.conf (got: $features)" ;;
        esac

        mkdir "$out"
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
