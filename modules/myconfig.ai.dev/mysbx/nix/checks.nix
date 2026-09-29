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
#   mysbx-krun-wrapper-test
#                 the podman-krun runtime pin (docs/design/backends.md D2):
#                 the wrapper carries MYSBX_KRUN_RUNTIME at a crun built
#                 with the libkrun handler (`+LIBKRUN` on its own feature
#                 line), and a `backend = "podman-krun"` dry run emits
#                 `--runtime=<that path>` — the runtime swap is the only
#                 argv difference, so the dry run doubles as the swap proof.
#
#   mysbx-krun-guest-nix-test
#                 the guest nix wrappers of the podman-krun backend
#                 (./krun-guest-nix.nix, docs/design/backends.md D2).
#
#   mysbx-completions
#                 the fish tab completion shipped by the package
#                 (../mysbx-rs/completions/mysbx.fish, installed by
#                 ./mysbx.nix): installed byte-for-byte, parses as fish,
#                 and every subcommand and option of usage.txt is
#                 completed. Same pattern as the gvisor tier's
#                 `agent-gvisor-completions` check.
#
#   mysbx-herdr-entry-test
#                 static-contract check of the herdr entry script
#                 (./herdr-entry.nix): it must configure herdr's
#                 `[worktrees] directory` to point at the workmux
#                 `<repo>__worktrees` sibling, ONLY when the sibling
#                 exists (the in-sandbox `-d` test is the bind test,
#                 because the argv builder mounts it rw exactly then),
#                 and it must never create the sibling — mysbx's D13
#                 guard. Bash-syntax checked with `bash -n`.
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
  # The FULL default wrapper — re-called WITH the nono pin (bd
  # myconfig-pux): a host's `default.nix` passes `nono =
  # cfg.nono.package` (default `pkgs.nono`), so the pins
  # MYSBX_NONO/MYSBX_NONO_PROFILE/MYSBX_ENV must exist on the nono ≠
  # null path. The checks below read this derivation's wrapper env.
  pkgNono = pkgs.callPackage ../nix/mysbx.nix {
    nono = pkgs.nono;
    ssh-keygen = pkgs.openssh;
  };
  # The wrapper script of the full package — the file postBuild
  # produced; its content carries every `--set` pin.
  wrapped = pkgs.runCommand "mysbx-wrapped-content" { } ''
    cat "${pkgNono}/bin/mysbx" > $out
  '';
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

  # The nested-podman guest tree (bd myconfig-6di.5.8): the storage
  # wrapper at bin/podman (bd myconfig-6di.5.16) and the bytes podman
  # will read inside the krun guest. Static by
  # design — the nested run itself is live validation (bd
  # myconfig-6di.5.7), which this sandbox cannot do without /dev/kvm.
  # The TOML files are PARSED and compared table by table: podman
  # reads each key only from its own table and silently ignores a
  # misplaced one (containers/common config/new.go logs undecoded
  # keys at debug level), so a line grep cannot catch a key in the
  # wrong table. Any extra, missing or moved key fails.
  mysbx-krun-guest-conf-test =
    let
      guestConf = pkgs.callPackage ../nix/krun-guest-conf.nix { };
      expected = {
        containers = {
          containers = {
            apparmor_profile = "";
            cgroups = "disabled";
            log_driver = "k8s-file";
            netns = "host";
          };
          engine = {
            cgroup_manager = "cgroupfs";
            events_logger = "file";
            no_pivot_root = true;
            tmp_dir = "/run/containers/libpod";
            image_copy_tmp_dir = "storage";
            runtimes_flags.crun = [ "root=/run/containers/crun" ];
          };
          network.network_config_dir = "/var/tmp/containers/networks";
        };
        storage = {
          storage = {
            driver = "overlay";
            # Below the guest tmpfs mounts of the podman wrapper.
            graphroot = "/var/tmp/containers/storage";
            runroot = "/run/containers/storage";
          };
        };
      };
      expectedJson = pkgs.writeText "mysbx-krun-guest-conf-expected.json" (builtins.toJSON expected);
      compare = pkgs.writeText "mysbx-krun-guest-conf-compare.py" ''
        import json, sys, tomllib

        etc, expected_path = sys.argv[1], sys.argv[2]
        expected = json.load(open(expected_path))
        failed = False
        for name, want in expected.items():
            path = f"{etc}/containers/{name}.conf"
            with open(path, "rb") as f:
                got = tomllib.load(f)
            if got != want:
                failed = True
                print(f"{path}: parsed tables differ from the pinned set", file=sys.stderr)
                print(f"  got:      {json.dumps(got, sort_keys=True)}", file=sys.stderr)
                print(f"  expected: {json.dumps(want, sort_keys=True)}", file=sys.stderr)
        sys.exit(1 if failed else 0)
      '';
    in
    pkgs.runCommand "mysbx-krun-guest-conf-test" { nativeBuildInputs = [ pkgs.python3 ]; } ''
      fail() {
        echo "mysbx-krun-guest-conf-test: $*" >&2
        exit 1
      }
      python3 ${compare} "${guestConf}/etc" ${expectedJson} \
        || fail "the guest containers.conf/storage.conf do not match the pinned tables"
      # containers/image refuses every pull without a policy file.
      python3 -c 'import json, sys; p = json.load(open(sys.argv[1])); sys.exit(0 if p["default"] == [{"type": "insecureAcceptAnything"}] else 1)' \
        "${guestConf}/etc/containers/policy.json" \
        || fail "policy.json must exist, parse as JSON and carry the default insecureAcceptAnything policy"
      # bin/podman is the storage wrapper: it mounts guest tmpfs at
      # both storage mounts as root, fails instead of falling back,
      # and execs exactly the pinned podman.
      wrapper="${guestConf}/bin/podman"
      [ -x "$wrapper" ] || fail "bin/podman (the storage wrapper) is missing"
      [ "$(readlink -f "$wrapper")" = "$(readlink -f "${guestConf.wrapper}/bin/podman")" ] \
        || fail "bin/podman is not the storage wrapper"
      for dir in /var/tmp/containers /run/containers; do
        grep -qF "$dir" "$wrapper" || fail "the wrapper does not mount $dir"
      done
      grep -q 'mount -t tmpfs' "$wrapper" || fail "the wrapper must mount a guest tmpfs"
      grep -q 'exit 125' "$wrapper" || fail "a failed mount must be an error, never a fallback"
      grep -qF 'exec ${guestConf.podman}/bin/podman "$@"' "$wrapper" \
        || fail "the wrapper must exec the pinned podman"
      # The build user is not root: the pass-through path, end to end.
      [ "$(id -u)" -ne 0 ] || fail "the check expects a non-root build user"
      HOME=$TMPDIR "$wrapper" --version | grep -q '^podman version ${guestConf.podman.version}$' \
        || fail "the wrapper does not pass through to the pinned podman as non-root"
      grep -q '^agent:100000:65536$' "${guestConf}/etc/subuid" \
        || fail "the subuid range for the guest-root user is missing"
      grep -q '^agent:100000:65536$' "${guestConf}/etc/subgid" \
        || fail "the subgid range for the guest-root user is missing"
      touch $out
    '';

  # The guest nix tree (bd myconfig-pz6, docs/design/backends.md D2):
  # every nix entry point is the guest-root wrapper, the setup script
  # overlays the image store on guest tmpfs and copies the registered
  # database (skipping the unreadable lock files), a failure is exit 125, and the non-root path passes
  # through. The wrappers must also win the image's collision-ignoring
  # buildEnv against a plain nix. The mount itself is live validation.
  mysbx-krun-guest-nix-test =
    let
      guestNix = pkgs.callPackage ../nix/krun-guest-nix.nix {
        nixConfig = "substituters = https://cache.example";
      };
      imageRootLike = pkgs.buildEnv {
        name = "mysbx-krun-guest-nix-test-root";
        paths = [
          guestNix.nix
          guestNix
        ];
        pathsToLink = [ "/bin" ];
        ignoreCollisions = true;
      };
    in
    pkgs.runCommand "mysbx-krun-guest-nix-test" { } ''
      fail() {
        echo "mysbx-krun-guest-nix-test: $*" >&2
        exit 1
      }
      setup="${guestNix.setup}/bin/mysbx-krun-nix-setup"
      for name in ${pkgs.lib.concatStringsSep " " guestNix.names}; do
        w="${guestNix}/bin/$name"
        [ -x "$w" ] || fail "bin/$name is missing"
        grep -qF "$setup" "$w" || fail "bin/$name does not run the setup"
        grep -qF 'exec ${guestNix.nix}/bin/'"$name"' "$@"' "$w" \
          || fail "bin/$name does not exec the pinned nix entry point"
        for v in NIX_REMOTE=local NIX_STATE_DIR NIX_LOG_DIR NIX_CACHE_HOME TMPDIR; do
          grep -qF "export $v" "$w" || fail "bin/$name does not export $v"
        done
        grep -qF 'substituters = https://cache.example' "$w" \
          || fail "bin/$name does not carry the configured nix settings"
        [ "$(readlink -f "${imageRootLike}/bin/$name")" = "$(readlink -f "$w")" ] \
          || fail "bin/$name loses the buildEnv collision against the plain nix"
      done
      [ ! -e "${guestNix}/bin/nix-daemon" ] || fail "the guest has no daemon; bin/nix-daemon must not be wrapped"
      grep -qF 'lowerdir=/nix/store,upperdir=' "$setup" || fail "the setup does not overlay the image store"
      grep -q 'mount -t tmpfs' "$setup" || fail "the setup must mount a guest tmpfs"
      grep -qF '${guestNix.copyState}/bin/mysbx-krun-nix-copy-state /nix/var/nix' "$setup" \
        || fail "the setup must copy (not overlay) the image database"
      # The image's big-lock/reserved are unreadable to guest root.
      src="$TMPDIR/var-nix"
      mkdir -p "$src"/db "$src"/gcroots/docker "$src"/profiles/per-user "$src"/temproots
      for f in db.sqlite db.sqlite-wal db.sqlite-shm schema; do echo "$f" > "$src/db/$f"; done
      touch "$src"/db/big-lock "$src"/db/reserved
      chmod 000 "$src"/db/big-lock "$src"/db/reserved
      ln -s /nix/store/00000000000000000000000000000000-x "$src"/gcroots/docker/x
      "${guestNix.copyState}/bin/mysbx-krun-nix-copy-state" "$src" "$TMPDIR/state" \
        || fail "the copy fails on an unreadable lock file in the database dir"
      for f in db.sqlite db.sqlite-wal db.sqlite-shm schema; do
        [ "$(cat "$TMPDIR/state/db/$f")" = "$f" ] || fail "the copy lost db/$f"
      done
      [ ! -e "$TMPDIR/state/db/big-lock" ] && [ ! -e "$TMPDIR/state/db/reserved" ] \
        || fail "the copy must skip db/big-lock and db/reserved"
      [ -L "$TMPDIR/state/gcroots/docker/x" ] && [ -d "$TMPDIR/state/profiles/per-user" ] \
        && [ -d "$TMPDIR/state/temproots" ] || fail "the copy lost the state layout outside db/"
      chmod 000 "$src"/db/db.sqlite
      ! "${guestNix.copyState}/bin/mysbx-krun-nix-copy-state" "$src" "$TMPDIR/state2" 2>/dev/null \
        || fail "an unreadable db.sqlite must fail the copy"
      grep -qF '/nix/var/nix/db/db.sqlite' "$setup" || fail "the setup must require the registered database"
      grep -q 'exit 125' "$setup" || fail "a failed mount must be an error, never a fallback"
      [ "$(id -u)" -ne 0 ] || fail "the check expects a non-root build user"
      "${guestNix}/bin/nix" --version | grep -q '^nix (Nix) ${guestNix.nix.version}$' \
        || fail "the wrapper does not pass through to the pinned nix as non-root"
      touch $out
    '';

  # The podman-krun runtime pin (docs/design/backends.md D2, bd
  # myconfig-6di.5.2): the wrapper must carry `MYSBX_KRUN_RUNTIME`
  # pointing at a crun built WITH the libkrun handler, and a
  # `backend = "podman-krun"` dry run must emit `--runtime=<that
  # path>` — the runtime swap is the ONLY argv difference, so the
  # dry run doubling as the swap proof is the honest static gate.
  mysbx-krun-wrapper-test =
    let
      # The wrapper WITH the krun pin, the shape a host's default.nix
      # builds (`krunRuntime = cfg.krun.runtime`).
      pkgKrun = pkgs.callPackage ../nix/mysbx.nix {
        krunRuntime = pkgs.crun.override { withLibkrun = true; };
        ssh-keygen = pkgs.openssh;
      };
    in
    pkgs.runCommand "mysbx-krun-wrapper-test"
      {
        nativeBuildInputs = [
          pkgs.git
        ];
      }
      ''
        fail() {
          echo "mysbx-krun-wrapper-test: $*" >&2
          exit 1
        }

        content=$(cat "${pkgKrun}/bin/mysbx")
        # 1. the wrapper pins the runtime, as a --set-default (the makeBinaryWrapper
        #    result carries the env table AND the embedded script line; strip
        #    the quotes before matching the flag, the same idiom the nono
        #    wrapper check uses).
        echo "$content" | grep -aq "MYSBX_KRUN_RUNTIME" \
          || fail "the wrapper does not pin MYSBX_KRUN_RUNTIME"
        crun=$(echo "$content" | tr -d "'" | awk '/--set-default MYSBX_KRUN_RUNTIME/ {print $3; exit}')
        case "$crun" in
          /nix/store/*-crun-*/bin/crun) ;;
          *) fail "the MYSBX_KRUN_RUNTIME value is not a crun store path: $crun" ;;
        esac
        test -x "$crun" || fail "the pinned runtime does not exist: $crun"
        # 2. the pinned crun carries the libkrun handler: its own
        #    feature line names +LIBKRUN.
        "$crun" --version 2>/dev/null | grep -q '+LIBKRUN' \
          || fail "the pinned crun was built without libkrun"

        # 3. the dry run: backend = "podman-krun" swaps --runtime to
        #    the pinned path, and carries the handler annotation —
        #    crun runs the libkrun VM handler ONLY for
        #    run.oci.handler=krun (custom-handler.c), without it the
        #    run would silently be a plain container, no VM.
        repo="$TMPDIR/repo"
        mkdir -p "$repo" "$TMPDIR/repo.mysbx"
        printf 'backend = "podman-krun"\n' > "$TMPDIR/repo.mysbx/config.toml"
        git -C "$repo" init -q
        git -C "$repo" config user.email t@invalid
        git -C "$repo" config user.name t
        touch "$repo/README"
        git -C "$repo" add . && git -C "$repo" commit -qm init
        export MYSBX_GVISOR_IMAGE=localhost/test:latest
        # The canonicalization needs a HOME that exists (the check
        # sandbox has none — runCommand's user is /homeless-shelter).
        mkdir -p "$TMPDIR/home"
        argv=$( cd "$repo" && HOME="$TMPDIR/home" "${pkgKrun}/bin/mysbx" --dry-run ) \
          || fail "the dry run failed"
        first=$(echo "$argv" | sed -n '2p')
        [ "$first" = "--runtime=$crun" ] \
          || fail "the dry run does not swap --runtime to the pinned crun (got: $first)"
        echo "$argv" | grep -qx 'run.oci.handler=krun' \
          || fail "the dry run does not carry the run.oci.handler=krun annotation (no VM without it)"
        echo "$argv" | grep -qx -- '--group-add=keep-groups' \
          || fail "the dry run does not preserve the supplementary groups (bd myconfig-b5o: the VMM loses the kvm group and every device-by-group run dies with EACCES)"
        echo "$argv" | grep -q '^--cap-drop=ALL$' \
          && fail "the krun argv advertises --cap-drop=ALL, which the krun handler never enforces (the payload is guest root)"
        # 4. the rest of the argv is the gvisor layout: the image
        #    reference and the image-userland payload survive.
        echo "$argv" | grep -q '^localhost/test:latest$' \
          || fail "the image reference is missing from the krun argv"
        test "$(echo "$argv" | tail -n 1)" = "/bin/bash" \
          || fail "the payload is not the image shell"

        touch $out
      '';

  # The herdr entry's worktree-placement contract (bd myconfig-i6h):
  # the entry must point herdr's `[worktrees] directory` at the
  # workmux `<repo>__worktrees` sibling — the same registry
  # `Repo::worktrees` binds — and must do so ONLY for an existing
  # (i.e. bound) directory, never create it. The check extracts the
  # entry's own placement block (`worktrees="$(dirname …` to its
  # `fi`), runs it against both a present and an absent sibling, and
  # validates the generated session config with herdr's own
  # `config check` — the parser that would silently fall back to its
  # default on a broken value otherwise. The entry's server pre-start
  # block (bd myconfig-bk2) is exercised the same way: extracted and
  # run against the REAL herdr binary, which must spawn its headless
  # server and make the socket API answer. Static guards pin the rest of
  # the contract: `bash -n` on the whole script, and no `mkdir` on
  # the sibling (config.md D13: a run never creates it).
  mysbx-herdr-entry-test =
    let
      lib = inputs.nixpkgs.lib;
      # The static session config the entry installs (the `sessionConfig`
      # of ./herdr-entry.nix), written ONCE here so both scenario runs
      # start from the identical base the entry produces.
      baseConfig = pkgs.writeText "mysbx-herdr-config.toml" ''
        # Generated by myconfig for the mysbx sandbox — do not edit.
        onboarding = false

        [terminal]
        new_cwd = "follow"
      '';
      # The entry as this build's wrapper pins it: same module wiring
      # as `muxEntries` in ../default.nix, never a hand-rolled
      # callPackage with diverging arguments.
      cfg = self.nixosConfigurations.test-f13.config.myconfig.ai.dev.mysbx;
      entry = pkgs.callPackage ./herdr-entry.nix {
        herdr = cfg.herdr.package;
      };
    in
    pkgs.runCommand "mysbx-herdr-entry-test"
      {
        inherit baseConfig;
        baseConfigPath = toString baseConfig;
        nativeBuildInputs = with pkgs; [
          bashInteractive
          gnugrep
          herdr
          procps
        ];
      }
      ''
        fail() {
          echo "mysbx-herdr-entry-test: $*" >&2
          exit 1
        }

        entry='${lib.getExe entry}'
        test -x "$entry" || fail "entry not executable"
        bash -n "$entry" || fail "bash -n rejects the entry"

        # Static contract guards: the sibling spelling is configured,
        # never created — mysbx binds it only when it exists, so the
        # entry must not `mkdir` it (config.md D13).
        grep -q '\[worktrees\]' "$entry" \
          || fail "the entry does not write a [worktrees] table"
        if grep -qE 'mkdir[^#]*worktrees' "$entry"; then
          fail "the entry must never mkdir the worktrees sibling (config.md D13)"
        fi

        # Extract the entry's own placement block — `worktrees="$(dirname
        # …` through its `fi` — so the check runs the very code the
        # sandbox runs, not a re-implementation of it.
        block="$(sed -n '/^worktrees="\$(dirname/,/^fi$/p' "$entry")" \
          || fail "cannot extract the placement block"
        printf '%s\n' "$block" | grep -q '^if \[ -d "\$worktrees" \]' \
          || fail "the extracted block is not gated on the sibling existing"

        tmp="$(mktemp -d)"

        # (a) a BOUND sibling (the argv builder mounts it rw exactly
        # when it exists on the host): the config gains the
        # repository-local root.
        mkdir -p "$tmp/repo" "$tmp/repo__worktrees" "$tmp/home/.config/herdr"
        cat "$baseConfigPath" > "$tmp/home/.config/herdr/config.toml"
        repo_root="$tmp/repo" HOME="$tmp/home" bash -c "$block" \
          || fail "the placement block failed on an existing sibling"
        grep -q '^directory = "'"$tmp"'/repo__worktrees"$' \
          "$tmp/home/.config/herdr/config.toml" \
          || fail "no repository-local [worktrees] directory for an existing sibling"
        HOME="$tmp/home" herdr config check \
          || fail "herdr config check rejects the generated config"

        # (b) an ABSENT sibling: no [worktrees] table may appear —
        # herdr keeps its own default, which lives in the sandbox's
        # tmpfs home and is therefore ephemeral.
        rm -rf "$tmp/repo__worktrees"
        cat "$baseConfigPath" > "$tmp/home/.config/herdr/config.toml"
        repo_root="$tmp/repo" HOME="$tmp/home" bash -c "$block" \
          || fail "the placement block failed on an absent sibling"
        if grep -q '^\[worktrees\]' "$tmp/home/.config/herdr/config.toml"; then
          fail "an absent sibling must leave [worktrees] unset"
        fi
        test ! -e "$tmp/repo__worktrees" \
          || fail "the block created the sibling"

        # The server pre-start contract (bd myconfig-bk2): the entry
        # must start herdr's headless server when its API socket is not
        # bound yet, and leave an already-running one alone. Extract
        # the entry's own pre-start block — the `-S` gate through its
        # `fi` — and run it against the REAL herdr binary with a
        # throwaway HOME, exactly like a fresh sandbox start: the socket
        # must appear and the API must answer; a second run must not
        # spawn a second server. The spawn must be gated on the socket
        # FILE (herdr 0.9.1's `status server` exits 0 even when nothing
        # runs, so it gates nothing), and readiness must be asked of
        # the API, not the filesystem.
        #
        # The PACING is part of the contract (bd myconfig-27o): under
        # nono's pathname AF_UNIX mediation the supervisor rate-limits
        # every mediated bind/connect with a token bucket (10/s refill,
        # burst 5) whose exhausted denials are EPERM and never reach the
        # IPC-denial footer, so the entry's wait must (1) wait for the
        # socket file with free stat() calls, (2) grant a fixed 1s
        # grace before the first API probe (bootstrap + bucket refill),
        # and (3) probe the API at a 1s cadence — a faster poll outruns
        # the refill and can never succeed. All three shapes are
        # asserted on the entry's own text before the live run replays
        # the very same wait.
        prestart="$(sed -n '/^if \[ ! -S "\$HOME\/\.config\/herdr\/herdr\.sock" \]; then/,/^fi$/p' "$entry")" \
          || fail "cannot extract the server pre-start block"
        printf '%s\n' "$prestart" | grep -q 'herdr server' \
          || fail "the pre-start block does not start the herdr server"
        # The entry's script text is dedented when writeShellApplication
        # renders it, so every extraction below matches the RENDERED
        # entry ($entry), top-level loops at column 0.
        waitblock="$(sed -n '/^for _ in \$(seq 1 50); do/,/^done$/p' "$entry" \
          | sed -n '1,/^done$/p')" \
          || fail "cannot extract the socket-file wait block"
        printf '%s\n' "$waitblock" | grep -Fq '[ -S "$HOME/.config/herdr/herdr.sock" ] && break' \
          || fail "the entry does not wait for the socket FILE with a stat"
        printf '%s\n' "$waitblock" | grep -q '^    sleep 0.1$' \
          || fail "the socket-file wait is not a 10Hz stat poll (free — stat costs no mediation token)"
        graceblock="$(sed -n '/^sleep 1$/,/^for _ in \$(seq 1 10); do$/p' "$entry" | head -n 2)" \
          || true
        printf '%s\n' "$graceblock" | grep -q '^sleep 1$' \
          || fail "the entry has no 1s grace between socket file and API probe"
        pollblock="$(sed -n '/^for _ in \$(seq 1 10); do/,/^done$/p' "$entry" \
          | sed -n '1,/^done$/p')" \
          || fail "cannot extract the readiness poll block"
        printf '%s\n' "$pollblock" | grep -q 'herdr workspace list' \
          || fail "the entry does not poll the API for server readiness"
        printf '%s\n' "$pollblock" | grep -q '^    sleep 1$' \
          || fail "the readiness poll is not paced at 1s (nono token bucket, bd myconfig-27o)"
        # No API-touching loop anywhere in the entry may run faster
        # than 1s: the relocation subshell is the only other consumer.
        if grep -q 'sleep 0.1' <(sed -n '/^(\$/,/^) >\/dev\/null 2>&1 &$/p' "$entry"); then
          fail "an un-paced 0.1s API poll survived in the relocation subshell"
        fi

        # Scratch HOME for the live part of the check — nothing under
        # the builder's real HOME is read or written.
        htmp="$(mktemp -d)"
        mkdir -p "$htmp/.config/herdr"
        # Replay the entry's OWN wait, not a re-implementation: the
        # stat wait, the 1s grace and the paced API poll, exactly as
        # the sandbox runs them.
        run_prestart() {
          HOME="$htmp" bash -c "$prestart
        $waitblock
        sleep 1
        $pollblock"
        }
        run_prestart \
          || fail "the pre-start block failed on a fresh HOME"
        test -S "$htmp/.config/herdr/herdr.sock" \
          || fail "no herdr.sock after the pre-start block"
        HOME="$htmp" herdr workspace list >/dev/null 2>&1 \
          || fail "the socket API does not answer after the pre-start block"
        # A second run must not spawn a second server: the `-S` gate
        # sees the socket the first run's server bound (on this — the
        # check's — host there is no nono mediation, so the pacing is
        # exercised as text only here; the live nono behavior is the
        # f13 acceptance).
        server_pids_before="$(pgrep -f 'herdr server' || true)"
        run_prestart \
          || fail "the pre-start block failed on an already-running server"
        server_pids_after="$(pgrep -f 'herdr server' || true)"
        [ "$server_pids_before" = "$server_pids_after" ] \
          || fail "the second pre-start run spawned a second server"
        HOME="$htmp" herdr server stop >/dev/null 2>&1 || true

        mkdir "$out"
      '';

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
        # `podman-load-image --force|--test|--image`) — extracted,
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

  # The `MYSBX_NIX_CONF` pin is a file named `nix.conf` inside a
  # directory: the wrapper pins `sandboxNixConfDir/nix.conf`, the
  # shape bwrap binds at /etc/nix/nix.conf. (The original reason was
  # the pure-nono backend's `NIX_CONF_DIR = <parent>` consumption, bd
  # myconfig-bf2 — that backend is gone, backends.md D1, but the pin
  # shape stays a wrapper invariant and the gates stay: a bare store
  # file would silently change what the sandbox reads.) Two gates: an
  # eval-time assertion on the path shape, and a build-time one where
  # the REAL pinned nix resolves its configuration from the REAL
  # pinned directory — `nix config show experimental-features` must
  # report `flakes`, which is only possible when the file was
  # actually loaded.
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
        # pinned directory — checked in an empty environment so no
        # host config can mask a miss
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

  # bd myconfig-6di.4.3: the gate the packaging build's own `profile
  # validate` cannot cover — the DELIVERED wrapper's profile pin (the
  # exact store path MYSBX_NONO_PROFILE carries on a host)
  # re-validated by the nono the wrapper pins. A drifted closure —
  # nono and profile from different generations — fails here, not at
  # a sandbox run. The expected CONTENTS are covered by the crate's
  # golden argv fixtures (the profile value flows into the argv as
  # `--profile <store path>`), with one exception asserted here:
  # the /tmp + $TMPDIR READ grant next to the write grant (bd
  # myconfig-2pe) — nono's `write` axis is write-ONLY, so without
  # `filesystem.read` the payload could never read back its own temp
  # files. `profile validate` accepts a readless profile silently,
  # so the regression gate lives here.
  mysbx-nono-profile-test =
    let
      pin = pkg.passthru.nonoProfilePin;
    in
    pkgs.runCommand "mysbx-nono-profile-test"
      {
        nativeBuildInputs = [
          pkgs.nono
          pkgs.jq
        ];
      }
      ''
        fail() {
          echo "mysbx-nono-profile-test: $*" >&2
          exit 1
        }

        test -s "${pin}" || fail "the profile pin is empty: ${pin}"

        # empty-environment run so no host config can mask a miss:
        # the same acceptance the packaging build ran, against the
        # exact file a host's MYSBX_NONO_PROFILE names.
        env -i PATH="$PATH" HOME="$TMPDIR" \
          ${pkgs.nono}/bin/nono profile validate "${pin}" \
          || fail "nono refused the pinned profile"

        # bd myconfig-2pe: the read grant is a behavior gate, not
        # schema — `profile validate` passes a write-only /tmp too.
        # nono 0.74.0's `AccessMode::Write` adds no Landlock
        # `ReadFile`, so the delivered profile must carry
        # `filesystem.read` mirroring `filesystem.write` path for
        # path, or every temp file the payload creates is one it can
        # never open again.
        jq -e '
          (.filesystem.read | sort) == (["/tmp", "$TMPDIR"] | sort)
          and (.filesystem.write | sort) == (["/tmp", "$TMPDIR"] | sort)
        ' "${pin}" >/dev/null \
          || fail "the pinned profile lost the /tmp + $TMPDIR read/write grants"

        mkdir "$out"
      '';

  # bd myconfig-pux: the nono backend's WRAPPER wiring, end to end.
  # The gate has two halves, and each proves what static greps on the
  # package file cannot:
  #
  # 1. The wrapper env of the FULL default package (nono = pkgs.nono,
  #    how every host builds it): MYSBX_NONO, MYSBX_NONO_PROFILE and
  #    MYSBX_ENV are present AND each value is an absolute store path
  #    that actually exists in THIS closure — a pin whose target fell
  #    out of the closure (a dropped nonoProfile input, a renamed
  #    attrset key) shows up here, not as a runtime PATH fallback.
  # 2. A REAL `--dry-run` of the wrapped binary under the generated
  #    nono config: a synthetic repo + a sidecar config with
  #    `backend = "nono"`, run with the wrapper's own pins — proves
  #    the nono selection builds the layered argv cleanly (no
  #    refusal) with everything the wrapper pins, i.e. the pin set is
  #    not just PRESENT but SUFFICIENT for the backend this host
  #    configures. What it does NOT prove: an actual sandboxed
  #    execution (Landlock inside the check sandbox is not
  #    possible) — that is bd myconfig-27o.
  mysbx-nono-wrapper-test =
    pkgs.runCommand "mysbx-nono-wrapper-test"
      {
        nativeBuildInputs = [
          pkgNono
          pkgs.git
        ];
      }
      ''
                fail() {
                  echo "mysbx-nono-wrapper-test: $*" >&2
                  exit 1
                }

                content=$(cat "${pkgNono}/bin/mysbx")
                for var in MYSBX_NONO MYSBX_NONO_PROFILE MYSBX_ENV; do
                  echo "$content" | grep -aq "$var" \
                    || fail "the wrapper does not pin $var"
                done
                # The profile pin is a store FILE (the generated nono profile
                # of bd myconfig-6di.4.3), not the operator-knob string.
                # The wrapper is a makeBinaryWrapper result: the shell line
                # `--set-default 'MYSBX_NONO_PROFILE' '<path>'` survives in the
                # embedded postBuild string. Pull the profile's store path out
                # of it — the VALUE the wrapper's stub registers.
                prof=$(echo "$content" | tr -d "'" | awk '/--set-default MYSBX_NONO_PROFILE/ {print $3; exit}')
                [ -n "$prof" ] || fail "no MYSBX_NONO_PROFILE value found"
                test -s "$prof" || fail "the profile pin target does not exist: $prof"
                grep -q '"name":"mysbx"' "$prof" \
                  || fail "the profile pin is not the generated mysbx profile"

                # The dry-run half: a synthetic repo with a sidecar naming
                # `backend = "nono"`; the wrapped binary runs dry inside the
                # check sandbox with its OWN pins (no env override).
                repo="$TMPDIR/repo"
                mkdir -p "$repo" "$TMPDIR/repo.mysbx"
                # The sidecar the run reads: `backend = "nono"` — the thin
                # shape the dry-run half of this check proves.
                cat > "$TMPDIR/repo.mysbx/config.toml" <<'CLOSURECONFIG'
        backend = "nono"
        network = false
        CLOSURECONFIG
                git -C "$repo" init -q
                git -C "$repo" config user.email t@invalid
                git -C "$repo" config user.name t
                touch "$repo/README"
                git -C "$repo" add README
                git -C "$repo" commit -qm init

                cd "$repo"
                # The canonicalization needs a HOME that exists (the check
                # sandbox has none — runCommand's user is /homeless-shelter).
                mkdir -p "$TMPDIR/home"
                argv=$(HOME="$TMPDIR/home" "${pkgNono}/bin/mysbx" --dry-run 2>"$TMPDIR/stderr") \
                  || fail "the wrapped dry-run refused: $(cat "$TMPDIR/stderr")"
                echo "$argv" | head -1 | grep -q bwrap \
                  || fail "the argv[0] is not bwrap: $argv"
                echo "$argv" | grep -q '^/nix/store/[a-z0-9]*-nono-[^/]*/bin/nono$' \
                  || fail "the nono binary is not the inner wrapper: $argv"
                echo "$argv" | grep -q "^--profile$" \
                  || fail "no profile arg in the chain: $argv"
                # The profile arg carries the pinned store file, exactly the
                # value the wrapper set.
                echo "$argv" | grep -q "^$prof$" \
                  || fail "the argv does not carry the pinned profile"
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
    # `openssh` is the `ssh-keygen` of the now-UNCONDITIONAL sandbox
    # keypair (docs/design/config.md D22): every live run generates
    # one host-side, and the unwrapped crate's PATH fallback must find
    # it — the same way the wrapper's pin does on a host.
    nativeCheckInputs = (old.nativeCheckInputs or [ ]) ++ [
      pkgs.git
      pkgs.openssh
    ];
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
