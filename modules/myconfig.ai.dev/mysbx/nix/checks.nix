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
#   mysbx-disabled-backends-wrapper-test
#                 the backend opt-in gate's wrapper half (bd
#                 myconfig-vc8k/jlam): a flags-off host's wrapper pins
#                 MYSBX_ENABLED_BACKENDS 'bubblewrap nono' and carries
#                 NONE of the three heavy backends' closure pins — the
#                 closure goal of the opt-in. The CLI refusal is the
#                 cargo suite's job (bd myconfig-a551's gate tests).
#
#   mysbx-microvm-qemu-wrapper-test
#                 the native-qemu wrapper pins (backends.md D9, bd
#                 myconfig-6x9c): the wrapper carries the backend's three
#                 closure pins (launcher, rootfs, kernel) and a
#                 `backend = "microvm-qemu"` dry run names --kernel,
#                 --exit-channel, --rootfs, --init and the manifest
#                 pointer — and carries NO --krun-overlay flag (bd
#                 myconfig-govd: the launcher refuses it). Stubs for the
#                 pins — the real kernel stays a host build.
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
#                 completed.
#
#   mysbx-hermes-test
#                 the hermes tier (../../hermes-agent/mysbx.nix) on the
#                 reference host that enables both halves (`test-p14`):
#                 the launcher on the sandbox PATH AND in the podman image,
#                 `.hermes` as a `state-dirs` entry, `CUSTOM_BASE_URL` for
#                 the container backends, and the seeder's behaviour — a
#                 fresh home gets the LiteLLM route, an existing config is
#                 never rewritten.
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
#   mysbx-reattach-loop-test
#                 the detach.md B7 reattach loop (bd
#                 myconfig-7ioq.6): with `MYSBX_DETACHABLE=1` the
#                 multiplexer entries loop instead of exec'ing their
#                 client, so an inner detach prints the prompt and
#                 Enter reattaches, q ends the run, and a dead server
#                 ends it too — within ~1s of the server dying, even
#                 while the prompt is shown (the wait polls with
#                 `read -t 1`, bd myconfig-7ioq.15). Replayed against
#                 a stateful tmux STUB (a real `attach-session` needs
#                 a pty the build sandbox lacks); aoe's every-socket
#                 probe is replayed too (a named-socket server), herdr
#                 is covered by the same fragment, asserted statically
#                 — the live reattach of aoe/herdr is the bead's
#                 manual acceptance.
#
# Wired into `nix flake check` for `x86_64-linux` in `flake.nix`.
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
  lib = inputs.nixpkgs.lib;

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
  # Known and accepted: CI
  # tests this crate from the locked `inputs.nixpkgs`, which can differ
  # slightly from the host-eval nixpkgs the wrapped binary on a host
  # was built with. The crate is dependency-free, so the drift surface
  # is the toolchain, not the library set.
in
{
  # The crate itself, with `doCheck = true`: `cargo test` in the build
  # sandbox.
  # The generator, evaluated: what a host actually gets in
  # `~/.config/mysbx/config.toml` (review-4 item 4).
  mysbx-generated-config-test = import ./config-eval-test.nix { inherit inputs system; };

  # The launcher's patched library runs real passthrough/server tests without KVM.
  mysbx-libkrun-readdirplus-test = pkgs.callPackage ./libkrun.nix { };

  # The rootfs builder exercises its generated init with the baked static tools:
  # copied-db safety and validation before the fresh store overlay is mounted.
  mysbx-krun-rootfs-test =
    self.nixosConfigurations.test-f13.config.myconfig.ai.dev.mysbx.krun.direct.rootfs;

  # The guest-own store disk (docs/design/guest-store.md): the
  # erofs image probed with the tool that reads it in anger. Built
  # from a SMALL fixture closure, not the full wrapper's tool tree -
  # the image's contract is closure-independent (mkfs.erofs packs
  # whatever rootPaths name) and the full bake costs CI the whole
  # tool closure. fsck.erofs walks the tree the guest kernel walks
  # over the virtio-blk device, --extract lays it out, and the
  # superblock carries the FIXED UUID the determinism block pins
  # (dump.erofs -s prints it verbatim). The store/, var/nix/db and
  # gcroots members follow the dockerTools recipe the guest nix
  # reads. The WRAPPER side pins the pin itself: the --set spelling
  # (a string literal, reflow-proof) plus the marker agreement across
  # the crate, the wrapper and the init; the module WIRING is the
  # eval-time block below; the negative (a flags-off host pins
  # nothing) is the disabled-backends check's list.
  mysbx-krun-store-disk-test =
    let
      storeImage = pkgs.callPackage ../nix/krun-store-image.nix { };
      img = storeImage { rootPaths = [ pkgs.hello ]; };
      # The module wiring, pinned at EVAL time: a host with
      # krun.store.enable must bake and pin the disk, the guest nix
      # wrappers must ride along while krun.nix.enable is on, and
      # must drop out when it is off. test-f13 enables krun.nix by
      # default, so the nix arm compares against an extension that
      # FORCES it off — the plain host cannot discriminate it. Forcing
      # the package passthrus evaluates the wrapper, nothing builds
      # here (the image build is the fixture arm above).
      storeHost = self.nixosConfigurations.test-f13.extendModules {
        modules = [ { myconfig.ai.dev.mysbx.krun.store.enable = true; } ];
      };
      storeNoNixHost = self.nixosConfigurations.test-f13.extendModules {
        modules = [
          {
            myconfig.ai.dev.mysbx.krun.store.enable = true;
            myconfig.ai.dev.mysbx.krun.nix.enable = lib.mkForce false;
          }
        ];
      };
      plainHost = self.nixosConfigurations.test-f13;
      pkgOf = host: host.config.myconfig.ai.dev.mysbx.package;
    in
    pkgs.runCommand "mysbx-krun-store-disk-test"
      {
        nativeBuildInputs = [
          pkgs.erofs-utils
          pkgs.sqlite
        ];
        # The eval results, injected as plain strings the shell
        # asserts on (a bool and two path lists).
        storeWiredOn = if (pkgOf storeHost).passthru.krunStoreBaked then "1" else "0";
        storeWiredOff = if (pkgOf plainHost).passthru.krunStoreBaked then "1" else "0";
        storeRootsOn = lib.concatStringsSep " " (pkgOf storeHost).passthru.krunStoreRoots;
        storeRootsNoNix = lib.concatStringsSep " " (pkgOf storeNoNixHost).passthru.krunStoreRoots;
      }
      ''
        fail() {
          echo "mysbx-krun-store-disk-test: $*" >&2
          exit 1
        }

        # 1. integrity: fsck.erofs checks every file is well
        #    encoded (lz4, xattrs, the --tar layout) — the same
        #    reader the guest kernel runs over the ro blk device.
        fsck.erofs -d2 "${img}" >/dev/null \
          || fail "fsck.erofs rejects the store disk image"

        # 2. the fixed superblock metadata (krun-store-image.nix's
        #    determinism block): the UUID the launcher and init can
        #    pin. dump.erofs -s prints it verbatim; the label is not
        #    in its output (erofs-utils 1.9.4), so the UUID is the
        #    assertable half of the pair.
        dump.erofs -s "${img}" | grep -q "5f6d7962-7374-6f72-6564-69736b2d0001" \
          || fail "the superblock does not carry the fixed UUID"

        # 3. the layout: store/<name> for the fixture root (the
        #    --transform rewrite), the REGISTERED db and the
        #    gcroots of the dockerTools recipe.
        extract=$(mktemp -d)
        fsck.erofs --extract="$extract" "${img}" >/dev/null \
          || fail "fsck.erofs cannot extract the store disk image"
        ls "$extract"/store/ | grep -q -- "-hello-" \
          || fail "the store/ directory does not carry the fixture root"
        test -f "$extract/var/nix/db/db.sqlite" \
          || fail "the var/nix/db/db.sqlite registration is missing"
        rows=$(${lib.getExe pkgs.sqlite} "$extract/var/nix/db/db.sqlite" \
          'SELECT count(*) FROM ValidPaths')
        test "$rows" -gt 0 \
          || fail "the registered db is empty"
        test -n "$(find "$extract/var/nix/gcroots/mysbx" -type l)" \
          || fail "the gcroots do not pin the fixture closure"
        # No rm of the extract tree: the store/ members keep their
        # read-only modes (dr-xr-xr-x) and the builder sandbox sweeps
        # the tempdir anyway.

        # 4. the module wiring (the env vars above). On: the wrapper
        #    bakes and pins the disk, and the krun.nix guest wrappers
        #    ride along as extra roots (their store paths a baked run
        #    has no host-store share to resolve through). Off: nothing.
        [ "$storeWiredOn" = 1 ] \
          || fail "krun.store.enable does not bake the disk (the default.nix krunStoreBaked wiring is broken)"
        [ "$storeWiredOff" = 0 ] \
          || fail "the wrapper bakes the disk without krun.store.enable (a stock host must keep the host-store share)"
        [ "$storeRootsOn" != "$storeRootsNoNix" ] \
          || fail "krun.nix.enable did not add the guest nix wrappers to the disk roots (the krunStoreExtraRoots wiring is broken)"
        case "$storeRootsOn" in
          *krun-guest-nix*) ;;
          *) fail "the disk roots do not carry the guest nix wrappers: $storeRootsOn" ;;
        esac
        case "$storeRootsNoNix" in
          *krun-guest-nix*) fail "the disk roots carry the guest nix wrappers with krun.nix.enable off: $storeRootsNoNix" ;;
        esac
        # The --set SPELLING the crate reads is a string literal in
        # mysbx.nix (a reformat never splits literals, so this grep
        # is reflow-proof).
        grep -q -- '--set MYSBX_KRUN_STORE_DISK' ${../nix/mysbx.nix} \
          || fail "the wrapper lost the MYSBX_KRUN_STORE_DISK pin"
        # The marker AGREEMENT: the crate's record, the wrapper's
        # --set and the init's gate must spell the same name, or a
        # baked run announces a disk the guest never looks for. The
        # crate anchor is the record VALUE itself — a string literal
        # rustfmt never splits, wherever the declaration reflows to.
        grep -qF '"MYSBX_KRUN_STORE_DISK=1"' \
          ${../mysbx-rs/src/lib.rs} \
          || fail "the crate's store-disk env record changed"
        grep -qF 'MYSBX_KRUN_STORE_DISK' ${../nix/krun-rootfs.nix} \
          || fail "the guest init lost the MYSBX_KRUN_STORE_DISK gate"

        touch $out
      '';

  mysbx-krun-fd-monitor-test = pkgs.runCommand "mysbx-krun-fd-monitor-test" { } ''
    ${pkgs.python3}/bin/python3 ${./krun-fd-stress.py} --self-test
    touch $out
  '';

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
        storeScan = pkgs.callPackage ../nix/krun-store-scan.nix { };
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
      grep -q 'mount -t tmpfs' "$setup" || fail "the setup must keep the tmpfs fallback"
      # The disk-backed scratch of bd myconfig-0pi: the setup
      # loop-mounts MYSBX_KRUN_SCRATCH_IMG when the run provides it,
      # mkfs.ext4's it (e2fsprogs in the image via
      # krun.nix.packages), tries to remove the path right after the
      # attach (best-effort, the open loop device keeps the inode
      # alive), and ANNOUNCES
      # the tmpfs fallback — never a silent switch.
      grep -qF 'MYSBX_KRUN_SCRATCH_IMG' "$setup" \
        || fail "the setup does not read MYSBX_KRUN_SCRATCH_IMG (bd myconfig-0pi)"
      grep -qF 'losetup --find --show' "$setup" \
        || fail "the scratch path does not attach a loop device"
      grep -qF 'mkfs.ext4 -q -F' "$setup" \
        || fail "the scratch path does not mkfs.ext4 the loop device (e2fsprogs)"
      grep -qF 'mount -t ext4' "$setup" \
        || fail "the scratch path does not mount the ext4 at the scratch"
      grep -qF 'rm -f "$img"' "$setup" \
        || fail "the setup must try to remove the image path right after the loop attach"
      grep -qF 'no scratch disk provided' "$setup" \
        || fail "the tmpfs fallback is not announced (bd myconfig-0pi: refuse or announce, never silently switch)"
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
      # The db reconcile (bd myconfig-mxu): the setup must prune
      # copied-db rows whose store path the image store cannot
      # keep (missing or a symlink — a dangling poisoned link prunes
      # too, [ -e ] is false for it), BEFORE the overlay is mounted
      # and any payload nix runs.
      grep -qF 'SELECT path FROM ValidPaths WHERE path LIKE' "$setup" \
        || fail "the setup does not enumerate the copied db's store paths (the reconcile's SELECT)"
      grep -qF 'DELETE FROM ValidPaths WHERE path =' "$setup" \
        || fail "the setup does not drop the doomed rows (the reconcile's DELETE)"
      grep -qF 'PRAGMA foreign_keys=ON' "$setup" \
        || fail "the reconcile does not enable SQLite foreign keys before its transaction"
      grep -qF 'DELETE FROM Refs WHERE reference =' "$setup" \
        || fail "the reconcile does not remove references from surviving referrers"
      grep -qF 'PRAGMA foreign_key_check;' "$setup" \
        || fail "the reconcile does not check foreign-key integrity"
      grep -qF 'foreign-key violations remain' "$setup" \
        || fail "the reconcile does not fail on foreign-key violations"
      grep -qF -- '-L "$p"' "$setup" \
        || fail "the reconcile must test for symlinked store paths, not only missing ones"
      grep -qF 'reconciling the db' "$setup" \
        || fail "the reconcile must announce how many rows it drops (refuse or announce)"
      # The bulk scan + join of bd myconfig-kid: the per-path stat
      # loop must be gone from the MAIN path (it survives only as
      # the DT_UNKNOWN/deep-path fallback), and the scanner must
      # be part of the setup's tools.
      grep -qF 'store-scan /nix/store' "$setup" \
        || fail "the reconcile must bulk-scan the share instead of stat-ing every registered path (bd myconfig-kid)"
      grep -qF 'entry[$2] = $1' "$setup" \
        || fail "the reconcile must join the registered paths against the store scan in bulk"
      # ...and in order: after copyState, before the overlay mount.
      copy_line=$(grep -n 'mysbx-krun-nix-copy-state /nix/var/nix' "$setup" | head -1 | cut -d: -f1)
      scan_line=$(grep -n 'store-scan /nix/store' "$setup" | head -1 | cut -d: -f1)
      overlay_line=$(grep -n 'mount -t overlay' "$setup" | head -1 | cut -d: -f1)
      [ -n "$copy_line" ] && [ -n "$scan_line" ] && [ -n "$overlay_line" ] \
        && [ "$copy_line" -lt "$scan_line" ] && [ "$scan_line" -lt "$overlay_line" ] \
        || fail "the reconcile must run after copyState and before the overlay mount"
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
        # The repo must sit below a share root the krun guest can PLACE a
        # share under (mysbx-rs/src/krun.rs::check_share_root): the baked
        # roots plus /tmp and /mysbx-home. $TMPDIR is NOT usable here —
        # runCommand sets it to /build, which is neither, and the dry run
        # then fails with UnknownShareRoot for a reason that has nothing
        # to do with what this check asserts. /tmp is the one placeable
        # root the build sandbox lets us create (measured: the baked roots
        # are all mkdir-denied there).
        if ! mkdir -p /tmp/mysbx-krun-wrapper-test-$$ 2>/dev/null; then
          fail "cannot create a fixture root under /tmp (the one placeable share root)"
        fi
        work=/tmp/mysbx-krun-wrapper-test-$$
        trap 'rm -rf "$work"' EXIT
        repo="$work/repo"
        mkdir -p "$repo" "$work/repo.mysbx"
        printf 'backend = "podman-krun"\n' > "$work/repo.mysbx/config.toml"
        git -C "$repo" init -q
        git -C "$repo" config user.email t@invalid
        git -C "$repo" config user.name t
        touch "$repo/README"
        git -C "$repo" add . && git -C "$repo" commit -qm init
        export MYSBX_PODMAN_IMAGE=localhost/test:latest
        # The canonicalization needs a HOME that exists (the check
        # sandbox has none — runCommand's user is /homeless-shelter).
        mkdir -p "$work/home"
        argv=$( cd "$repo" && HOME="$work/home" "${pkgKrun}/bin/mysbx" --dry-run ) \
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

  mysbx-microvm-qemu-wrapper-test =
    let
      # Stubs of the three pins, in the REAL shapes the wrapper
      # --sets: a launcher package with bin/, a rootfs package with
      # bin/, and a kernel PACKAGE whose bzImage member is the pin
      # (the module wiring passes "${pkg}/bzImage", never the
      # package — bd myconfig-q3se). Stubs keep this check from
      # building the whole pinned kernel in CI; the wiring itself
      # is pinned by the source grep below and the run-path
      # is-file refusal (mysbx-rs) on a host that opts into the
      # backend.
      stubLauncher = pkgs.runCommand "mysbx-microvm-qemu-stub-launcher" { } ''
        mkdir -p $out/bin
        printf '#!/bin/sh\necho "mysbx-microvm-qemu: unknown argument $1" >&2\nexit 2\n' > $out/bin/mysbx-microvm-qemu
        chmod +x $out/bin/mysbx-microvm-qemu
      '';
      stubRootfs = pkgs.runCommand "mysbx-microvm-qemu-stub-rootfs" { } ''
        mkdir -p $out/bin
        printf '#!/bin/sh\n' > $out/bin/mysbx-init
        chmod +x $out/bin/mysbx-init
      '';
      stubKernelPkg = pkgs.runCommand "mysbx-microvm-qemu-stub-kernel" { } ''
        mkdir $out
        printf 'stub-bzImage' > $out/bzImage
      '';
      stubKernel = "${stubKernelPkg}/bzImage";
      pkgQemu = pkgs.callPackage ../nix/mysbx.nix {
        microvmQemuLauncher = stubLauncher;
        microvmQemuRootfs = stubRootfs;
        microvmQemuKernel = stubKernel;
        microvmQemuEnv = {
          OPENAI_BASE_URL = "http://10.0.2.2:4000/v1";
          MYCONFIG_LITELLM_BASE_URL = "http://10.0.2.2:4000/v1";
        };
        ssh-keygen = pkgs.openssh;
      };
    in
    pkgs.runCommand "mysbx-microvm-qemu-wrapper-test"
      {
        nativeBuildInputs = [
          pkgs.git
        ];
      }
      ''
        fail() {
          echo "mysbx-microvm-qemu-wrapper-test: $*" >&2
          exit 1
        }

        content=$(cat "${pkgQemu}/bin/mysbx")
        # 1. the wrapper pins all three, as --set values pointing at
        #    the stubs. The makeBinaryWrapper docstring quotes BOTH the
        #    variable and the value (`--set 'VAR' 'VALUE'`), so match the
        #    variable NAME alone and strip the quotes before awk — the
        #    krun check's idiom. After `tr -d "'"` the value is awk
        #    field 3 (field 4 is the line-continuation backslash).
        for pin in MYSBX_MICROVM_QEMU_LAUNCHER MYSBX_MICROVM_QEMU_ROOTFS MYSBX_MICROVM_QEMU_KERNEL; do
          echo "$content" | grep -q -- "$pin" \
            || fail "the wrapper does not pin $pin"
        done
        launcher=$(echo "$content" | tr -d "'" | awk '/--set MYSBX_MICROVM_QEMU_LAUNCHER/ {print $3; exit}')
        case "$launcher" in
          /nix/store/*-mysbx-microvm-qemu-stub-launcher/bin/mysbx-microvm-qemu) ;;
          *) fail "the pinned launcher is not the stub: $launcher" ;;
        esac
        test -x "$launcher" || fail "the pinned launcher does not exist: $launcher"
        # The kernel pin names the bzImage FILE (a store file path),
        # never the package directory (bd myconfig-q3se).
        kernel=$(echo "$content" | tr -d "'" | awk '/--set MYSBX_MICROVM_QEMU_KERNEL/ {print $3; exit}')
        case "$kernel" in
          /nix/store/*-mysbx-microvm-qemu-stub-kernel/bzImage) ;;
          *) fail "the pinned kernel is not the stub bzImage: $kernel" ;;
        esac
        test -f "$kernel" || fail "the pinned kernel is not a file: $kernel"
        # And the module wiring passes the member: the line in
        # default.nix that feeds the wrapper's kernel pin must name
        # the bzImage INSIDE the package, never the package itself.
        wiring=$(grep -n 'microvmQemuKernel = ' ${../default.nix} \
          || fail "the module wiring (default.nix microvmQemuKernel) is gone")
        echo "$wiring" | grep -q '/bzImage"' \
          || fail "the module wiring does not pin the bzImage member: $wiring"

        # 2. the env pins (bd myconfig-mzys): the wrapper pins them as
        #    a --set-default SPACE-SEPARATED list, and the dry run
        #    carries each entry as a manifest --env AFTER the config
        #    layers (a pin wins over a configured value in the guest,
        #    where the manifest env records are exported in order).
        #    Strip the wrapper's quotes BEFORE matching (the krun
        #    check's idiom), then the value is awk field 3.
        content=$(echo "$content" | tr -d "'")
        echo "$content" | grep -aq -- '--set-default MYSBX_MICROVM_QEMU_ENV' \
          || fail "the wrapper does not pin MYSBX_MICROVM_QEMU_ENV"
        qemu_env=$(echo "$content" | awk '/--set-default MYSBX_MICROVM_QEMU_ENV/ {$1=""; $2=""; print; exit}' | sed 's/^  //')
        case "$qemu_env" in
          "MYCONFIG_LITELLM_BASE_URL=http://10.0.2.2:4000/v1 OPENAI_BASE_URL=http://10.0.2.2:4000/v1") ;;
          *) fail "the pinned MYSBX_MICROVM_QEMU_ENV is not the expected list: $qemu_env" ;;
        esac

        # 3. the dry run: a repo below a placeable share root, the
        #    backend in its sidecar, the payload a one-shot true.
        if ! mkdir -p /tmp/mysbx-microvm-qemu-wrapper-test-$$ 2>/dev/null; then
          fail "cannot create a fixture root under /tmp (the one placeable share root)"
        fi
        work=/tmp/mysbx-microvm-qemu-wrapper-test-$$
        trap 'rm -rf "$work"' EXIT
        repo="$work/repo"
        mkdir -p "$repo" "$work/repo.mysbx"
        printf 'backend = "microvm-qemu"\n' > "$work/repo.mysbx/config.toml"
        git -C "$repo" init -q
        git -C "$repo" config user.email t@invalid
        git -C "$repo" config user.name t
        touch "$repo/README"
        git -C "$repo" add . && git -C "$repo" commit -qm init
        mkdir -p "$work/home"
        argv=$( cd "$repo" && HOME="$work/home" "${pkgQemu}/bin/mysbx" run --dry-run -- true ) \
          || fail "the wrapped dry run refused: run it verbosely for the diagnosis"
        # The qemu-only pins: kernel + exit-channel after the rootfs.
        echo "$argv" | grep -qx -- '--kernel' \
          || fail "the dry run does not name --kernel"
        echo "$argv" | grep -qx -- '--exit-channel' \
          || fail "the dry run does not name --exit-channel (the exit-code channel is the default contract)"
        echo "$argv" | grep -qx -- '--rootfs' \
          || fail "the dry run does not name --rootfs"
        echo "$argv" | grep -qx -- '--init' \
          || fail "the dry run does not name --init"
        echo "$argv" | grep -qx -- '/bin/mysbx-init' \
          || fail "the dry run does not pin the guest init"
        # NO overlay flag — the launcher refuses it (bd myconfig-govd).
        if echo "$argv" | grep -q -- '--krun-overlay'; then
          fail "the dry run carries a --krun-overlay flag — the launcher refuses it, every real run would exit 125"
        fi
        # The manifest pointer rides (the argv records' channel).
        echo "$argv" | grep -q -- '--manifest' \
          || fail "the dry run does not carry the manifest pointer"
        # The backend env pins ride as --env records after the config
        # layers (bd myconfig-mzys).
        echo "$argv" | grep -qx -- '--env' \
          || fail "the dry run carries no --env records"
        echo "$argv" | grep -qx -- 'OPENAI_BASE_URL=http://10.0.2.2:4000/v1' \
          || fail "the dry run does not carry the pinned OPENAI_BASE_URL"
        echo "$argv" | grep -qx -- 'MYCONFIG_LITELLM_BASE_URL=http://10.0.2.2:4000/v1' \
          || fail "the dry run does not carry the pinned MYCONFIG_LITELLM_BASE_URL"
        # The pins must precede the SLiRP NIC marker (the same
        # placement rule as the podman backend's pins).
        if grep -n -q -- 'MYSBX_MICROVM_NET=1' <<<"$argv"; then
          pin_line=$(grep -n -- 'MYCONFIG_LITELLM_BASE_URL=' <<<"$argv" | head -1 | cut -d: -f1)
          net_line=$(grep -n -- 'MYSBX_MICROVM_NET=1' <<<"$argv" | head -1 | cut -d: -f1)
          [ "$pin_line" -lt "$net_line" ] \
            || fail "the env pins must precede the SLiRP NIC marker"
        fi

        touch $out
      '';

  # The backend opt-in gate (bd myconfig-dq2w, wrapper half bd
  # myconfig-vc8k): a flags-off host's wrapper carries the availability
  # pin (MYSBX_ENABLED_BACKENDS 'bubblewrap nono') and NONE of the
  # three heavy backends' closure pins — the closure goal of the
  # opt-in. The CLI refusal itself is the cargo suite's job (the gate
  # tests of bd myconfig-a551); this is the WRAPPER side: what a host
  # that never opts in ships.
  mysbx-disabled-backends-wrapper-test =
    let
      # The flags-off wrapper: exactly what default.nix builds for a
      # host with podman.enable/krun.enable/microvm-qemu.enable all
      # off — every backend argument null, enabledBackends the
      # always-on pair.
      pkgDisabled = pkgs.callPackage ../nix/mysbx.nix {
        enabledBackends = [
          "bubblewrap"
          "nono"
        ];
        podmanImage = null;
        podmanShell = null;
        podmanPastaSpec = null;
        podmanEnv = { };
        krunRuntime = null;
        krunScratchSize = null;
        krunLauncher = null;
        krunRootfs = null;
        microvmQemuLauncher = null;
        microvmQemuRootfs = null;
        microvmQemuKernel = null;
        microvmQemuEnv = { };
        podmanWaypipe = null;
        podmanMuxEntries = { };
        nono = pkgs.nono;
        ssh-keygen = pkgs.openssh;
      };
      content = pkgs.runCommand "mysbx-disabled-backends-content" { } ''
        cat "${pkgDisabled}/bin/mysbx" > $out
      '';
    in
    pkgs.runCommand "mysbx-disabled-backends-wrapper-test"
      {
        nativeBuildInputs = [ pkgs.gnugrep ];
      }
      ''
        fail() {
          echo "mysbx-disabled-backends-wrapper-test: $*" >&2
          exit 1
        }

        wrapped=$(cat ${content})

        # 1. the availability pin: exactly the always-on pair. The
        #    makeBinaryWrapper result embeds the assignments plainly
        #    (`MYSBX_ENABLED_BACKENDS=bubblewrap nono`) — match without
        #    the --set quoting, the krun check's `grep -aq` idiom.
        echo "$wrapped" | grep -aq "MYSBX_ENABLED_BACKENDS=bubblewrap nono" \
          || fail "the flags-off wrapper does not pin MYSBX_ENABLED_BACKENDS=bubblewrap nono"

        # 2. no closure pin of the three opt-in backends survives: the
        #    wrapper must be unable to run them even before the CLI
        #    gate refuses (belt and suspenders — and the closure goal:
        #    nothing of gvisor/podman/libkrun/qemu enters the build).
        #    The krun wrapper check needs the `--set-default`/`--set`
        #    spelling disambiguated; here the NAMES are enough — a
        #    disabled backend pins nothing under any spelling.
        for pin in \
          MYSBX_PODMAN_TARBALL MYSBX_PODMAN_IMAGE MYSBX_PODMAN_IMAGE_ID \
          MYSBX_PODMAN_SHELL MYSBX_PODMAN_PASTA_SPEC MYSBX_PODMAN_ENV \
          MYSBX_PODMAN_WAYPIPE MYSBX_KRUN_RUNTIME MYSBX_KRUN_SCRATCH_SIZE \
          MYSBX_KRUN_LAUNCHER MYSBX_KRUN_ROOTFS MYSBX_KRUN_STORE_DISK \
          MYSBX_MICROVM_QEMU_LAUNCHER MYSBX_MICROVM_QEMU_ROOTFS \
          MYSBX_MICROVM_QEMU_KERNEL MYSBX_MICROVM_QEMU_ENV; do
          echo "$wrapped" | grep -aq -- "$pin" \
            && fail "a disabled backend's pin survived: $pin"
        done

        # 3. the always-on backends keep their pins (the opt-out must
        #    not have broken the base): bwrap, the tools tree and the
        #    nono pin (the module default passes `nono =
        #    cfg.nono.package`, i.e. pkgs.nono).
        for pin in MYSBX_BWRAP MYSBX_TOOLS_PATH MYSBX_NONO; do
          echo "$wrapped" | grep -q -- "$pin" \
            || fail "an always-on pin is missing: $pin"
        done

        touch $out
      '';

  # The herdr entry's worktree-placement contract (bd myconfig-i6h):
  # the entry must point herdr's `[worktrees] directory` at the
  # workmux `<repo>__worktrees` sibling — the same registry
  # `Repo::worktrees` binds — and must do so ONLY for an existing
  # (i.e. bound) directory, never create it (mysbx itself created it
  # before the argv, config.md D13; the entry only configures). The check extracts the
  # entry's own placement code (the shared sibling discovery and the
  # `[worktrees]` block), runs it against both a present and an absent sibling, and
  # validates the generated session config with herdr's own
  # `config check` — the parser that would silently fall back to its
  # default on a broken value otherwise. The entry's server pre-start
  # block (bd myconfig-bk2) is exercised the same way: extracted and
  # run against the REAL herdr binary, which must spawn its headless
  # server and make the socket API answer. Static guards pin the rest of
  # the contract: `bash -n` on the whole script, and no `mkdir` on
  # the sibling (the entry configures, mysbx created it: config.md D13).
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

        # Extract the entry's own placement code — the shared sibling
        # discovery (`repo_root="$PWD"` through its `fi`) plus the
        # `[worktrees]` block (`if [ -d "$worktrees" ]` through its
        # `fi`) — so the check runs the very code the sandbox runs, not
        # a re-implementation of it.
        block="$(sed -n '/^repo_root="\$PWD"$/,/^fi$/p' "$entry")
        $(sed -n '/^if \[ -d "\$worktrees" \]; then$/,/^fi$/p' "$entry")" \
          || fail "cannot extract the placement block"
        printf '%s\n' "$block" | grep -q '^if \[ -d "\$worktrees" \]' \
          || fail "the extracted block is not gated on the sibling existing"

        tmp="$(mktemp -d)"

        # (a) a BOUND sibling (the argv builder mounts it rw exactly
        # when it exists on the host): the config gains the
        # repository-local root.
        mkdir -p "$tmp/repo" "$tmp/repo__worktrees" "$tmp/home/.config/herdr"
        cat "$baseConfigPath" > "$tmp/home/.config/herdr/config.toml"
        (cd "$tmp/repo" && HOME="$tmp/home" bash -c "$block") \
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
        (cd "$tmp/repo" && HOME="$tmp/home" bash -c "$block") \
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

  # The detach.md B7 reattach loop (bd myconfig-7ioq.6): with
  # `MYSBX_DETACHABLE=1` the multiplexer entries loop instead of
  # exec'ing their client, so a detach of the INNER multiplexer does
  # not end the sandbox. The loop is one shared fragment
  # (./mux-entry-lib.nix `detachableLoop`), so the check extracts the
  # gate block from the tmux and workmux entries and replays it
  # against a stateful tmux STUB — a real `attach-session` needs a
  # pty the build sandbox lacks. The stub kills the alive marker on
  # the SECOND attach, which replays the last-pane-exit scenario
  # (server has no session → the run ends), and the kill runner
  # (bd myconfig-7ioq.15) kills the marker WHILE the gate waits at
  # the prompt with a silent stdin — the wait must poll (`read -t`)
  # and re-probe, or the run hangs and the timeout bound fails it.
  # aoe's every-socket probe (bd myconfig-7ioq.15) is replayed by its
  # own runner against a named-socket server; herdr uses the same
  # fragment with its own attach/probe and is asserted statically
  # because the live reattach of aoe/herdr is the bead's MANUAL
  # acceptance, not a build-time one.
  mysbx-reattach-loop-test =
    let
      lib = inputs.nixpkgs.lib;
      cfg = self.nixosConfigurations.test-f13.config.myconfig.ai.dev.mysbx;
      tmuxEntry = pkgs.callPackage ./tmux-entry.nix {
        tmux = cfg.tmux.package;
      };
      # A stub workmux (test-f13 carries none — and the gate must
      # not care: the loop is the same text either way).
      workmuxStub = pkgs.writeShellScriptBin "workmux" ''
        exit 0
      '';
      workmuxEntry = pkgs.callPackage ./workmux-entry.nix {
        workmux = workmuxStub;
        tmux = cfg.tmux.package;
      };
      # The aoe stub is stateful like the tmux stub below: an attach
      # run counts, creates the alive marker on the first run and
      # removes it on the second (the (g) replay feeds `q` before a
      # second run, so the marker stays for the probe).
      aoeStub = pkgs.writeShellScriptBin "aoe" ''
        state="''${TMPDIR:-/tmp}/mysbx-reattach-stub"
        mkdir -p "$state"
        count_file="$state/attach"
        alive_file="$state/alive"
        count=0
        [ -f "$count_file" ] && count=$(cat "$count_file")
        count=$((count + 1))
        printf '%s' "$count" >"$count_file"
        if [ "$count" -ge 2 ]; then
          rm -f "$alive_file"
        elif [ -n "''${MYSBX_STUB_NO_ALIVE:-}" ]; then
          : # the replay pins a server that never had a session
        else
          : >"$alive_file"
        fi
        exit "''${MYSBX_STUB_ATTACH_EXIT:-0}"
      '';
      aoeEntry = pkgs.callPackage ./aoe-entry.nix {
        aoe = aoeStub;
        tmux = cfg.tmux.package;
      };
      # A stub herdr — the entry is asserted statically here; the
      # real package's own check is mysbx-herdr-entry-test.
      herdrStub = pkgs.writeShellScriptBin "herdr" ''
        exit 0
      '';
      herdrEntry = pkgs.callPackage ./herdr-entry.nix {
        herdr = herdrStub;
        tmux = cfg.tmux.package;
      };
      # A stateful tmux stub: `attach-session` increments a counter,
      # creates the alive marker on the FIRST attach (a fresh server
      # has a session) and kills it on the SECOND (the last-pane-exit
      # replay), `has-session` stats the marker, `ls` is the aoe
      # probe (below): a `ls` WITHOUT `-S` asks tmux's DEFAULT
      # socket — dead unless the stub is told otherwise — and a `ls`
      # with `-S <socket>` answers from the marker, so a server on a
      # NAMED socket is alive exactly when the marker exists (the
      # aoe `[tmux] socket_name` situation, bd myconfig-7ioq.15).
      # Anything else passes through with exit 0. The attach exit
      # code is overridable via `MYSBX_STUB_ATTACH_EXIT` for the
      # plain-path scenario (d). Matches on the whole argv so the
      # entries' SHORT `tmux attach-session` (the fragment) and the
      # `exec`-form `tmux -S … attach-session` (the plain path) land
      # in the same arm.
      tmuxStub = pkgs.writeShellScriptBin "tmux" ''
        state="''${TMPDIR:-/tmp}/mysbx-reattach-stub"
        case " $* " in
          *" attach-session "*)
              count_file="$state/attach"
              alive_file="$state/alive"
              mkdir -p "$state"
              count=0
              [ -f "$count_file" ] && count=$(cat "$count_file")
              count=$((count + 1))
              printf '%s' "$count" >"$count_file"
              if [ "$count" -ge 2 ]; then
                  rm -f "$alive_file"
              else
                  : >"$alive_file"
              fi
              exit "''${MYSBX_STUB_ATTACH_EXIT:-0}"
              ;;
          *" has-session "*)
              [ -e "''${TMPDIR:-/tmp}/mysbx-reattach-stub/alive" ]
              ;;
          *" ls "*)
              # `ls` (aoe's probe): a plain `ls` asks tmux's DEFAULT
              # socket and answers DEAD — the (g) scenario's server
              # lives on a NAMED socket, so only a probe that asks
              # `-S <socket>` can see it (the aoe `[tmux]
              # socket_name` situation, bd myconfig-7ioq.15).
              case " $* " in
                *" -S "*)
                    [ -e "''${TMPDIR:-/tmp}/mysbx-reattach-stub/alive" ]
                    ;;
                *)
                    exit 1
                    ;;
              esac
              ;;
          *)
              exit 0
              ;;
        esac
      '';
      # The runner replays one entry's extracted gate block against
      # the stub with a given stdin and environment, and prints
      # `<rc> <count>' for the scenario assertions below.
      runner = pkgs.writeShellScript "mysbx-reattach-runner" ''
        set -u
        entry="$1"
        stdin="$2"
        export PATH="${tmuxStub}/bin:${pkgs.bashInteractive}/bin:$PATH"
        rm -rf "''${TMPDIR:-/tmp}/mysbx-reattach-stub"
        gate="$(sed -n '/^if \[ "''${MYSBX_DETACHABLE:-}" = "1" \]; then$/,/^fi$/p' "$entry")"
        [ -n "$gate" ] || {
          echo "cannot extract the gate block" >&2
          exit 99
        }
        # The `tmux()` socket-pinning function, exactly as the entry
        # defines it, plus the vars the gate references. Both are
        # exported so the `bash -c` below sees them.
        prelude="$(sed -n '/^tmux() /p' "$entry")"
        [ -n "$prelude" ] || {
          echo "cannot extract the tmux() prelude" >&2
          exit 98
        }
        export session=mysbx-test
        export socket="''${TMPDIR:-/tmp}/mysbx-reattach-stub/socket"
        # The gate's stderr (the prompt) passes through to the
        # runner's stderr, its stdout is empty by construction.
        # The entries run under writeShellApplication's strict mode;
        # errexit is the D8 propagation under test in scenario (e).
        printf '%s' "$stdin" \
          | MYSBX_DETACHABLE=''${MYSBX_DETACHABLE:-} bash -c "set -o errexit
        $prelude
        $gate"
        rc=$?
        count=0
        [ -f "''${TMPDIR:-/tmp}/mysbx-reattach-stub/attach" ] \
          && count=$(cat "''${TMPDIR:-/tmp}/mysbx-reattach-stub/attach")
        printf '%s %s' "$rc" "$count"
      '';
      # The B7-liveness runner (bd myconfig-7ioq.15): stdin is an
      # OPEN pipe that never delivers a line, and the alive marker is
      # killed mid-wait (`kill-after` seconds in), so the gate MUST
      # end the run on its own — a gate that blocks in `read`
      # forever hits the `timeout` bound and fails. The elapsed time
      # is measured from the kill and printed third, so the scenario
      # can pin the ~2s bound of the acceptance (the poll interval
      # plus one probe). Prints `<rc> <count> <elapsed>'.
      killRunner = pkgs.writeShellScript "mysbx-reattach-kill-runner" ''
        set -u
        entry="$1"
        kill_after="$2"
        export PATH="${tmuxStub}/bin:${pkgs.bashInteractive}/bin:${pkgs.coreutils}/bin:$PATH"
        rm -rf "''${TMPDIR:-/tmp}/mysbx-reattach-stub"
        gate="$(sed -n '/^if \[ "''${MYSBX_DETACHABLE:-}" = "1" \]; then$/,/^fi$/p' "$entry")"
        [ -n "$gate" ] || {
          echo "cannot extract the gate block" >&2
          exit 99
        }
        prelude="$(sed -n '/^tmux() /p' "$entry")"
        [ -n "$prelude" ] || {
          echo "cannot extract the tmux() prelude" >&2
          exit 98
        }
        export session=mysbx-test
        export socket="''${TMPDIR:-/tmp}/mysbx-reattach-stub/socket"
        fifo="''${TMPDIR:-/tmp}/mysbx-reattach-stub.fifo"
        rm -f "$fifo"
        mkfifo "$fifo"
        # Hold the write end open forever: `read` gets no line and no
        # EOF, so the ONLY way the gate can end is the liveness poll.
        sleep 60 >"$fifo" &
        holder=$!
        # The killer records the moment the marker dies, so the
        # elapsed-time check below can pin the ~2s bound: the gate's
        # poll must notice within one interval plus one probe.
        ( sleep "$kill_after"; date +%s >"''${TMPDIR:-/tmp}/mysbx-reattach-stub/kill_time"; rm -f "''${TMPDIR:-/tmp}/mysbx-reattach-stub/alive" ) &
        killer=$!
        timeout 10 bash -c "set -o errexit
        $prelude
        $gate" <"$fifo"
        rc=$?
        end=$(date +%s)
        kill "$holder" "$killer" 2>/dev/null || true
        wait "$holder" "$killer" 2>/dev/null || true
        rm -f "$fifo"
        count=0
        [ -f "''${TMPDIR:-/tmp}/mysbx-reattach-stub/attach" ] \
          && count=$(cat "''${TMPDIR:-/tmp}/mysbx-reattach-stub/attach")
        elapsed=-1
        [ -f "''${TMPDIR:-/tmp}/mysbx-reattach-stub/kill_time" ] \
          && elapsed=$((end - $(cat "''${TMPDIR:-/tmp}/mysbx-reattach-stub/kill_time")))
        printf '%s %s %s' "$rc" "$count" "$elapsed"
      '';
      # The aoe replay runner: extracts aoe's gate AND its own
      # `aoe_alive()` probe (the entry defines no `tmux()` prelude,
      # so the sibling runner's extraction does not apply), so the
      # gate can be replayed against the stub instead of only
      # asserted statically. `$TMUX_TMPDIR` is pointed at the stub
      # state dir, so the probe's every-socket glob sees
      # `$state/aoe-socket`; the stub answers `-S` `ls` from the
      # alive marker. Prints `<rc> <count>' like the runner above.
      #
      # The python heredoc below is the nix string's strip anchor:
      # its terminator must stay the least-indented line of the
      # whole `''` body, or the strip width shifts and the rendered
      # heredoc breaks.
      aoeRunner = pkgs.writeShellScript "mysbx-reattach-aoe-runner" ''
        set -u
        entry="$1"
        stdin="$2"
        # Optional third argument: the socket NAME the stub server
        # binds under the per-user tmux dir (default `aoe-socket`).
        # A name starting with a dot (bd myconfig-7ioq.22) replays
        # the glob-skips-dotfiles situation.
        sock_name="''${3:-aoe-socket}"
        export PATH="${aoeStub}/bin:${tmuxStub}/bin:${pkgs.coreutils}/bin:${pkgs.python3}/bin:${pkgs.bashInteractive}/bin:$PATH"
        rm -rf "''${TMPDIR:-/tmp}/mysbx-reattach-stub"
        mkdir -p "''${TMPDIR:-/tmp}/mysbx-reattach-stub"
        gate="$(sed -n '/^if \[ "''${MYSBX_DETACHABLE:-}" = "1" \]; then$/,/^fi$/p' "$entry")"
        [ -n "$gate" ] || {
          echo "cannot extract the gate block" >&2
          exit 99
        }
        probe="$(sed -n '/^aoe_alive() ($/,/^)$/p' "$entry")"
        [ -n "$probe" ] || {
          echo "cannot extract the aoe_alive probe" >&2
          exit 97
        }
        # The probe's alive marker, on a NAMED socket under the
        # per-user tmux dir (`$TMUX_TMPDIR/tmux-<uid>/`) so only
        # `tmux -S <socket> ls` (the every-socket probe) finds it —
        # a plain `tmux ls` asks the DEFAULT socket and is dead. The
        # socket file is bound for real (python `AF_UNIX`): the
        # probe's `-S` test must not be satisfied by a regular file.
        state="''${TMPDIR:-/tmp}/mysbx-reattach-stub"
        mkdir -p "$state/tmux-$(id -u)"
        if [ -z "''${MYSBX_STUB_NO_ALIVE:-}" ]; then
          : >"$state/alive"
        fi
        python3 - "$state/tmux-$(id -u)/$sock_name" <<'PYEOF'
        import socket, sys
        s = socket.socket(socket.AF_UNIX)
        s.bind(sys.argv[1])
        PYEOF
        # The bind above must hold, so every scenario provably runs
        # against a real socket file (a failed bind would leave the
        # glob empty and the scenarios would pass vacuously).
        [ -S "$state/tmux-$(id -u)/$sock_name" ] || {
          echo "cannot bind the stub socket" >&2
          exit 96
        }
        export TMUX_TMPDIR="$state"
        printf '%s' "$stdin" \
          | MYSBX_DETACHABLE=''${MYSBX_DETACHABLE:-} bash -c "set -o errexit
        $probe
        $gate"
        rc=$?
        count=0
        [ -f "''${TMPDIR:-/tmp}/mysbx-reattach-stub/attach" ] \
          && count=$(cat "''${TMPDIR:-/tmp}/mysbx-reattach-stub/attach")
        printf '%s %s' "$rc" "$count"
      '';
    in
    pkgs.runCommand "mysbx-reattach-loop-test"
      {
        nativeBuildInputs = with pkgs; [
          bashInteractive
          gnugrep
          gnused
        ];
      }
      ''
        fail() {
          echo "mysbx-reattach-loop-test: $*" >&2
          exit 1
        }

        tmux_entry='${lib.getExe tmuxEntry}'
        workmux_entry='${lib.getExe workmuxEntry}'
        aoe_entry='${lib.getExe aoeEntry}'
        herdr_entry='${lib.getExe herdrEntry}'
        for e in "$tmux_entry" "$workmux_entry" "$aoe_entry" "$herdr_entry"; do
          test -x "$e" || fail "entry not executable: $e"
          bash -n "$e" || fail "bash -n rejects $e"
        done

        prompt='session still running — Enter: reattach, q: end the sandbox, ^\: detach mysbx'

        # Scenario runner: detach (Enter), q, EOF, unset. Each scenario
        # starts from a fresh stub state.
        scenario() { # entry, label, stdin, MYSBX_DETACHABLE, want_rc, want_count, want_prompt
          entry="$1"; label="$2"; input="$3"; flag="$4"; want_rc="$5"; want_count="$6"; want_prompt="$7"
          # `export`, not an assignment prefix: prefix assignments do
          # not reach a binary inside a command substitution.
          export MYSBX_DETACHABLE TMPDIR="$PWD/tmpdir"
          if [ -n "$flag" ]; then
            MYSBX_DETACHABLE="$flag"
          else
            unset MYSBX_DETACHABLE
          fi
          result="$(${runner} "$entry" "$input")" \
            || fail "$label: the runner failed"
          rc="''${result% *}"
          count="''${result#* }"
          [ "$rc" = "$want_rc" ] \
            || fail "$label: rc $rc, want $want_rc"
          [ "$count" = "$want_count" ] \
            || fail "$label: attach count $count, want $want_count"
          err="$(${runner} "$entry" "$input" 2>&1 >/dev/null)" \
            || fail "$label: the runner failed (stderr pass)"
          if [ "$want_prompt" = yes ]; then
            printf '%s\n' "$err" | grep -Fq "$prompt" \
              || fail "$label: the prompt is missing or wrong"
          else
            if printf '%s\n' "$err" | grep -Fq "$prompt"; then
              fail "$label: the plain path must not print the prompt"
            fi
          fi
        }

        mkdir -p "$PWD/tmpdir"
        for entry in "$tmux_entry" "$workmux_entry"; do
          name="$(basename "$entry")"
          # (a) detach → prompt → Enter reattaches; the second attach
          # kills the alive marker (last pane exit) → the run ends.
          scenario "$entry" "$name (a) reattach" $'\n' 1 0 2 yes
          # (b) q ends the run — a terminated line, so `read`
          # returns 0 and the q branch (not EOF) is taken.
          scenario "$entry" "$name (b) q" $'q\n' 1 0 1 yes
          # (c) EOF (the dtach master died) ends the run like q.
          scenario "$entry" "$name (c) EOF" "" 1 0 1 yes
          # (d) without the variable the gate execs the client; the
          # stub's exit code must propagate (cli.md D8).
          MYSBX_STUB_ATTACH_EXIT=42 \
            scenario "$entry" "$name (d) unset" "" "" 42 1 no
          # (e) a failing attach under the variable ends the run with
          # the attach's code (errexit, cli.md D8).
          MYSBX_STUB_ATTACH_EXIT=42 \
            scenario "$entry" "$name (e) attach fails" "" 1 42 1 no
          # (f) B7 liveness (bd myconfig-7ioq.15): the last pane
          # exits WHILE the prompt is shown and stdin stays silent —
          # an open pipe, no line, no EOF. The gate must end the run
          # on its own; a gate that blocks in `read` hits the
          # runner's 10s bound (rc 124), and the elapsed time pins
          # the acceptance's ~2s: the poll notices the death within
          # one interval plus one probe, so anything above 3s means
          # the wait stopped re-probing.
          result="$(MYSBX_DETACHABLE=1 TMPDIR="$PWD/tmpdir" ${killRunner} "$entry" 2)" \
            || fail "$name (f) prompt liveness: the kill runner failed"
          rc="''${result%% *}"; tail="''${result#* }"
          count="''${tail%% *}"; elapsed="''${tail#* }"
          [ "$rc" = 0 ] \
            || fail "$name (f) prompt liveness: rc $rc (124 = the gate blocks in read), want 0"
          [ "$count" = 1 ] \
            || fail "$name (f) prompt liveness: attach count $count, want 1"
          [ "$elapsed" -le 3 ] \
            || fail "$name (f) prompt liveness: the gate noticed the death ''${elapsed}s after it, want ≤3"
        done

        # The aoe row: its gate is REPLAYED against the stub, not
        # just asserted statically — the stub answers a plain `ls`
        # (tmux's DEFAULT socket) DEAD, while `-S <named socket>`
        # `ls` answers from the marker (bd myconfig-7ioq.15: a
        # persisted `[tmux] socket_name` moves aoe's server onto a
        # named socket, and only an every-socket probe sees it).
        # (g) a named-socket server: the probe must see it — the
        # prompt appears and `q` ends the run after ONE attach.
        result="$(MYSBX_DETACHABLE=1 TMPDIR="$PWD/tmpdir" ${aoeRunner} "$aoe_entry" $'q\n')" \
          || fail "aoe (g) named socket: the aoe runner failed"
        rc="''${result% *}"; count="''${result#* }"
        [ "$rc" = 0 ] \
          || fail "aoe (g) named socket: rc $rc, want 0"
        [ "$count" = 1 ] \
          || fail "aoe (g) named socket: attach count $count, want 1"
        err="$(MYSBX_DETACHABLE=1 TMPDIR="$PWD/tmpdir" ${aoeRunner} "$aoe_entry" $'q\n' 2>&1 >/dev/null)" \
          || fail "aoe (g) named socket: the aoe runner failed (stderr pass)"
        printf '%s\n' "$err" | grep -Fq "$prompt" \
          || fail "aoe (g) named socket: the probe missed the named-socket server (no prompt)"

        # (h) a DOT-PREFIXED socket name (bd myconfig-7ioq.22): a
        # bare `[tmux] socket_name` may start with a dot, so the
        # server's socket is a dotfile under tmux-<uid>/ — default
        # bash globbing skips it, and a probe without dotglob would
        # report the run dead and end the sandbox on `q` without a
        # prompt. The probe must see the dotted server: the prompt
        # appears and `q` ends after ONE attach; scenario (i) below
        # pins the stale counterpart.
        result="$(MYSBX_DETACHABLE=1 TMPDIR="$PWD/tmpdir" ${aoeRunner} "$aoe_entry" $'q\n' .aoe-socket)" \
          || fail "aoe (h) dotted socket: the aoe runner failed"
        rc="''${result% *}"; count="''${result#* }"
        [ "$rc" = 0 ] \
          || fail "aoe (h) dotted socket: rc $rc, want 0"
        [ "$count" = 1 ] \
          || fail "aoe (h) dotted socket: attach count $count, want 1"
        err="$(MYSBX_DETACHABLE=1 TMPDIR="$PWD/tmpdir" ${aoeRunner} "$aoe_entry" $'q\n' .aoe-socket 2>&1 >/dev/null)" \
          || fail "aoe (h) dotted socket: the aoe runner failed (stderr pass)"
        printf '%s\n' "$err" | grep -Fq "$prompt" \
          || fail "aoe (h) dotted socket: the probe missed the dotted server (no prompt)"

        # (i) a stale dotted socket (the file exists, no server
        # answers) counts as NOT alive: the stub never arms the
        # alive marker (`MYSBX_STUB_NO_ALIVE`), so the probe's
        # `ls -S` fails although the socket FILE is there — the gate
        # must end the run right after the FIRST attach, printing NO
        # prompt (no reattach is offered against a dead server).
        result="$(MYSBX_DETACHABLE=1 TMPDIR="$PWD/tmpdir" MYSBX_STUB_NO_ALIVE=1 ${aoeRunner} "$aoe_entry" ''' .aoe-socket)" \
          || fail "aoe (i) stale dotted socket: the aoe runner failed"
        rc="''${result% *}"; count="''${result#* }"
        [ "$rc" = 0 ] \
          || fail "aoe (i) stale dotted socket: rc $rc, want 0"
        [ "$count" = 1 ] \
          || fail "aoe (i) stale dotted socket: attach count $count, want 1"
        err="$(MYSBX_DETACHABLE=1 TMPDIR="$PWD/tmpdir" MYSBX_STUB_NO_ALIVE=1 ${aoeRunner} "$aoe_entry" ''' .aoe-socket 2>&1 >/dev/null)" \
          || fail "aoe (i) stale dotted socket: the aoe runner failed (stderr pass)"
        if printf '%s\n' "$err" | grep -Fq "$prompt"; then
          fail "aoe (i) stale dotted socket: a dead server must not offer the prompt"
        fi

        # The shared-fragment drift guard: the tmux and workmux gate
        # blocks are byte-identical (only the entries around them
        # differ — the attach/probe happen to match for both).
        sed -n '/^if \[ "''${MYSBX_DETACHABLE:-}" = "1" \]; then$/,/^fi$/p' "$tmux_entry" >"$PWD/gate-tmux"
        sed -n '/^if \[ "''${MYSBX_DETACHABLE:-}" = "1" \]; then$/,/^fi$/p' "$workmux_entry" >"$PWD/gate-workmux"
        cmp "$PWD/gate-tmux" "$PWD/gate-workmux" \
          || fail "the tmux and workmux gate blocks drifted apart"

        # Static presence of the same fragment in aoe and herdr: the
        # plain-path exec line survives verbatim and the gate names
        # the entry's own attach and liveness probe. aoe's probe is
        # the every-socket `aoe_alive()` (bd myconfig-7ioq.15), so it
        # is pinned by its signature pieces — the probe function
        # call in the gate, the per-user tmux dir glob, and the
        # per-socket `ls`. The live reattach is the bead's manual
        # acceptance.
        grep -qF 'exec aoe "$@"' "$aoe_entry" \
          || fail "the aoe entry lost its plain-path exec line"
        # The gate names the probe; the probe itself lives in the
        # entry body (outside the gate block), pinned by its glob and
        # its per-socket `ls`.
        sed -n '/^if \[ "''${MYSBX_DETACHABLE:-}" = "1" \]; then$/,/^fi$/p' "$aoe_entry" \
          | grep -qF 'aoe_alive' \
          || fail "the aoe gate does not call the aoe_alive probe"
        for s in 'tmux-$(id -u)' 'tmux -S "$sock" ls'; do
          grep -qF "$s" "$aoe_entry" \
            || fail "the aoe entry does not contain: $s"
        done
        sed -n '/^if \[ "''${MYSBX_DETACHABLE:-}" = "1" \]; then$/,/^fi$/p' "$aoe_entry" \
          | grep -qF "$prompt" \
          || fail "the aoe gate does not contain: $prompt"
        grep -qF 'exec herdr "$@"' "$herdr_entry" \
          || fail "the herdr entry lost its plain-path exec line"
        for s in 'herdr "$@"' 'herdr workspace list >/dev/null 2>&1' "$prompt"; do
          sed -n '/^if \[ "''${MYSBX_DETACHABLE:-}" = "1" \]; then$/,/^fi$/p' "$herdr_entry" \
            | grep -qF "$s" \
            || fail "the herdr gate does not contain: $s"
        done

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

  # The hermes tier (../../hermes-agent/mysbx.nix), evaluated against the
  # REAL reference host that enables both halves (`test-p14`:
  # `hermes.enable` in hosts/host.p14/hermes-agent.nix, `mysbx.enable` in
  # hosts/host.p14/ai.p14.nix). Two properties are asserted:
  #
  #   * the launcher is on the sandbox PATH AND baked into the podman image
  #     (the container backends run the image's own `/bin`, so a PATH-only
  #     entry would be invisible there), `.hermes` is a `state-dirs` entry, and
  #     the podman backends carry `CUSTOM_BASE_URL` — a container's loopback is
  #     its own, so the seeded host URL is wrong there;
  #   * the seed itself: a FRESH home gets a config naming the LiteLLM route
  #     and the placeholder key (so no first-run wizard appears), and an
  #     EXISTING config is left byte-identical (the payload's own `hermes
  #     model` must survive across runs through the `state-dirs` store).
  mysbx-hermes-test =
    let
      lib = inputs.nixpkgs.lib;
      cfg = self.nixosConfigurations.test-p14.config;
      mysbxCfg = cfg.myconfig.ai.dev.mysbx;
      named = prefix: ps: builtins.filter (p: lib.hasPrefix prefix (p.name or "")) ps;
      launchers = named "hermes-agent-mysbx-" mysbxCfg.extraTools;
      imageLaunchers = named "hermes-agent-mysbx-" mysbxCfg.podman.imagePackages;
      expectedBaseUrl = cfg.myconfig.ai.dev.hermes.model.baseUrl;
      evalAssertions = [
        {
          assertion = builtins.length launchers == 1;
          message = "mysbx-hermes-test: expected exactly one hermes launcher on the sandbox PATH, got: ${
            lib.concatStringsSep ", " (map (p: p.name) mysbxCfg.extraTools)
          }";
        }
        {
          assertion = builtins.length imageLaunchers == 1;
          message = "mysbx-hermes-test: the launcher is not baked into the podman image — a container session would not find `hermes`";
        }
        {
          assertion = builtins.elem ".hermes" mysbxCfg.config.stateDirs;
          message = "mysbx-hermes-test: `.hermes` is not a state-dirs entry — sessions would die with the tmpfs home";
        }
        {
          assertion =
            (mysbxCfg.podman.env.CUSTOM_BASE_URL or null) == cfg.myconfig.ai.dev.litellm-forwarder.endpoint;
          message = "mysbx-hermes-test: the podman backends do not point hermes at the forwarded endpoint (got ${
            toString (mysbxCfg.podman.env.CUSTOM_BASE_URL or null)
          })";
        }
      ];
      failures = builtins.filter (a: !a.assertion) evalAssertions;
      evalGate =
        if failures != [ ] then
          throw "mysbx-hermes-test: ${lib.concatMapStringsSep "\n  - " (f: f.message) failures}"
        else
          "ok";
      launcher = builtins.head launchers;
    in
    pkgs.runCommand "mysbx-hermes-test"
      {
        inherit evalGate launcher;
        seed = launcher.seed;
        baseUrl = expectedBaseUrl;
        nativeBuildInputs = with pkgs; [
          gnugrep
          coreutils
        ];
      }
      ''
        fail() {
          echo "mysbx-hermes-test: $*" >&2
          exit 1
        }

        [ "$evalGate" = ok ] || { echo "mysbx-hermes-test: eval gate: $evalGate" >&2; exit 1; }

        # The launcher runs the seeder BEFORE the upstream binary: a static
        # contract, the same shape the claude-code launcher check uses.
        grep -q "$seed" "$launcher/bin/hermes" \
          || fail "bin/hermes does not run the config seeder"
        grep -q 'hermes-upstream' "$launcher/bin/hermes" \
          || fail "bin/hermes does not exec the upstream binary"

        # A fresh home is seeded with the LiteLLM route and the placeholder
        # key (the value `_has_any_provider_configured` needs so no
        # first-run wizard appears).
        fresh="$(mktemp -d)"
        HERMES_HOME="$fresh/.hermes" "$seed/bin/hermes-mysbx-seed-config"
        test -f "$fresh/.hermes/config.yaml" || fail "the seeder wrote no config.yaml"
        grep -q "base_url: $baseUrl" "$fresh/.hermes/config.yaml" \
          || fail "the seeded config does not name the LiteLLM route"
        grep -q 'api_key: local-key' "$fresh/.hermes/config.yaml" \
          || fail "the seeded config does not carry the placeholder key"
        grep -q 'default: hermes' "$fresh/.hermes/config.yaml" \
          || fail "the seeded config does not name the default model"

        # An existing config (the payload's own, persisted in the sidecar
        # store) is never rewritten.
        printf 'model:\n  default: mine\n' > "$fresh/.hermes/config.yaml"
        HERMES_HOME="$fresh/.hermes" "$seed/bin/hermes-mysbx-seed-config"
        grep -q 'default: mine' "$fresh/.hermes/config.yaml" \
          || fail "the seeder overwrote an existing config.yaml"
        if grep -q 'local-key' "$fresh/.hermes/config.yaml"; then
          fail "the seeder appended to an existing config.yaml"
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
    # config.
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
