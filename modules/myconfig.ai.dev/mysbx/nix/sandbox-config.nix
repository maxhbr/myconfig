# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# `mkSandboxConfig` — the sanctioned way a tool module mounts
# home-manager-deployed configuration into a mysbx sandbox
# (../docs/design/config.md D6): it builds one self-contained store
# tree of dereferenced REAL-FILE copies of the mounted subtrees, plus
# the ready-to-assign `myconfig.ai.dev.mysbx.config.mounts` entries
# that bind those subtrees read-only below the sandbox home
# (`/mysbx-home`, D14).
#
# Why copies are needed at all: home-manager deploys `home.file` entries
# as SYMLINKS into its `<hash>-home-manager-files` generation tree,
# whose leaves may themselves be symlinks into package store paths — a
# two-hop chain. The podman-gvisor backend deliberately mounts NOTHING
# from the host /nix/store (../mysbx-rs/src/podman_gvisor.rs: the
# image's own userland provides everything), and while the FINAL hop is
# in the image, the INTERMEDIATE home-manager-files path is not, so a
# mount of the raw host tree hands the container dangling symlinks and
# the tool starts with almost no configuration (observed with pi: bd
# myconfig-576). Mounting dereferenced copies from the built tree
# instead keeps every mount path an absolute store path that exists —
# canonicalization (D8) is a no-op. (The bubblewrap backend is
# unaffected either way: it ro-binds the host /nix, so both spellings
# resolve there.)
#
# The copy semantics follow the seed.rs precedent
# (../../sandboxes/myconfig.ai.gvisor-agent-sandbox/rust/src/seed.rs —
# "Files are copies, not symlinks, because the sandbox has no /nix").
#
# Consumed like ./mux-entry-lib.nix — a plain function file, no NixOS
# option machinery. A `myconfig.ai.dev.mysbx.helpers` option was
# considered and rejected: the library needs no module state, a plain
# import cannot introduce option-merge or priority surprises, and the
# callers already import sibling library files the same way.
{ lib, runCommand }:
{
  # `homeFile` — home-manager's merged `home.file` option of the user
  #               whose mysbx config layer is generated: the single
  #               source of truth (see below).
  # `subtrees` — the mounted subtrees: relative paths under `$HOME`
  #               (e.g. `.pi/agent/extensions` or `.config/opencode`).
  #               The ONLY tool-specific input; everything else is
  #               generic. A subtree may be a FILE (its copied store
  #               source is the file, not a directory).
  # `name`     — derivation name (e.g. "pi-sandbox-config"); also the
  #               prefix of the source re-import names, so every tree
  #               in the store names its owner.
  # `homeDirectory` — the user's `home.homeDirectory`. Home Manager
  #               keys `xdg.configFile` entries into `home.file` under
  #               the ABSOLUTE `xdg.configHome` (its default:
  #               `${home.homeDirectory}/.config`), so most tool config
  #               appears under `/home/<user>/.config/…` keys while
  #               directly-set `home.file` entries stay relative.
  #               Targets are normalized by stripping this prefix, or
  #               the subtree match would silently miss everything
  #               deployed through `xdg.configFile`.
  #
  # Returns `{ tree, mounts }`:
  #   `tree`   — the dereferenced copy derivation;
  #   `mounts` — the ready-to-assign list of
  #              `myconfig.ai.dev.mysbx.config.mounts` entries.
  mkSandboxConfig =
    {
      homeFile,
      subtrees,
      name,
      homeDirectory ? null,
    }:
    let
      # Home-manager's own trick (modules/files.nix `sourceStorePath`): a
      # `source` that is a context-less store path — a flake source tree —
      # would NOT become a derivation input when stringified into the copy
      # script, and the build would fail with `cannot stat` on a builder
      # that lacks the path (observed: the rtk skill dir, bd myconfig-576).
      # Re-importing it via `builtins.path` adds the context.
      sourceWithContext =
        source:
        let
          str = toString source;
        in
        if builtins.hasContext str then
          str
        else
          builtins.path {
            path = source;
            name = lib.strings.sanitizeDerivationName "${name}-source-${baseNameOf str}";
            recursive = lib.filesystem.pathIsDirectory source;
          };

      # The single source of truth both the host deployment and the
      # sandbox copies are built from: home-manager's merged `home.file`
      # option, passed in by the caller. Every file a tool module deploys
      # below a mounted subtree — whether via `home.file`, `xdg.configFile`
      # (which home-manager feeds into `home.file`; see `homeDirectory`
      # above for the key spelling) or a `text`-style entry (materialised
      # as a `source` derivation, file-type.nix) — is an entry of this
      # attrset, so the sandbox tree can never drift from what the host
      # actually deploys: a new entry under a mounted subtree lands in
      # the sandbox automatically on the next build.
      #
      # The caller must reference the result only under
      # `mkIf (…mysbx.enable …)` (Nix is lazy), like `hmRipgrep` in
      # ../default.nix. It CANNOT be circular: it feeds
      # `…mysbx.config.mounts`, which the mysbx module renders into
      # `home-manager.users.<user>.xdg.configFile."mysbx/config.toml"`
      # — and `xdg.configFile` feeds `home.file` keyed by `.config/mysbx/…`
      # (relatively or absolutely spelled; the normalization below maps
      # both onto `.config/mysbx/…`), a prefix the subtree filter never
      # matches (callers must never list `.config/mysbx` as a subtree),
      # so `filterAttrs`'s lazy value forcing stops before the cycle
      # could close.
      normalize =
        t:
        if homeDirectory != null && lib.hasPrefix "${homeDirectory}/" t then
          lib.removePrefix "${homeDirectory}/" t
        else
          t;
      inSubtree = target: builtins.any (sub: target == sub || lib.hasPrefix "${sub}/" target) subtrees;
      entries = lib.mapAttrs' (target: v: lib.nameValuePair (normalize target) v) (
        lib.filterAttrs (target: _: inSubtree (normalize target)) homeFile
      );

      # Home-manager's own deployment skips `enable = false` entries
      # (modules/files.nix `enabledFiles`); the copies must match, or a
      # disabled entry would exist inside the sandbox but not in `~`.
      # BTree-style order: `mkdir -p` of a later entry's parent must not
      # collide with an earlier copied FILE. Sorting by target length
      # puts parents (shorter paths) before their children, the same
      # invariant home-manager's own `files.nix` sorts by.
      sorted = lib.sortOn (e: builtins.stringLength e.target) (
        lib.filter (e: e.enable) (lib.attrValues entries)
      );
      copyLine =
        e:
        let
          src = sourceWithContext e.source;
        in
        ''

          mkdir -p "$out/${lib.dirOf e.target}"
          cp -RL --no-preserve=mode -- ${lib.escapeShellArg (toString src)} "$out/${e.target}"'';

      # Subtrees no enabled entry populated still get their (empty)
      # directory: the mount path must exist in the tree, or mysbx's
      # eager canonicalization fails the run as a missing mount source
      # (D8). Subtrees WITH entries are skipped — `mkdir -p` on an
      # already-copied FILE path (a file subtree) would fail the build.
      subtreeCopied = sub: builtins.any (e: e.target == sub || lib.hasPrefix "${sub}/" e.target) sorted;
      emptySubtrees = builtins.filter (sub: !subtreeCopied sub) subtrees;

      tree = runCommand name { } (
        ''
          mkdir -p "$out"
        ''
        + lib.concatStrings (map copyLine sorted)
        + lib.concatMapStrings (sub: ''

          mkdir -p "$out/${sub}"'') emptySubtrees
        + ''

          if [ -n "$(find "$out" -type l -print -quit)" ]; then
            echo "${name}: symlinks left in the output tree" >&2
            exit 1
          fi
        ''
      );

      # Every entry carries a `dest` under `/mysbx-home` because `HOME`
      # is `/mysbx-home` in the sandbox (D14) and the tool looks for its
      # config below `$HOME`. The `path` is a subtree of `tree`: an
      # absolute store path of REAL files instead of the host `~/…`
      # symlink tree. A FILE subtree stays a FILE bind: its store
      # source is the copied file, not a directory.
      mounts = map (sub: {
        path = "${tree}/${sub}";
        dest = "/mysbx-home/${sub}";
        mode = "ro";
      }) subtrees;
    in
    {
      inherit tree mounts;
    };
}
