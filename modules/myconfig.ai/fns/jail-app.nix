# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Reusable wrapper around the vendored jail.nix library for jailing LLM
# coding agents (pi, opencode, claude-code, crush, gemini-cli, ...).
#
# Usage:
#
#   let
#     callLib = file: import file { inherit lib pkgs jail; };
#     jail-app = callLib ../fns/jail-app.nix;
#   in
#   jail-app {
#     name = "jailed-pi";
#     pkg = pkgs.pi-coding-agent;
#     userDataDirs = [ ".pi" ];
#   }
#
# All "default" lists (configDirs, devTools, fwdEnv, userDataDirs) can be
# either replaced wholesale or extended via the matching `extra*` argument.
# In addition, every wrapper inherits `myconfig.ai.jail.fwdEnvs` from the
# NixOS config passed in as `osconfig` — see `globalFwdEnvs` below. This
# extends the always-forwarded `OPENAI_API_KEY`. The shared sandbox tool
# option `myconfig.ai.sandboxTools` (packages + env) is inherited the same
# way — see `sharedTools`/`sharedEnv` below.
#
# The resulting derivation is a `jail` permission bundle. See
# `vendor/alexdavid-jail.nix/lib/combinators/` for available primitives.
{
  lib,
  pkgs,
  jail,
  # The NixOS `config`, used to read the shared `myconfig.ai.jail.fwdEnvs`
  # option (see `../myconfig.ai.jail.nix`) so every `jail-app` wrapper picks
  # up the global forwarded-env list without each call site having to pass
  # it explicitly. Defaults to `{}` so the library still works standalone
  # (e.g. in `nix repl`); in that case `globalFwdEnvs` reduces to just the
  # always-forwarded `OPENAI_API_KEY`.
  osconfig ? { },
}:

{
  # Required: unique name of the resulting wrapper binary.
  name,

  # Required: the package whose `lib.getExe pkg` is jailed.
  pkg,

  # Writable home directories for the agent's own state/config (e.g.
  # [ ".pi" ] for pi, [ ".claude" ] for claude-code). Each entry is
  # interpreted relative to $HOME and rw-bound into the jail. The helper
  # creates the host-side directory with `mkdir -p` if it does not exist.
  # For single-file state (e.g. ".claude.json") use `userDataFiles`
  # instead.
  userDataDirs ? [ ],

  # Writable single files in the home directory for the agent's state.
  # Use this instead of `userDataDirs` for entries that are files rather
  # than directories (e.g. ".claude.json"). The helper `touch`es the
  # host-side file so the bind mount has something to attach to.
  userDataFiles ? [ ],

  # Read-only host config directories (relative to $HOME) that tools inside
  # the jail should pick up. Each entry is bound read-only only if it exists
  # on the host; a missing directory is skipped silently (try-ro-bind), so a
  # jail never fails to start because an optional config dir is absent.
  # Override to replace; use `extraConfigDirs` to append.
  configDirs ? [
    ".config/pistol"
    ".config/ripgrep"
    ".config/bat"
    ".config/git"
  ],
  extraConfigDirs ? [ ],

  # Developer tools made available inside the jail via add-pkg-deps. Mirrors
  # andersonjoseph/jailed-agents' commonPkgs plus a few extras (fd, ripgrep,
  # less, wget, curl, jq, nix) we use everywhere. Override to replace; use
  # `extraDevTools` to append.
  devTools ? [
    pkgs.bashInteractive
    pkgs.git
    pkgs.coreutils
    pkgs.findutils
    pkgs.gnugrep
    pkgs.gnused
    pkgs.gawk
    pkgs.ripgrep
    pkgs.fd
    pkgs.less
    pkgs.which
    pkgs.wget
    pkgs.curl
    pkgs.jq
    pkgs.nix
    pkgs.procps
    pkgs.diffutils
    pkgs.gnutar
    pkgs.gzip
    pkgs.unzip
    pkgs.shfmt
    pkgs.shellcheck
    pkgs.python3
  ],
  extraDevTools ? [ ],
  fwdEnv ? [
    "TERM"
    "COLORTERM"
    "LANG"
    "LC_ALL"
    "EDITOR"
    "VISUAL"
  ],
  extraFwdEnv ? [ ],
  # NOTE: in addition to `fwdEnv` + `extraFwdEnv`, every wrapper inherits the
  # shared `myconfig.ai.jail.fwdEnvs` list (read from `osconfig`); see
  # `globalFwdEnvs` in the `let` below. Use `extraFwdEnv` only for
  # wrapper-specific variables.

  # Extra environment variables to set *inside* the jail, as a `name = value`
  # attrset. Unlike `fwdEnv`/`extraFwdEnv` (which forward a variable from the
  # host environment only if it is set), these are set unconditionally via the
  # `set-env` combinator (`--setenv`), independent of the host environment.
  # Useful for marking a particular wrapper so that tools inside the jail can
  # detect how they were launched (e.g. `PI_JAIL_MARKER = "1"`).
  extraRuntimeEnv ? { },

  # Read-only host paths discovered at runtime from environment
  # variables. Each entry is the name of an env var whose value (when set
  # and non-empty) is treated as an absolute host path to bind read-only
  # into the jail at the same path. The bind uses `--ro-bind-try` so a
  # missing path is skipped silently instead of aborting the jail.
  #
  # This is the mechanism the `*-worktree` wrapper scripts use to make the
  # *original* git repository (the worktree's linked main repo) visible
  # read-only inside the jail, so git operations against the worktree can
  # resolve the shared `.git` object store, refs and config. The env var
  # is set by the shell wrapper before exec'ing the jailed binary, so the
  # path is only bound when the wrapper actually created a worktree.
  #
  # Set to `[ ]` (the default) for wrappers that have no runtime-discovered
  # host paths to expose.
  extraReadOnlyEnvPaths ? [ ],

  # Read-write host paths discovered at runtime from environment variables.
  # Each entry is the name of an env var whose non-empty value is treated as
  # an absolute host path and bind-mounted read-write at the same path. Unlike
  # `extraReadOnlyEnvPaths`, the bind uses `--bind`: callers must only set the
  # variable to an existing path.
  #
  # This is intentionally separate from `extraReadOnlyEnvPaths` so callers
  # can expose a mostly read-only tree and remount only a nested subpath
  # writable (for example, a linked worktree's shared `.git` directory).
  extraReadWriteEnvPaths ? [ ],

  # Name of the environment variable (default `JAIL_EXTRA_RO_PATHS`)
  # whose colon-separated value lists host paths to bind read-only into
  # the jail at runtime. Each existing entry is bound via
  # `--ro-bind-try` (missing entries are skipped silently). This is the
  # interactive escape hatch for exposing extra directories without
  # rebuilding. The jail deliberately does **not** bind `/run` (which would
  # expose the host's D-Bus/PipeWire/Wayland/tmux sockets and the ssh-agent,
  # gpg-agent and keyring credential sockets under `/run/user/<uid>/`), so
  # this is also the way to re-expose a specific host runtime path on
  # demand — e.g.:
  #
  #   JAIL_EXTRA_RO_PATHS=/etc:/run/user/1000 jailed-pi
  #
  # Entries should be absolute paths; relative paths are rejected by
  # bubblewrap. Set to `null` to disable the mechanism for this wrapper.
  extraReadOnlyPathsEnvVar ? "JAIL_EXTRA_RO_PATHS",

  # Same as `extraReadOnlyPathsEnvVar` but binds read-write (`--bind`).
  # Emitted *after* the read-only block so a writable entry overrides an
  # earlier read-only parent bind. Defaults to `JAIL_EXTRA_RW_PATHS`;
  # set to `null` to disable.
  extraReadWritePathsEnvVar ? "JAIL_EXTRA_RW_PATHS",

  extraPermissions ? [ ],
  bindFullNixStore ? true,
  bindUsrBin ? true,
  persistentTmp ? true,
  bindUserTmp ? true,

  # When true (the default), refuse to start if the working directory is
  # $HOME. Otherwise `mount-cwd` would bind-mount the *entire* home
  # directory read-write into the jail, exposing every file (including
  # secrets and other agents' state) to the agent. The `*-tmp` and
  # `*-worktree` wrapper variants are unaffected because they `cd` away
  # from $HOME before exec'ing the jailed binary.
  rejectHomeCwd ? true,
}:

let
  jailLib = jail.init pkgs;

  inherit (jailLib.combinators)
    network
    time-zone
    no-new-session
    ro-bind
    try-ro-bind
    rw-bind
    add-runtime
    add-pkg-deps
    set-env
    try-fwd-env
    mount-cwd
    noescape
    ;

  userDataPerms = lib.concatMap (dir: [
    (add-runtime "mkdir -p ~/${dir}")
    (rw-bind (noescape "~/${dir}") (noescape "~/${dir}"))
  ]) userDataDirs;

  userDataFilePerms = lib.concatMap (file: [
    (add-runtime "touch ~/${file}")
    (rw-bind (noescape "~/${file}") (noescape "~/${file}"))
  ]) userDataFiles;

  configDirPerms = lib.map (dir: try-ro-bind (noescape "~/${dir}") (noescape "~/${dir}")) (
    configDirs ++ extraConfigDirs
  );

  # Environment variables forwarded from the host into *every* jail-app
  # wrapper. `OPENAI_API_KEY` is always forwarded; the shared
  # `myconfig.ai.jail.fwdEnvs` option extends this base list so that
  # adding a new wrapper requires no per-call wiring. `or [ ]` makes this
  # safe when `osconfig` is `{}` (standalone library use).
  globalFwdEnvs = [ "OPENAI_API_KEY" ] ++ (osconfig.myconfig.ai.jail.fwdEnvs or [ ]);

  fwdEnvPerms = lib.map try-fwd-env (fwdEnv ++ extraFwdEnv ++ globalFwdEnvs);

  # Shared sandbox tooling (see ../myconfig.ai.sandboxTools.nix): packages
  # appended to the tool set below for EVERY `jail-app` wrapper, and env
  # vars set unconditionally via `set-env`. Wrapper-specific
  # `extraRuntimeEnv` wins over `sharedEnv` on a name clash.
  sharedTools = osconfig.myconfig.ai.sandboxTools.extraPackages or [ ];
  sharedEnv = osconfig.myconfig.ai.sandboxTools.extraEnv or { };

  runtimeEnvPerms = lib.mapAttrsToList (name: value: set-env name value) (
    sharedEnv // extraRuntimeEnv
  );

  # Bind host paths named by env vars read-only at runtime. Each `var` is
  # the name of an environment variable; when it is set and non-empty the
  # value is treated as an absolute host path and bound read-only into the
  # jail at the same path via `--ro-bind-try` (missing path → skipped). The
  # interpolation `${var}` expands to the variable name, and `''${var}` is
  # emitted literally so bash expands it at runtime.
  runtimeReadOnlyEnvPathPerms = lib.map (
    var:
    add-runtime ''
      if [ -n "''${${var}:-}" ]; then
        RUNTIME_ARGS+=(--ro-bind-try "''${${var}}" "''${${var}}")
      fi''
  ) extraReadOnlyEnvPaths;

  # Bind host paths named by env vars read-write at runtime. These permissions
  # deliberately follow `runtimeReadOnlyEnvPathPerms` below so a writable
  # nested bind overrides an earlier read-only parent bind.
  runtimeReadWriteEnvPathPerms = lib.map (
    var:
    add-runtime ''
      if [ -n "''${${var}:-}" ]; then
        RUNTIME_ARGS+=(--bind "''${${var}}" "''${${var}}")
      fi''
  ) extraReadWriteEnvPaths;

  # Colon-separated host paths from a single env var (default
  # `JAIL_EXTRA_RO_PATHS`), bound read-only at runtime. This is the
  # interactive escape hatch — set the env var at the shell prompt to
  # expose extra host directories without rebuilding.
  runtimeReadOnlyPathsPerm = lib.optional (extraReadOnlyPathsEnvVar != null) (add-runtime ''
    if [ -n "''${${extraReadOnlyPathsEnvVar}:-}" ]; then
      IFS=':' read -ra _ro_paths <<< "''${${extraReadOnlyPathsEnvVar}}"
      for _p in "''${_ro_paths[@]}"; do
        if [ -n "$_p" ] && [ -e "$_p" ]; then
          RUNTIME_ARGS+=(--ro-bind-try "$_p" "$_p")
        fi
      done
    fi'');

  runtimeReadWritePathsPerm = lib.optional (extraReadWritePathsEnvVar != null) (add-runtime ''
    if [ -n "''${${extraReadWritePathsEnvVar}:-}" ]; then
      IFS=':' read -ra _rw_paths <<< "''${${extraReadWritePathsEnvVar}}"
      for _p in "''${_rw_paths[@]}"; do
        if [ -n "$_p" ] && [ -e "$_p" ]; then
          RUNTIME_ARGS+=(--bind "$_p" "$_p")
        fi
      done
    fi'');

  permissions =
    lib.optional rejectHomeCwd (
      # Refuse to start when the working directory is $HOME. Otherwise
      # mount-cwd would bind-mount the *entire* home directory read-write
      # into the jail, exposing every file (including secrets and other
      # agents' state) to the agent. Run from a project subdirectory.
      add-runtime ''
        if [ "$PWD" = "$HOME" ]; then
          echo "${name}: refusing to run in home directory ($HOME): the working directory would expose your entire home directory to the agent." >&2
          echo "${name}: run from a project subdirectory instead." >&2
          exit 1
        fi
      ''
    )
    ++ [
      # Network access for talking to LLM endpoints, including TLS/CA bundle
      # and /etc/resolv.conf etc.
      network

      # Expose the host's timezone (binds /etc/localtime) so timestamps,
      # git commits and the agent's notion of "now" match the host.
      time-zone

      # Drop bwrap's `--new-session` flag. With --new-session, the jailed
      # process is detached from the controlling TTY which breaks signal
      # handling (Ctrl-C) and some TUI features in interactive agents.
      # See BWRAP(1) for security trade-offs.
      no-new-session
    ]
    ++ lib.optional bindFullNixStore (
      # Bind the entire `/nix/store` read-only. The base permissions only
      # bind the runtime closure of the jailed derivation; agents shell out
      # to arbitrary tools (git, ripgrep, ...) added via add-pkg-deps and
      # may also exec store paths discovered in the user's project (e.g.
      # `nix run`, `direnv`, etc.), so we expose the full store instead.
      ro-bind "/nix/store" "/nix/store"
    )
    ++ userDataPerms
    ++ userDataFilePerms
    ++ lib.optionals bindUserTmp [
      # Expose the host's `~/tmp` directory read-write inside the jail so
      # the agent has a persistent writable scratch space under $HOME.
      (add-runtime "mkdir -p ~/tmp")
      (rw-bind (noescape "~/tmp") (noescape "~/tmp"))
    ]
    ++ lib.optional persistentTmp (
      # Provide a host-backed /tmp instead of the base tmpfs. Creates
      # /tmp/<name> on the host and bind-mounts it as /tmp in the jail,
      # giving the agent a real writable /tmp that survives across
      # invocations.
      add-runtime ''
        mkdir -p /tmp/${name}
        RUNTIME_ARGS+=(--bind /tmp/${name} /tmp)
      ''
    )
    ++ [
      # Bind-mount the working directory read-write so the agent can edit
      # files in the user's project. The CWD is typically the project root
      # (or a worktree, when invoked via a *-worktree wrapper).
      mount-cwd
    ]
    ++ lib.optional bindUsrBin (
      # Expose `/usr/bin` read-only so the agent can inspect host-installed
      # binaries (e.g. `which`, `file`, or system-provided tools outside
      # the Nix store).
      ro-bind "/usr/bin" "/usr/bin"
    )
    ++ [
      # NOTE: `/run` is intentionally *not* bound into the jail. A blanket
      # `/run` ro-bind would expose the host's D-Bus, PipeWire/PulseAudio,
      # Wayland and tmux sockets and — under `/run/user/<uid>/` — the
      # ssh-agent, gpg-agent (`gnupg/`) and keyring credential sockets, none
      # of which a coding agent needs (and a read-only bind does *not*
      # protect a unix socket: `connect()` still works). DNS/TLS is provided
      # by the `network` combinator (which binds `/etc/resolv.conf`,
      # `/etc/ssl` and `/run/systemd/resolve` itself) and the nix daemon
      # socket lives under `/nix/var/nix` (bound below), so nothing here
      # depends on a blanket `/run` bind. To re-expose a specific runtime
      # path on demand use the `JAIL_EXTRA_RO_PATHS` escape hatch, e.g.
      # `JAIL_EXTRA_RO_PATHS=/run/user/1000 jailed-pi`.

      # Expose `/etc/nix/nix.conf` read-only so that `nix` commands inside
      # the jail pick up the host's Nix configuration (substituters,
      # trusted-users, experimental-features, etc.).
      (try-ro-bind "/etc/nix/nix.conf" "/etc/nix/nix.conf")
    ]
    ++ [
      # Expose `/nix/var/nix` read-only so that `nix` commands inside
      # the jail can find the store database and don't fall back to the
      # multi-user chroot store path (which produces a warning).
      (try-ro-bind "/nix/var/nix" "/nix/var/nix")
    ]
    ++ configDirPerms
    ++ [
      (add-pkg-deps (devTools ++ extraDevTools ++ sharedTools))
    ]
    ++ fwdEnvPerms
    ++ runtimeEnvPerms
    ++ runtimeReadOnlyEnvPathPerms
    ++ runtimeReadWriteEnvPathPerms
    ++ runtimeReadOnlyPathsPerm
    ++ runtimeReadWritePathsPerm
    ++ extraPermissions;
in
jailLib name pkg permissions
