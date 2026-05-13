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
#
# The resulting derivation is a `jail` permission bundle. See
# `vendor/alexdavid-jail.nix/lib/combinators/` for available primitives.
{
  lib,
  pkgs,
  jail,
}:

{
  # Required: unique name of the resulting wrapper binary.
  name,

  # Required: the package whose `lib.getExe pkg` is jailed.
  pkg,

  # Writable home directories for the agent's own state/config (e.g.
  # [ ".pi" ] for pi, [ ".claude" ".claude.json" ] for claude-code). Each
  # entry is interpreted relative to $HOME and rw-bound into the jail. The
  # helper creates the host-side directory if it does not exist.
  userDataDirs ? [ ],

  # Read-only host config directories (relative to $HOME) that tools inside
  # the jail should pick up. Override to replace; use `extraConfigDirs` to
  # append.
  configDirs ? [
    ".config/nix"
    ".config/nixpkgs"
    ".config/pistol"
    ".config/ripgrep"
    ".config/bat"
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
  ],
  extraDevTools ? [ ],

  # Environment variables forwarded from the host if set.
  fwdEnv ? [
    "TERM"
    "COLORTERM"
    "LANG"
    "LC_ALL"
    "EDITOR"
    "VISUAL"
  ],
  extraFwdEnv ? [ ],

  # Escape hatch: arbitrary extra jail.combinators permissions appended at
  # the end (e.g. additional rw-binds, set-env, add-runtime). Callers can
  # build these with `jail.init pkgs` if they need combinator helpers
  # themselves; otherwise pass raw permissions.
  extraPermissions ? [ ],

  # Toggle the full /nix/store ro-bind. Defaults to true because agents
  # routinely shell out to arbitrary store paths (nix run, direnv, store
  # paths embedded in project files). Disable only if you want strict
  # closure-based isolation.
  bindFullNixStore ? true,

  # Toggle the /usr/bin ro-bind. Useful for tools that look up host-only
  # binaries via PATH (e.g. `which`). Defaults to true to match the
  # existing pi-coding-agent behaviour.
  bindUsrBin ? true,

  # Toggle persistent host-backed /tmp at /tmp/<name>. When true (default),
  # the host directory `/tmp/<name>` is created and bind-mounted as /tmp
  # inside the jail so scratch files survive across invocations.
  persistentTmp ? true,

  # Toggle ~/tmp rw-bind. Defaults to true.
  bindUserTmp ? true,
}:

let
  jailLib = jail.init pkgs;

  inherit (jailLib.combinators)
    network
    time-zone
    no-new-session
    ro-bind
    rw-bind
    add-runtime
    add-pkg-deps
    try-fwd-env
    mount-cwd
    noescape
    ;

  userDataPerms = lib.concatMap (dir: [
    (add-runtime "mkdir -p ~/${dir}")
    (rw-bind (noescape "~/${dir}") (noescape "~/${dir}"))
  ]) userDataDirs;

  configDirPerms = lib.map (dir: ro-bind (noescape "~/${dir}") (noescape "~/${dir}")) (
    configDirs ++ extraConfigDirs
  );

  fwdEnvPerms = lib.map try-fwd-env (fwdEnv ++ extraFwdEnv);

  permissions = [
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
  ++ configDirPerms
  ++ [
    (add-pkg-deps (devTools ++ extraDevTools))
  ]
  ++ fwdEnvPerms
  ++ extraPermissions;
in
jailLib name pkg permissions
