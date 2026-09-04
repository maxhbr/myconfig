# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Reusable wrapper around the nono sandboxing tool for jailing LLM
# coding agents (pi, opencode, claude-code, gemini-cli, ...).
#
# Usage:
#
#   let
#     callLib = file: import file { inherit lib pkgs; };
#     nono-app = callLib ../fns/nono-app.nix;
#   in
#   nono-app {
#     name = "agent-nono-pi";
#     pkg = pkgs.pi-coding-agent;
#     userDataDirs = [ ".pi" ];
#   }
#
# All "default" lists (configDirs, devTools, fwdEnv, userDataDirs) can be
# either replaced wholesale or extended via the matching `extra*` argument.
# In addition, every wrapper inherits `myconfig.ai.nono.fwdEnvs` from the
# NixOS config passed in as `osconfig` — see `globalFwdEnvs` below. This
# extends the always-forwarded `OPENAI_API_KEY`. The shared sandbox tool
# option `myconfig.ai.sandboxTools` (packages + env) is inherited the
# same way — see `sharedTools`/`sharedEnv` below.
#
# The resulting derivation is a shell script that wraps the command in
# `nono run` with appropriate flags. See
# https://nono.sh/docs for available flags and profiles.
{
  lib,
  pkgs,
  # The NixOS `config`, used to read the shared `myconfig.ai.nono.fwdEnvs`
  # option (see `../myconfig.ai.nono.nix`) so every `nono-app` wrapper picks
  # up the global forwarded-env list without each call site having to
  # pass it explicitly. Defaults to `{}` so the library still works standalone
  # (e.g. in `nix repl`); in that case `globalFwdEnvs` reduces to just the
  # always-forwarded `OPENAI_API_KEY`.
  osconfig ? { },
}:

{
  # Required: unique name of the resulting wrapper binary.
  name,

  # Required: the package whose `lib.getExe pkg` is run in the sandbox.
  pkg,

  # Writable home directories for the agent's own state/config (e.g.
  # [ ".pi" ] for pi, [ ".claude" ] for claude-code). Each entry is
  # interpreted relative to $HOME and made writable in the sandbox.
  # The helper creates the host-side directory with `mkdir -p` if it
  # does not exist. For single-file state (e.g. ".claude.json") use
  # `userDataFiles` instead.
  userDataDirs ? [ ],

  # Writable single files in the home directory for the agent's state.
  # Use this instead of `userDataDirs` for entries that are files rather
  # than directories (e.g. ".claude.json"). The helper `touch`es the
  # host-side file so it exists for the sandbox.
  userDataFiles ? [ ],

  # Read-only host config directories (relative to $HOME) that tools inside
  # the sandbox should pick up. Each entry is bound read-only only if it
  # exists on the host; a missing directory is skipped silently.
  # Override to replace; use `extraConfigDirs` to append.
  configDirs ? [
    ".config/pistol"
    ".config/ripgrep"
    ".config/bat"
    ".config/git"
  ],
  extraConfigDirs ? [ ],

  # Developer tools made available inside the sandbox. Mirrors
  # the jail-app.nix commonPkgs plus extras (fd, ripgrep, less, wget,
  # curl, jq, nix) we use everywhere. Override to replace; use
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

  # Environment variables forwarded from the host into the sandbox.
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
  # shared `myconfig.ai.nono.fwdEnvs` list (read from `osconfig`); see
  # `globalFwdEnvs` in the `let` below. Use `extraFwdEnv` only for
  # wrapper-specific variables.

  # Extra environment variables to set *inside* the sandbox, as a
  # `name = value` attrset. Unlike `fwdEnv`/`extraFwdEnv` (which forward
  # a variable from the host environment only if it is set), these are
  # set unconditionally. Useful for marking a particular wrapper so that
  # tools inside the sandbox can detect how they were launched (e.g.
  # `NONO_SANDBOX_WRAPPER = "1"`).
  extraRuntimeEnv ? { },

  # Directories to allow read+write access to (passed to `--allow`).
  # These are in addition to the CWD and home directory state dirs.
  extraAllowDirs ? [ ],

  # Directories to allow read-only access to (passed to `--read`).
  extraReadOnlyDirs ? [ ],

  # Unix sockets to allow connect() to (passed to `--allow-unix-socket`).
  extraUnixSockets ? [ ],

  # TCP ports to allow outbound connect to (passed to `--allow-connect-port`).
  extraConnectPorts ? [ ],

  # TCP ports to allow listen on (passed to `--listen-port`).
  extraListenPorts ? [ ],

  # Domains to allow through the proxy (passed to `--allow-domain`).
  extraAllowDomains ? [ ],

  # Credential services to inject (passed to `--credential`).
  extraCredentials ? [ ],

  # nono profile to use (passed to `--profile`). Use "default" for the
  # conservative base profile, or specify a custom profile path.
  profile ? "default",

  # Extra nono flags to append to the `nono run` command.
  extraNonoFlags ? [ ],

  # When true (the default), refuse to start if the working directory is
  # $HOME. Otherwise the sandbox would expose your entire home directory
  # to the agent. The `*-worktree` wrapper variants are unaffected because
  # they `cd` away from $HOME before exec'ing the sandboxed binary.
  rejectHomeCwd ? true,
}:

let
  # Environment variables forwarded from the host into *every* nono-app
  # wrapper. `OPENAI_API_KEY` is always forwarded; the shared
  # `myconfig.ai.nono.fwdEnvs` option extends this base list so that
  # adding a new wrapper requires no per-call wiring. `or [ ]` makes this
  # safe when `osconfig` is `{}` (standalone library use).
  globalFwdEnvs = [ "OPENAI_API_KEY" ] ++ (osconfig.myconfig.ai.nono.fwdEnvs or [ ]);

  allFwdEnvs = fwdEnv ++ extraFwdEnv ++ globalFwdEnvs;

  # Shared sandbox tooling (see ../myconfig.ai.sandboxTools.nix): packages
  # appended to the tool set below for EVERY `nono-app` wrapper, and env
  # vars set unconditionally. Wrapper-specific `extraRuntimeEnv` wins over
  # `sharedEnv` on a name clash.
  sharedTools = osconfig.myconfig.ai.sandboxTools.extraPackages or [ ];
  sharedEnv = osconfig.myconfig.ai.sandboxTools.extraEnv or { };

  allRuntimeEnv = sharedEnv // extraRuntimeEnv;

  userDataPerms = lib.concatMapStringsSep "\n    " (dir: ''mkdir -p "$HOME/${dir}"'') userDataDirs;

  userDataFilePerms = lib.concatMapStringsSep "\n    " (
    file: ''touch "$HOME/${file}"''
  ) userDataFiles;

  configDirChecks = lib.concatMapStringsSep "\n    " (
    dir: ''[ -d "$HOME/${dir}" ] && echo "Config dir: $HOME/${dir}"''
  ) (configDirs ++ extraConfigDirs);

  # Build the nono run flags
  allowDirFlags = lib.concatMapStringsSep " " (dir: "--allow \"$HOME/${dir}\"") (
    userDataDirs ++ extraAllowDirs
  );

  # Read-only directories: user config dirs plus the full Nix store
  # (required so the sandboxed agent binary can be executed).
  readOnlyDirFlags =
    lib.concatMapStringsSep " " (dir: "--read \"$HOME/${dir}\"") (
      configDirs ++ extraConfigDirs ++ extraReadOnlyDirs
    )
    + " --read /nix/store";

  unixSocketFlags = lib.concatMapStringsSep " " (
    socket: "--allow-unix-socket \"${socket}\""
  ) extraUnixSockets;

  connectPortFlags = lib.concatMapStringsSep " " (
    port: "--allow-connect-port ${toString port}"
  ) extraConnectPorts;

  listenPortFlags = lib.concatMapStringsSep " " (
    port: "--listen-port ${toString port}"
  ) extraListenPorts;

  allowDomainFlags = lib.concatMapStringsSep " " (
    domain: "--allow-domain \"${domain}\""
  ) extraAllowDomains;

  credentialFlags = lib.concatMapStringsSep " " (cred: "--credential \"${cred}\"") extraCredentials;

  # Build environment variable exports for forwarded vars
  fwdEnvExports = lib.concatMapStringsSep "\n    " (
    var: ''[ -n "''${${var}:-}" ] && export ${var}="''${${var}}"''
  ) allFwdEnvs;

  # Build environment variable exports for extra runtime env
  runtimeEnvExports = lib.concatStringsSep "\n    " (
    lib.mapAttrsToList (name: value: ''export ${name}="${value}"'') allRuntimeEnv
  );

in
pkgs.writeShellApplication {
  name = name;
  runtimeInputs = [
    pkgs.nono
    pkgs.bashInteractive
  ];
  text = ''
    set -euo pipefail

    # Refuse to start when the working directory is $HOME. Otherwise the
    # sandbox would expose your entire home directory to the agent. Run from
    # a project subdirectory.
    ${lib.optionalString rejectHomeCwd ''
      if [ "$PWD" = "$HOME" ]; then
        echo "${name}: refusing to run in home directory ($HOME): the working directory would expose your entire home directory to the agent." >&2
        echo "${name}: run from a project subdirectory instead." >&2
        exit 1
      fi
    ''}

    # Prepare writable state directories
    ${userDataPerms}
    ${userDataFilePerms}

    # Check config dirs exist (informational)
    ${configDirChecks}

    # Forward environment variables from host
    ${fwdEnvExports}

    # Set extra runtime environment variables
    ${runtimeEnvExports}

    # Build the nono run command
    exec ${lib.getExe pkgs.nono} run \
      --profile "${profile}" \
      ${allowDirFlags} \
      ${readOnlyDirFlags} \
      ${unixSocketFlags} \
      ${connectPortFlags} \
      ${listenPortFlags} \
      ${allowDomainFlags} \
      ${credentialFlags} \
      --allow-cwd \
      ${lib.concatStringsSep " \\\n      " extraNonoFlags} \
      -- ${lib.getExe pkg} "$@"
  '';

  meta = with lib; {
    description = "Sandboxed ${name} wrapper using nono";
    maintainers = [ ];
    platforms = platforms.linux;
  };
}
