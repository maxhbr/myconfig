# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Impure per-invocation runner entry point. The host wrappers select a runner
# with QEMU_AGENT_SANDBOX_KIND and pass launch-specific paths through the
# corresponding environment variables.
{
  nixpkgs,
  nixosSystem,
  system,
  microvmModule,
  seedAgentConfig,
  piPackage,
  herdrPackage,
  workmuxPackage,
}:
let
  lib = nixpkgs.lib;
  pkgs = nixpkgs.legacyPackages.${system};
  builders = import ./builders.nix {
    inherit
      nixpkgs
      nixosSystem
      microvmModule
      seedAgentConfig
      ;
  };

  getEnvOr =
    name: fallback:
    let
      value = builtins.getEnv name;
    in
    if value == "" then fallback else value;

  requireEnv =
    name:
    let
      value = builtins.getEnv name;
    in
    if value == "" then throw "qemu-agent-sandbox runner requires ${name}" else value;

  intEnvOrNull =
    name:
    let
      value = builtins.getEnv name;
    in
    if value == "" then null else lib.toInt value;

  jsonEnv = name: builtins.fromJSON (getEnvOr name "[]");
  kind = requireEnv "QEMU_AGENT_SANDBOX_KIND";
in
if kind == "pi" then
  builders.mkAgentQemuPiRunner {
    inherit system piPackage;
    workspace = requireEnv "AGENT_QEMU_PI_WORKSPACE";
    sshPort = lib.toInt (getEnvOr "AGENT_QEMU_PI_SSH_PORT" "2222");
    authorizedKeysFile = getEnvOr "AGENT_QEMU_PI_AUTHORIZED_KEYS" "/var/empty/authorized_keys";
    allowNetwork = getEnvOr "AGENT_QEMU_PI_NETWORK" "1" != "0";
    extraGuestPackagePaths = jsonEnv "AGENT_QEMU_PI_EXTRA_PACKAGES";
  }
else if kind == "herdr" then
  builders.mkAgentQemuHerdrRunner {
    inherit system herdrPackage;
    workspace = requireEnv "AGENT_QEMU_HERDR_WORKSPACE";
    sshPort = lib.toInt (getEnvOr "AGENT_QEMU_HERDR_SSH_PORT" "2222");
    authorizedKeysFile = getEnvOr "AGENT_QEMU_HERDR_AUTHORIZED_KEYS" "/var/empty/authorized_keys";
    allowNetwork = getEnvOr "AGENT_QEMU_HERDR_NETWORK" "1" != "0";
    hostUid = intEnvOrNull "AGENT_QEMU_HERDR_UID";
    hostGid = intEnvOrNull "AGENT_QEMU_HERDR_GID";
    agentPackages = jsonEnv "AGENT_QEMU_HERDR_AGENT_PACKAGES";
    extraGuestPackagePaths = jsonEnv "AGENT_QEMU_HERDR_EXTRA_PACKAGES";
  }
else if kind == "workmux" then
  builders.mkSandboxedWorkmuxRunner {
    inherit system piPackage workmuxPackage;
    workspace = requireEnv "SANDBOXED_WORKMUX_REPO";
    worktrees = getEnvOr "SANDBOXED_WORKMUX_WORKTREES" (requireEnv "SANDBOXED_WORKMUX_REPO");
    sshPort = lib.toInt (getEnvOr "SANDBOXED_WORKMUX_SSH_PORT" "2222");
    authorizedKeysFile = getEnvOr "SANDBOXED_WORKMUX_AUTHORIZED_KEYS" "/var/empty/authorized_keys";
    workmuxConfigFile = getEnvOr "SANDBOXED_WORKMUX_CONFIG" "/var/empty/config.yaml";
    tmuxConf = getEnvOr "SANDBOXED_WORKMUX_TMUXCONF" "";
    allowNetwork = getEnvOr "SANDBOXED_WORKMUX_NETWORK" "1" != "0";
    extraGuestPackagePaths = jsonEnv "SANDBOXED_WORKMUX_EXTRA_PACKAGES";
  }
else
  throw "unknown QEMU_AGENT_SANDBOX_KIND: ${kind} (expected pi, herdr, or workmux)"
