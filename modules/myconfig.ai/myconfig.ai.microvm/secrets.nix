# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.microvm — dedicated agent-VM SSH private-key secret stub.
#
# The guest `agent` user authorises exactly one dedicated public key
# (`sshPublicKeyFile`, committed in-repo). The MATCHING PRIVATE key is not in
# this repo — it lives in the separate `../priv/` repository. This module
# declares a `myconfig.secrets` STUB for that private key with NO `source`,
# so agenix knows the dest/owner/mode but cannot yet encrypt it. The priv
# repo fills in the source, e.g.:
#
#     myconfig.secrets."dedicated-agent-vm-key".source =
#       ./secrets/dedicated-agent-vm-key;
#
# Once provisioned, agenix decrypts the private key to the stable `dest`
# below (root:root 0400 by default; root:agent-microvm 0440 when
# `passwordlessControl` is on, so the non-root operator can read it). The
# host launcher (launcher.nix) then defaults `AGENT_MICROVM_SSH_KEY` to that
# path when the caller did not set one, so the `run --attach` / `ssh` /
# `console` paths find the dedicated key automatically — whether run directly
# by a control-group member or as root under `sudo` (which loses any user-set
# `AGENT_MICROVM_SSH_KEY` via `env_reset`). See ./docs/agent-microvm.md.
#
# While the source is unset (priv repo absent), `myconfig.secrets` emits its
# standard "source is missing" warning and no key is decrypted; the launcher
# fallback simply finds no file and behaves as before (caller must pass
# AGENT_MICROVM_SSH_KEY or give root a matching key by hand).
{
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.ai.microvm;
in
{
  config = lib.mkIf (cfg.enable && cfg.enableSsh) {
    # Secret stub — source is set in priv/. The dest path is stable
    # (sshKeyDest in launcher.nix references the same literal) so the host
    # launcher always finds the decrypted key at the same location.
    myconfig.secrets."dedicated-agent-vm-key" = {
      dest = "/run/agenix/dedicated-agent-vm-key";
      # source = <set in priv/, e.g. ./secrets/dedicated-agent-vm-key>;
    }
    # When the operator drives the launcher WITHOUT sudo
    # (passwordlessControl), the non-root `ssh` / `console` / `run --attach`
    # readiness paths must be able to READ the dedicated private key. The
    # `status`/`list`/`ssh`/`console` subcommands already need no root, so the
    # ONLY thing forcing `ssh` through sudo is this key's default root:root
    # 0400 mode. Make it group-readable (0440) by the `agent-microvm` control
    # group that launcher.nix creates and adds the operator to, so
    # `agent-microvm ssh <slot>` works with no sudo at all.
    #
    # This exposes the key ONLY to the already-trusted host operator (a full
    # sudoer who owns the workspace clones and uid 1000) — NEVER to the
    # untrusted guest — so the guest/agent isolation boundary is unchanged.
    # With passwordlessControl off the key stays root:root 0400 and `ssh`
    # must run via sudo (the launcher then re-reads it as root).
    // lib.optionalAttrs cfg.passwordlessControl {
      group = "agent-microvm";
      permissions = "0440";
    };
  };
}
