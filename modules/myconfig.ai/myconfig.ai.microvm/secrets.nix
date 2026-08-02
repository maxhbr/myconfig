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
# below (root-owned, 0400). The host launcher (launcher.nix) then defaults
# `AGENT_MICROVM_SSH_KEY` to that path when the caller did not set one, so the
# `run --attach` / `ssh` readiness paths — which run as root under `sudo` and
# therefore lose any user-set `AGENT_MICROVM_SSH_KEY` (sudo `env_reset`) —
# find the dedicated key automatically. See ./docs/agent-microvm.md.
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
    };
  };
}
