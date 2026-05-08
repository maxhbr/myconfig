# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# Standalone option declarations for `myconfig.wireguard.<wgInterface>`.
#
# The actual implementation (peer list, secrets, systemd units for the
# roaming probe) lives in `flake.lib.nix:setupAsWireguardClient`, which
# is invoked from a per-host module (typically in the private repo).
# Options live here so hosts can set them whether or not
# `setupAsWireguardClient` ends up importing on this host (e.g. during
# `nix flake check` of the public repo, where the private wg client
# wiring isn't loaded).

{ lib, ... }:
{
  options.myconfig.wireguard = lib.mkOption {
    type = lib.types.attrsOf (
      lib.types.submodule (
        { name, ... }:
        {
          options.roaming = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = ''
              Mark this host as a roaming WireGuard client (no stable LAN
              identity, e.g. a laptop or phone). Same-LAN peers are
              emitted as "ghost" peers in the static config (no endpoint,
              empty allowedIPs), so off-LAN all wg-subnet traffic relays
              via the rendezvous host (vserver). A `wg-roaming-${name}`
              systemd service detects when the host is back on the home
              LAN and patches in direct LAN endpoints + per-peer /32
              allowedIPs at runtime, restoring the LAN optimization.
            '';
          };
        }
      )
    );
    default = { };
    description = "Per-interface WireGuard client behavior.";
  };
}
