# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.microvm — deterministic PER-SLOT SSH host identities and the
# host-side known_hosts file (improvement ticket 3 B).
#
# Problem
# -------
# Before this, every slot booted with a freshly generated, throwaway SSH host
# key, so the launcher had to connect with `StrictHostKeyChecking=no` and
# `UserKnownHostsFile=/dev/null`: an UNAUTHENTICATED control channel. Per-TAP
# L2 isolation (ticket 3 A) removes the co-resident-guest MITM, but the
# operator still had no way to tell "the slot I started" from "something else
# answering on that address".
#
# Design
# ------
# One STABLE ed25519 host key per predeclared slot, generated ON THE HOST at
# runtime and never in the Nix store:
#
#   ${runtimeRoot}/hostkeys/<slot>/ssh_host_ed25519_key      root:root 0400
#   ${runtimeRoot}/hostkeys/<slot>/ssh_host_ed25519_key.pub  root:root 0444
#   ${runtimeRoot}/known_hosts                               root:root 0444
#
# Why not the Nix store: a store path is world-readable on the host, so every
# local user could impersonate a slot. Why not agenix: these are per-slot,
# host-local, regenerable identities — not shared secrets that belong in the
# priv repo. Why not `microvm.credentialFiles`: cloud-hypervisor does not
# support it (microvm.nix throws), and switching hypervisor is out of scope.
#
# Delivery to the guest is the per-slot READ-ONLY virtiofs share (guest.nix
# declares it), in which the key directory is the `hostkeys/` subdirectory.
# virtiofsd passes ownership through unchanged, so inside the guest the private
# key stays root:root 0400: the unprivileged, untrusted `agent` user cannot
# read it, and no other slot's directory is exposed. This is a deliberate,
# documented amendment to the original "EXACTLY ONE share" rule (plan §10) —
# the second share is read-only, root-only, single-purpose and per-slot.
#
# The public keys are collected into ONE host-side `known_hosts` file keyed by
# the slot's deterministic IPv4 address, which the launcher uses with
# `StrictHostKeyChecking=yes` (launcher.nix). It is world-readable on purpose:
# a non-root operator running `agent-microvm ssh` must be able to read it, and
# it contains only public keys.
#
# Everything is gated on `cfg.enable`, so a disabled feature produces no unit,
# no directory and no key.
{
  config,
  lib,
  pkgs,
  # The effective resource-class table (see default.nix).
  agentResourceClasses,
  # The ONE definition of the per-session tree (./session.nix): the per-slot
  # host identity lives in the `hostkeys/` subdirectory of the ONE READ-ONLY
  # share. It is deliberately NEVER placed in the writable session tree (the
  # plan says so explicitly, and ./session.nix's verifier refuses to launch a
  # slot whose writable tree contains key material).
  agentSession,
  ...
}:
let
  cfg = config.myconfig.ai.microvm;
  session = agentSession;

  # The slot pool of the effective resource classes (ticket 5 A). The class
  # table comes from default.nix (`_module.args.agentResourceClasses`), which
  # also performs the legacy `slotCount` migration, so every module builds the
  # SAME pool.
  slots = (import ./slots.nix { inherit lib; }).mkSlots agentResourceClasses;

  # --- the ONE definition of every host-key path ------------------------
  # Exported as a module argument (see `_module.args.agentHostKeys` below) so
  # guest.nix (which mounts the share and points sshd at the key) and
  # launcher.nix (which passes the known_hosts file to ssh) cannot drift from
  # the generator below.
  hostKeys = rec {
    # Host-side root of the per-slot key directories: the read-only session
    # tree.
    root = session.roRoot;
    slotDir = slotName: session.hostHostkeysDir slotName;
    # Host-side aggregated known_hosts consumed by the launcher.
    knownHosts = "${cfg.runtimeRoot}/known_hosts";
    # Guest-side mount point of the per-slot read-only share, its virtiofs tag
    # and the key file name (identical on host and guest).
    keyName = "ssh_host_ed25519_key";
    guestMountPoint = session.guestHostkeysDir;
    guestTag = "hostkey";
    guestKeyPath = "${guestMountPoint}/${keyName}";
  };

  # --- generator ---------------------------------------------------------
  # Idempotent: an existing key is kept (so a slot's identity is STABLE across
  # host reboots and rebuilds), a missing one is created. known_hosts is
  # rebuilt from the public keys on every run and installed atomically, so a
  # slot-count change or a manually deleted key heals on the next start.
  provisionHostKeys = pkgs.writeShellApplication {
    name = "agent-microvm-provision-hostkeys";
    runtimeInputs = with pkgs; [
      coreutils
      openssh
    ];
    text = ''
      set -euo pipefail

      root=${lib.escapeShellArg hostKeys.root}
      known_hosts=${lib.escapeShellArg hostKeys.knownHosts}
      key_name=${lib.escapeShellArg hostKeys.keyName}

      install -d -m 0700 -o root -g root -- "$root"

      tmp="$(mktemp "$known_hosts.XXXXXX")"
      trap 'rm -f -- "$tmp"' EXIT
      {
        echo "# Generated by agent-microvm-hostkeys.service — do not edit."
        echo "# One deterministic ed25519 host identity per microVM slot."
      } > "$tmp"

      ${lib.concatMapStringsSep "\n" (slot: ''
        slot_dir=${lib.escapeShellArg (hostKeys.slotDir slot.name)}
        install -d -m 0700 -o root -g root -- "$slot_dir"
        if [[ ! -s "$slot_dir/$key_name" ]]; then
            # Remove a half-written pair before regenerating, so ssh-keygen
            # never prompts for overwrite confirmation.
            rm -f -- "$slot_dir/$key_name" "$slot_dir/$key_name.pub"
            ssh-keygen -q -t ed25519 -N "" \
                -C ${lib.escapeShellArg "agent-microvm-${slot.name}"} \
                -f "$slot_dir/$key_name"
        fi
        # virtiofsd passes ownership through unchanged, so these host modes are
        # exactly what the guest sees: only guest root may read the private key.
        chown root:root -- "$slot_dir/$key_name" "$slot_dir/$key_name.pub"
        chmod 0400 -- "$slot_dir/$key_name"
        chmod 0444 -- "$slot_dir/$key_name.pub"
        printf '%s %s\n' ${lib.escapeShellArg slot.ip} \
            "$(cut -d" " -f1,2 -- "$slot_dir/$key_name.pub")" >> "$tmp"
      '') slots}

      # Public keys only → world-readable, so a non-root operator can run
      # `agent-microvm ssh` with StrictHostKeyChecking=yes.
      chmod 0444 -- "$tmp"
      mv -f -- "$tmp" "$known_hosts"
      trap - EXIT
    '';
    meta = with lib; {
      description = "Provision per-slot SSH host keys + known_hosts for myconfig.ai.microvm";
      platforms = platforms.linux;
    };
  };
in
{
  config = lib.mkMerge [
    # Path helpers are exported unconditionally (laziness means they cost
    # nothing while the feature is disabled), so the consuming modules can
    # take them as a module argument just like `agentRegistry`.
    { _module.args.agentHostKeys = hostKeys; }

    (lib.mkIf cfg.enable {
      # The runtime root must be traversable so a non-root operator can read
      # the world-readable known_hosts; the key directories stay 0700. Those
      # are part of the read-only session tree, which ./session.nix creates from
      # the ONE layout table (root-only 0700, per slot) — so only the runtime
      # root itself is emitted here.
      systemd.tmpfiles.rules = [
        "d ${cfg.runtimeRoot} 0755 root root - -"
      ];

      systemd.services.agent-microvm-hostkeys = {
        description = "Provision per-slot SSH host keys for agent microVMs";
        wantedBy = [ "multi-user.target" ];
        after = [ "systemd-tmpfiles-setup.service" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          ExecStart = lib.getExe provisionHostKeys;
        };
      };
    })
  ];
}
