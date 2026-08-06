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
  # The ONE resolved capability set (see default.nix, lightweight plan phase 5).
  # A per-slot SSH host identity exists for the `interactive` capability (the
  # TCP sshd control channel) OR the `vsock` capability (the `sshd-vsock@`
  # VSOCK control channel, phase 6): a batch-only host that selects `vsock`
  # has no TCP sshd but still needs a pinned host key so the VSOCK channel is
  # host-key-verified. The `hostkeys/` subdirectory of the read-only tree is
  # created by ../session.nix's own per-capability table for the same union,
  # so only the PROVISIONING (the key pair + the known_hosts database) is
  # gated below.
  agentCapabilities,
  # The ONE resolved network decision (see default.nix). The IPv4 `known_hosts`
  # entry below only makes sense while the guest HAS a network interface: under
  # the `vsock` transport (lightweight plan phase 6) there is no TAP and no
  # guest address, so pinning the key to an address nothing listens on would be
  # misleading dead data - the `vsock-mux/...` entry is the whole database there.
  agentNetwork,
  ...
}:
let
  cfg = config.myconfig.ai.microvm;
  session = agentSession;

  # The slot pool of the effective resource classes (ticket 5 A). The class
  # table comes from default.nix (`_module.args.agentResourceClasses`), so every
  # module builds the SAME pool.
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
    # Guest-side location of the key inside the ONE read-only share and the key
    # file name (identical on host and guest). The share itself (source, tag,
    # mount point) is declared once in ../session.nix — the host keys are just
    # one subdirectory of it, so there is deliberately no virtiofs tag here.
    keyName = "ssh_host_ed25519_key";
    # The provisioner itself, exported so the EXECUTED launcher harnesses
    # (tests/microvm-batch-launcher-submit.sh) can PLAY the provisioning unit
    # with the REAL script instead of stubbing key material into place — which
    # is what makes the launcher's identity validation / self-healing path
    # genuinely exercised rather than grep-only. Defined below; the recursive
    # attrset makes the forward reference legal.
    provisioner = provisionHostKeys;
    guestMountPoint = session.guestHostkeysDir;
    guestKeyPath = "${guestMountPoint}/${keyName}";
  };

  # --- the concurrency LOCK ----------------------------------------------
  # The provisioner mutates SHARED state (the aggregated known_hosts) and
  # per-slot key pairs, and it now has TWO callers: the boot-time unit and the
  # launcher's `systemctl restart` self-heal before a launch. Two concurrent
  # runs could therefore interleave a half-rebuilt database with a reader, or
  # race `ssh-keygen` on the same path.
  #
  # The lock lives in this feature's OWN runtime root rather than in
  # /run/lock: that directory is created by this module's tmpfiles rule (so it
  # always exists when the unit runs), it is root-owned 0755 (so no local user
  # can interfere), and it is inside the tree the executed launcher harnesses
  # already bind-mount — whereas touching /run/lock would mean either relying
  # on a directory this module does not own or `install -d`-ing over a
  # system directory whose modes are not ours to reset.
  lockFile = "${cfg.runtimeRoot}/hostkeys.lock";

  # --- generator ---------------------------------------------------------
  # IDEMPOTENT AND SELF-HEALING, in that order of priority:
  #
  #   * a VALID existing private key is NEVER touched, so a slot's identity is
  #     STABLE across host reboots, rebuilds, and repairs of OTHER slots (which
  #     is what lets the launcher call this unconditionally before a launch);
  #   * a MISSING or unusable private key is generated;
  #   * a missing/truncated/MISMATCHED public key is RE-DERIVED from the
  #     private key (`ssh-keygen -y`), never worked around by replacing the
  #     private key: the private key is the authority, and throwing it away
  #     would invalidate every known_hosts entry already distributed for that
  #     slot;
  #   * known_hosts is rebuilt DETERMINISTICALLY from the public keys, with at
  #     most ONE entry per alias, and installed ATOMICALLY (temp file + `mv`),
  #     so a reader never sees a partial database;
  #   * ownership and modes are set EXPLICITLY at the end of every slot's
  #     block, so a hand-edited mode heals too.
  #
  # Everything runs under ONE exclusive lock (see `lockFile`).
  provisionHostKeys = pkgs.writeShellApplication {
    name = "agent-microvm-provision-hostkeys";
    runtimeInputs = with pkgs; [
      coreutils
      openssh
      util-linux # flock — serialise concurrent provisioning runs
    ];
    text = ''
      set -euo pipefail

      root=${lib.escapeShellArg hostKeys.root}
      known_hosts=${lib.escapeShellArg hostKeys.knownHosts}
      key_name=${lib.escapeShellArg hostKeys.keyName}
      lock=${lib.escapeShellArg lockFile}

      # The runtime root carries both the lock and the known_hosts database.
      # tmpfiles creates it, but `install -d` keeps this script runnable on its
      # own (the launcher's self-heal path calls it through systemd, the
      # executed harnesses call it directly).
      install -d -m 0755 -o root -g root -- "$(dirname -- "$lock")"
      install -d -m 0700 -o root -g root -- "$root"

      # ---- CONCURRENCY: exactly one writer at a time --------------------
      # Held for the WHOLE body, so the per-slot key generation and the
      # known_hosts rebuild are one critical section. The descriptor is closed
      # (and the lock released) when this script exits, including on failure.
      exec 9>"$lock"
      flock -x 9

      tmp="$(mktemp "$known_hosts.XXXXXX")"
      trap 'rm -f -- "$tmp"' EXIT
      {
        echo "# Generated by agent-microvm-hostkeys.service — do not edit."
        echo "# One deterministic ed25519 host identity per microVM slot."
      } > "$tmp"

      # ONE entry per alias, ever: `ssh` REFUSES a known_hosts file that offers
      # two different keys for the same host, so a duplicate would break the
      # very verification this database exists for. The order is the
      # Nix-generated slot order, so the file is byte-stable across runs.
      declare -A kh_seen=()
      kh_add() {
          local alias="$1" key_line="$2"
          if [[ -n "''${kh_seen[$alias]:-}" ]]; then
              return 0
          fi
          kh_seen["$alias"]=1
          printf '%s %s\n' "$alias" "$key_line" >> "$tmp"
      }

      ${lib.concatMapStringsSep "\n" (slot: ''
        slot_dir=${lib.escapeShellArg (hostKeys.slotDir slot.name)}
        install -d -m 0700 -o root -g root -- "$slot_dir"
        key="$slot_dir/$key_name"
        pub="$key.pub"
        comment=${lib.escapeShellArg "agent-microvm-${slot.name}"}

        # ---- (a0) normalise the MODE before testing the key ---------------
        # `ssh-keygen` REFUSES to read a private key whose mode is too open, so
        # testing the key first would make a mere MODE DRIFT look like a corrupt
        # key and trigger a silent RE-KEY: collateral damage to a stable, pinned
        # identity (every distributed known_hosts entry for the slot would go
        # stale). Repairing the mode is exactly what this unit is for, so do that
        # first and then judge the key material.
        #
        # ONLY for a key that is ALREADY root:root — chowning a foreign-owned
        # key to root would launder a key someone else may have planted. Such a
        # key fails the ownership test below and is replaced instead.
        if [[ -e "$key" ]] && [[ "$(stat -c '%U:%G' -- "$key")" == "root:root" ]]; then
            mode="$(stat -c '%a' -- "$key")"
            if [[ "$mode" != "400" ]]; then
                # Recorded, not hidden: the key WAS readable more widely than it
                # should have been, which only the operator can judge.
                printf 'agent-microvm-hostkeys: normalised over-permissive mode %s on %s (consider rotating this slot key)\n' \
                    "$mode" "$key" >&2
            fi
            chmod 0400 -- "$key"
        fi

        # ---- (a) is the existing PRIVATE key usable AND trustworthy? ------
        # `ssh-keygen -y` is both the derivation of the public half and the
        # cheapest TOTAL validity test (a truncated or corrupted key fails).
        # Its output is discarded here, so no private material reaches a log.
        # Ownership is part of the test: a private key that is not root:root
        # could have been planted by another user, so it is NOT authoritative.
        # The mode is not (it is reset unconditionally below).
        private_ok=0
        if [[ -s "$key" ]] \
            && [[ "$(stat -c '%U:%G' -- "$key")" == "root:root" ]] \
            && ssh-keygen -y -f "$key" >/dev/null 2>&1; then
            private_ok=1
        fi
        if (( private_ok == 0 )); then
            # Only NOW is a key pair (re)generated. Remove a half-written pair
            # first, so ssh-keygen never prompts for overwrite confirmation.
            rm -f -- "$key" "$pub"
            ssh-keygen -q -t ed25519 -N "" -C "$comment" -f "$key"
        fi

        # ---- (b) the PUBLIC key is DERIVED from the private one ----------
        # A private key whose public half is missing, truncated or MISMATCHED is
        # a PARTIAL STATE, not a reason to discard the identity: re-derive the
        # public key instead. That keeps the slot's identity — and every
        # known_hosts entry already distributed for it — intact.
        # NOTE the `cut`: `ssh-keygen -y` echoes the private key's COMMENT as a
        # third field. Normalising to `<type> <body>` keeps (i) the known_hosts
        # entries byte-identical to the historical `cut -f1,2 <pub>` form,
        # (ii) the `.pub` file at ssh-keygen's own three-field shape (so the
        # comment is not duplicated), and (iii) the comparison below stable
        # across runs instead of rewriting the public key every time.
        derived="$(ssh-keygen -y -f "$key" | cut -d" " -f1,2)"
        if [[ ! -s "$pub" ]] || [[ "$(cut -d" " -f1,2 -- "$pub")" != "$derived" ]]; then
            rm -f -- "$pub"
            printf '%s %s\n' "$derived" "$comment" > "$pub"
        fi

        # ---- (c) EXPLICIT final ownership and modes ----------------------
        # virtiofsd passes ownership through unchanged, so these host modes are
        # exactly what the guest sees: only guest root may read the private key.
        # Set unconditionally, so a hand-edited mode heals on the next run.
        chown root:root -- "$key" "$pub"
        chmod 0400 -- "$key"
        chmod 0444 -- "$pub"

        # ---- (d) the slot's known_hosts entries --------------------------
        # Always taken from the DERIVED public key, so the database cannot
        # disagree with the key the guest's sshd will present.
        ${lib.optionalString agentNetwork.transportCaps.guestInterface ''
          # The TAP control channel: the slot's deterministic IPv4. ABSENT under
          # the `vsock` transport (lightweight plan phase 6), where the guest has
          # no network interface at all, so no sshd ever listens on that address.
          kh_add ${lib.escapeShellArg slot.ip} "$derived"
        ''}
        ${lib.optionalString agentCapabilities.vsock ''
          # The VSOCK control channel (lightweight plan phase 6): cloud-hypervisor
          # backs the guest's VSOCK device with the Unix socket
          # <stateRoot>/<slot>/notify.vsock, and the host reaches the guest's
          # `sshd-vsock@` (vsock::22) through `ssh vsock-mux/<that socket>`. Pin
          # the SAME per-slot host key under that address too, so the launcher's
          # VSOCK ssh runs with StrictHostKeyChecking=yes exactly like the TAP
          # one — the VSOCK channel is host-only (CID 2 -> guest) but the
          # verification is still the safer, recorded choice.
          kh_add ${lib.escapeShellArg "vsock-mux/${cfg.stateRoot}/${slot.name}/notify.vsock"} "$derived"
        ''}
      '') slots}

      # Public keys only → world-readable, so a non-root operator can run
      # `agent-microvm ssh` with StrictHostKeyChecking=yes. Installed with a
      # rename, so a concurrent reader sees either the old or the new file and
      # never a partial one.
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
    })

    (lib.mkIf (cfg.enable && (agentCapabilities.interactive || agentCapabilities.vsock)) {
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
