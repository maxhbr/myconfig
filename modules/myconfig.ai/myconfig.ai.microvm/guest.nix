# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.microvm — host microvm.nix integration, the fixed VM slot pool,
# and the minimal Cloud Hypervisor NixOS guest module.
#
# PHASE 2 (plan §2, §4, §5, §6):
#
#   §2  When the feature is enabled, import the microvm.nix *host* module
#       (`inputs.microvm.nixosModules.host`). The host module is imported
#       unconditionally so its `microvm.*` options always exist, but it is
#       neutralized with `microvm.host.enable = lib.mkDefault false` — exactly
#       the load-bearing gating pattern used by
#       `modules/myconfig.ai/hermes-agent/microvm.nix`. Only when
#       `myconfig.ai.microvm.enable` is true do we flip it to `true`, so a
#       disabled feature has zero config side effects (no tap/vhost_net
#       modules, no KSM, no `microvm` user).
#
#   §4  A fixed, declarative pool of microVM slots agent-0 .. agent-<n-1> with
#       deterministic per-slot name / hostname / MAC / IPv4 / TAP interface,
#       all generated from the slot index (never random).
#
#   §5  A MINIMAL Cloud Hypervisor NixOS guest: its own kernel (microvm.nix
#       builds a self-contained guest store disk — the host /nix/store is NOT
#       shared), cloud-hypervisor hypervisor, vcpu/mem from the module
#       options, serial console, graphics disabled, guest hostname = slot
#       name, current stateVersion.
#
#   §6  An unprivileged guest `agent` user (uid 1000, home /home/agent, no
#       extra groups, locked password, not root).
#
# DEFERRED to later phases (still inert stubs):
#   - network.nix  (plan §12–§16): private bridge, firewall, LiteLLM
#     forwarder, guest IP addressing / default route. Slots below therefore
#     declare a deterministic MAC + TAP but no guest-side IP configuration —
#     the interface is wired up but not yet addressed/routable.
#   - workmux.nix  (plan §29): Workmux agent registrations.
#
# IMPLEMENTED HERE (plan §10 workspace share, §11 UID/GID ownership):
#   Each slot declares EXACTLY ONE `microvm.shares` entry — the writable
#   virtiofs `/workspace` share, whose host source is the SAME path the
#   launcher bind-mounts the standalone clone onto
#   (${stateRoot}/<slot>/workspace). The launcher (launcher.nix) chowns that
#   clone to uid/gid 1000 so it appears owned by the guest `agent` user and is
#   read-write inside the guest (§11). See docs/agent-microvm/agent-microvm.md.
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.myconfig.ai.microvm;

  # Deterministic fixed slot pool — the single source of truth lives in
  # slots.nix and is shared with default.nix (which asserts uniqueness and
  # the slot-count bound over this exact table).
  slots = (import ./slots.nix { inherit lib; }).mkSlots cfg.slotCount;

  # Prefix length of the private subnet (e.g. "192.168.83.0/24" -> 24), used
  # for the guest-side static address. Derived from the SAME option the host
  # bridge uses, so host and guest agree.
  guestPrefixLength = lib.toInt (lib.last (lib.splitString "/" cfg.subnet));

  # --- §19 guest agent entry point (`agent-run`) --------------------------
  # Refuses root, verifies /workspace is a mounted, writable mount, cds into
  # it, prints the guest identity + selected agent, then execs the agent from
  # argv (no `eval`, so the exit status is the agent's own).
  agent-run = pkgs.writeShellApplication {
    name = "agent-run";
    runtimeInputs = with pkgs; [
      coreutils
      util-linux # findmnt
    ];
    text = ''
      set -euo pipefail

      if [[ "$(id -u)" -eq 0 ]]; then
          echo "agent-run: refusing to run as root" >&2
          exit 1
      fi
      if [[ "$#" -lt 1 ]]; then
          echo "usage: agent-run <agent> [args...]" >&2
          exit 2
      fi

      workspace=/workspace
      if ! findmnt -n -- "$workspace" >/dev/null 2>&1; then
          echo "agent-run: $workspace is not a mount point" >&2
          exit 1
      fi
      if [[ ! -w "$workspace" ]]; then
          echo "agent-run: $workspace is not writable" >&2
          exit 1
      fi

      cd "$workspace"
      printf 'agent-run: host=%s agent=%s workspace=%s\n' \
          "$(uname -n)" "$1" "$workspace" >&2

      exec "$@"
    '';
    meta = with lib; {
      description = "Guest-side agent entry point for myconfig.ai.microvm sandboxes";
      platforms = platforms.linux;
    };
  };

  # --- minimal Cloud Hypervisor guest for a given slot --------------------
  mkGuest = slot: {
    # microvm.nix auto-imports its guest `microvm` module for VMs declared
    # via `microvm.vms.<name>.config`, so no explicit import is needed here.
    # Redundant with microvm.nix's `networking.hostName = mkDefault name`,
    # kept as an explicit, non-default assertion of the slot's identity.
    networking.hostName = slot.hostName;

    microvm = {
      hypervisor = "cloud-hypervisor";
      vcpu = cfg.defaultVcpu;
      mem = cfg.defaultMemoryMiB;

      # Deterministic per-slot TAP + MAC (plan §4). Guest-side IP
      # addressing / routing is intentionally deferred to network.nix.
      interfaces = [
        {
          type = "tap";
          id = slot.tap;
          mac = slot.mac;
        }
      ];

      # No graphics for a headless agent sandbox (also microvm's default).
      graphics.enable = false;

      # --- §10 workspace share (the ONLY share) --------------------------
      # Exactly one virtiofs share surfaces the slot's host-side workspace
      # directory into the guest as the single writable `/workspace` mount.
      # Its `source` is the SAME host path the launcher uses as its
      # bind-mount target — `mount_point()` in launcher.nix renders
      # "${cfg.stateRoot}/<slot>/workspace" — so host and guest agree on the
      # one writable path (plan §10: "Slot references stable relative
      # workspace path in microVM state dir"). Read-write on purpose
      # (readOnly is left at its `false` default); the guest agent edits the
      # clone in place.
      #
      # Ownership (§11): virtiofsd passes file ownership through unchanged
      # (no --translate-uid/gid), and the launcher chowns the clone tree to
      # uid/gid 1000 on the host, so `/workspace` appears owned by the guest
      # `agent` user (uid 1000) and is writable — satisfying `agent-run`'s
      # `test -w /workspace` check. posixAcl stays at its `true` default
      # (no conflict, since no UID/GID translation is used).
      #
      # This is the ONLY share: microvm.nix keeps the guest /nix/store on its
      # own storeDisk (microvm.storeOnDisk defaults to true unless a share's
      # source is "/nix/store", which this is not), so it does NOT add a
      # store share. The guest therefore has EXACTLY this one share. Do NOT
      # add /nix, /home, host sockets or any other share here (plan §10).
      shares = [
        {
          proto = "virtiofs";
          tag = "workspace";
          source = "${cfg.stateRoot}/${slot.name}/workspace";
          mountPoint = "/workspace";
        }
      ];
    };

    # Unprivileged guest user that runs the agent (plan §6). Not root; no
    # wheel/docker/kvm/host groups; login disabled (`!` = locked password).
    users.users.agent = {
      isNormalUser = true;
      uid = 1000;
      home = "/home/agent";
      createHome = true;
      extraGroups = [ ];
      hashedPassword = "!";
    }
    // lib.optionalAttrs (cfg.enableSsh && cfg.sshPublicKeyFile != null) {
      # §18: exactly one dedicated public key authorises the guest `agent`
      # user. NOT a host authorized_keys file.
      openssh.authorizedKeys.keyFiles = [ cfg.sshPublicKeyFile ];
    };

    # --- §7 minimal guest toolchain + the §19 entry point ----------------
    # A deliberately small package set plus the four agent binaries so
    # `agent-run <bin>` can exec them inside the guest. The agent packages
    # are the SAME repo package attrs the host coding-agent modules use
    # (programs.claude-code → pkgs.claude-code, programs.codex → pkgs.codex,
    # programs.opencode → pkgs.opencode, programs.pi-coding-agent →
    # pkgs.nixos-unstable.pi-coding-agent). They are baked into the immutable
    # guest closure (§8: no runtime CLI download, no host Nix daemon). Their
    # exe names are exactly the launcher's agent set: claude / codex /
    # opencode / pi.
    environment.systemPackages = with pkgs; [
      agent-run
      # §7 agent binaries (exe names: claude, codex, opencode, pi).
      claude-code
      codex
      opencode
      nixos-unstable.pi-coding-agent
      bash
      coreutils
      curl
      diffutils
      fd
      file
      findutils
      git
      gnugrep
      gnumake
      gnused
      jq
      less
      openssh
      patch
      procps
      ripgrep
      rsync
      tree
      unzip
      util-linux
      which
    ];

    # --- §17 guest model-API config (no secrets) -------------------------
    # The guest reaches the model API ONLY via the bridge-only LiteLLM
    # forwarder. No real upstream key is ever placed in the guest.
    environment.variables = {
      OPENAI_BASE_URL = "http://${cfg.gatewayAddress}:${toString cfg.litellmPort}/v1";
      OPENAI_API_KEY = "not-needed";
    };

    # --- guest-side static addressing (deterministic, matches the slot) ---
    # Assign the slot's fixed IPv4 via systemd-networkd, matched on the
    # deterministic MAC so it is independent of the kernel interface name.
    # Default route points at the host bridge address (the only reachable
    # peer under the proxy-only firewall policy). IPv6 stays disabled (§15).
    systemd.network = lib.mkIf cfg.enableSsh {
      enable = true;
      networks."10-agent" = {
        matchConfig.MACAddress = slot.mac;
        address = [ "${slot.ip}/${toString guestPrefixLength}" ];
        routes = [ { Gateway = cfg.gatewayAddress; } ];
        networkConfig.LinkLocalAddressing = "no";
        linkConfig.RequiredForOnline = "no";
      };
    };

    # --- §18 hardened SSH, private guest interface only ------------------
    services.openssh = lib.mkIf cfg.enableSsh {
      enable = true;
      settings = {
        PermitRootLogin = "no";
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
        AllowAgentForwarding = "no";
        X11Forwarding = false;
        PermitTunnel = "no";
        AllowTcpForwarding = "no";
      };
    };

    system.stateVersion = "25.11";
  };
in
{
  # §2: the host module is imported unconditionally so `microvm.*` options
  # always exist, then neutralized below unless the feature is enabled.
  imports = [
    inputs.microvm.nixosModules.host
  ];

  config = lib.mkMerge [
    # Neutralize the upstream default `microvm.host.enable = true` so merely
    # importing this module (feature disabled) has no side effects. Matches
    # hermes-agent/microvm.nix; both set the same `mkDefault false`, which
    # merges cleanly.
    { microvm.host.enable = lib.mkDefault false; }

    (lib.mkIf cfg.enable {
      microvm.host.enable = true;

      # Fixed declarative slot pool → one microvm.nix VM per slot.
      microvm.vms = builtins.listToAttrs (
        map (slot: {
          name = slot.name;
          value = {
            # Do not autostart: slots are started on demand by the launcher
            # (later phase). Keeping them off avoids booting empty sandboxes.
            autostart = false;
            config = mkGuest slot;
          };
        }) slots
      );
    })
  ];
}
