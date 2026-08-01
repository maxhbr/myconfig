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
#   - launcher.nix (plan §20–§28): host launcher, slot allocation, bind-mount
#     lifecycle, workspace virtiofs share.
#   - workmux.nix  (plan §29): Workmux agent registrations.
{
  config,
  lib,
  inputs,
  ...
}:
let
  cfg = config.myconfig.ai.microvm;

  # Deterministic fixed slot pool — the single source of truth lives in
  # slots.nix and is shared with default.nix (which asserts uniqueness and
  # the slot-count bound over this exact table).
  slots = (import ./slots.nix { inherit lib; }).mkSlots cfg.slotCount;

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
