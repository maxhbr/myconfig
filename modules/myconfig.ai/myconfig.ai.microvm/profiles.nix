# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.microvm — THE profile table (lightweight plan, phase 1).
#
# `myconfig.ai.microvm.profile` is the stable compatibility boundary between
# the existing, full-featured sandbox tier and the LIGHTWEIGHT one the
# `docs/myconfig-ai-microvm-lightweight-plan.md` describes. It exists so that
# later phases of that plan (selected agents only, runtime config staging,
# consolidated shares, VSOCK transport) can change the `lite` shape without
# ever touching the behaviour an existing host gets.
#
# This file is the single source of truth for what a profile MEANS; default.nix
# resolves the effective profile and hands the resolved table to the sibling
# modules through `_module.args.agentProfile`, exactly like
# ./network-profiles.nix does for the network decision.
#
# Fields:
#
#   resourceClasses  the class table a host gets when it defines NEITHER
#                    `resourceClasses` NOR the deprecated slot options.
#                    `null` means "derive it from the (deprecated) slotCount /
#                    defaultVcpu / defaultMemoryMiB options", i.e. exactly the
#                    pre-profile behaviour.
#   optimizeStore    pin microvm.nix's closure/startup optimizations
#                    (`microvm.optimize.enable`) instead of inheriting its
#                    upstream default, so a guest cannot silently regain
#                    documentation, a non-systemd initrd or
#                    network-wait-online.
#   storeDiskType    pin the guest store disk filesystem
#                    (`microvm.storeDiskType`); `null` keeps microvm.nix's own
#                    default.
#   enabledAgents    the SELECTED agents (./agents.nix tokens) a host gets when
#                    it does not set `enabledAgents` itself. `null` means every
#                    declared agent, i.e. the historical behaviour.
#   minimalGuestPackages
#                    build the guest with the MINIMAL generic CLI toolset and a
#                    plain bash login shell instead of the historical full set
#                    plus fish (see guest.nix for the per-package rationale).
{ lib }:
rec {
  profiles = {
    # Existing behaviour, byte-for-byte: sizing from the legacy options (or an
    # explicit `resourceClasses`), and microvm.nix's own store-disk defaults.
    full = {
      resourceClasses = null;
      optimizeStore = null;
      storeDiskType = null;
      enabledAgents = null;
      minimalGuestPackages = false;
    };

    # The lightweight interactive shape: ONE prebuilt slot, 2 vCPU, 4 GiB RAM,
    # an explicitly optimized EROFS guest store, ONE agent, a minimal guest
    # toolset.
    lite = {
      resourceClasses = {
        lite = {
          count = 1;
          vcpu = 2;
          memoryMiB = 4096;
        };
      };
      optimizeStore = true;
      storeDiskType = "erofs";
      # ONE agent runtime in the guest closure. Codex is the plan's reference
      # agent ("a Codex-only interactive lite VM"); a host that wants another
      # one just sets `enabledAgents`, which outranks this default.
      enabledAgents = [ "codex" ];
      minimalGuestPackages = true;
    };
  };

  names = lib.attrNames profiles;

  forProfile = profile: profiles.${profile};
}
