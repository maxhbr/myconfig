# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.microvm — guest dotfile provisioning.
#
# Runs home-manager *inside* the Cloud Hypervisor guest for the unprivileged
# `agent` user, so a sandboxed agent gets the SAME shell + coding-agent
# configuration as the host primary user (fish prompt/abbreviations/functions,
# opencode / pi / codex settings, skills, ...).
#
# Design — allowlist copy, never wholesale:
#   The host primary user's home-manager config already *generates* every
#   dotfile as a `home.file` / `xdg.configFile` entry whose `source` is a
#   store path. Rather than re-evaluate the (deeply host-coupled: jail,
#   workmux, impermanence, secrets) home modules inside the guest, we copy the
#   ALREADY-EVALUATED file entries — keyed by their relative target — into the
#   guest agent's home-manager config. home-manager then pulls their sources
#   into the guest closure (the guest has its OWN /nix/store disk, so the
#   sources are baked into the guest image, not shared from the host).
#
#   The copy is an ALLOWLIST (prefix match on the entry key), never a
#   denylist: this is the same fail-closed posture as
#   `modules/myconfig.agentUsers.nix` (`inheritFromMainUser.homeFiles`). Only
#   the explicitly named prefixes cross the boundary, so host secrets
#   (tokens, credentials, keys) are never dragged into the sandbox by
#   accident. Keep secret-bearing paths OUT of the default prefix lists.
#
# This module only DEFINES the option surface + the pure helper that renders
# the guest home-manager module. guest.nix imports the helper and wires it
# into each slot's guest config (via `inputs.home.nixosModules.home-manager`).
{
  config,
  lib,
  myconfig,
  ...
}:
let
  cfg = config.myconfig.ai.microvm;
in
{
  options.myconfig.ai.microvm.guestDotfiles = with lib; {
    enable = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Provision the guest `agent` user with a copy of the host primary
        user's shell + coding-agent dotfiles (fish, opencode, pi, codex,
        skills). When false the guest keeps a bare home.
      '';
    };

    homeFilePrefixes = mkOption {
      type = types.listOf types.str;
      default = [
        ".pi/"
        ".codex/"
        ".agents/"
        ".qwen/"
        ".config/git/"
        ".gitconfig"
      ];
      description = ''
        Allowlist of `home.file` entry keys (relative to $HOME) copied from
        the host primary user into the guest `agent` user. A key matches when
        it equals a prefix or starts with it. ALLOWLIST — never put
        secret-bearing paths here; anything matched is baked into the guest
        image.
      '';
    };

    xdgConfigPrefixes = mkOption {
      type = types.listOf types.str;
      default = [
        "fish/"
        "opencode/"
      ];
      description = ''
        Allowlist of `xdg.configFile` entry keys (relative to
        $XDG_CONFIG_HOME, i.e. ~/.config) copied from the host primary user
        into the guest `agent` user. Same fail-closed allowlist semantics as
        `homeFilePrefixes`.
      '';
    };
  };

  config = {
    # Expose a pure renderer that guest.nix consumes. It reads the host
    # primary user's ALREADY-EVALUATED home-manager file entries and returns a
    # guest-side home-manager module. Placed under an internal-ish attr rather
    # than an option so it stays out of the documented option surface while
    # remaining a single source of truth.
    _module.args.mkGuestHome =
      { pkgs }:
      let
        dc = cfg.guestDotfiles;

        primaryHome = config.home-manager.users.${myconfig.user};

        matchesAny = prefixes: key: lib.any (p: key == p || lib.hasPrefix p key) prefixes;

        pick = fileset: prefixes: lib.filterAttrs (name: _: matchesAny prefixes name) fileset;
      in
      lib.optionalAttrs dc.enable {
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          users.agent = {
            home.stateVersion = "25.11";
            # Copy the allowlisted, already-rendered file entries verbatim.
            # Their `source` fields are store paths, so no host home module is
            # re-evaluated inside the guest.
            home.file = pick primaryHome.home.file dc.homeFilePrefixes;
            xdg.configFile = pick primaryHome.xdg.configFile dc.xdgConfigPrefixes;
          };
        };
      };
  };
}
