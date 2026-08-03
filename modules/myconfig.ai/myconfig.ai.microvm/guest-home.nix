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

        # --- agent skills -------------------------------------------------
        # The host primary user's skills come from TWO mechanisms:
        #   1. Handcrafted skills (commit, coordinator, playwright-cli, ...)
        #      are rendered as ordinary `home.file` / `xdg.configFile`
        #      entries (pi -> `.agents/skills/*`, codex -> `.codex/skills/*`,
        #      opencode -> `.config/opencode/skills/*`). Those already cross
        #      the boundary via the `pick` allowlist copy above.
        #   2. The `programs.agent-skills` framework (mattpocock's skill set)
        #      installs into the SAME per-agent skills dirs but via a runtime
        #      `home.activation` rsync of a composed store-path *bundle*, NOT
        #      as `home.file` entries — so the allowlist copy misses them
        #      entirely and the guest agents would see no framework skills.
        #
        # Bridge mechanism 2 into the copy-of-rendered-store-paths model by
        # symlinking the host's ALREADY-BUILT bundle (`bundlePath`, a store
        # path exactly like every other copied `source`) into each enabled
        # target's skills dir as a recursive `home.file` tree. This pulls the
        # framework skills into the guest closure and deploys them verbatim
        # (same `engineering/`+`productivity/` layout the host uses), for
        # every agent the host enabled a target for (pi, claude, codex,
        # opencode). Disjoint from the handcrafted leaf names above, so the
        # two coexist under the same dir.
        agentSkills = primaryHome.programs.agent-skills or null;
        skillsEnabled = agentSkills != null && (agentSkills.enable or false);

        # An agent-skills target `dest` is a shell string that may embed
        # `$HOME` or a `${VAR:-$HOME/...}` fallback (e.g. claude's
        # `${CLAUDE_CONFIG_DIR:-$HOME/.claude}/skills`). `home.file` keys are
        # static paths relative to $HOME, so reduce each dest to that form —
        # mirroring the vendored module's own `link`-structure handling.
        staticSkillDest =
          dest:
          let
            # Resolve a `${VAR:-$HOME/...}` fallback (e.g. claude's
            # `${CLAUDE_CONFIG_DIR:-$HOME/.claude}/skills`) by dropping the
            # `${VAR:-` prefix and the matching `}` — leaving the plain
            # `$HOME/...` form the else-branch below strips.
            resolved =
              if lib.hasPrefix "\${" dest then
                lib.replaceStrings [ "}" ] [ "" ] (lib.last (lib.splitString ":-" dest))
              else
                dest;
          in
          if lib.hasPrefix "$HOME/" resolved then
            lib.removePrefix "$HOME/" resolved
          else
            throw "myconfig.ai.microvm guest skills: unsupported agent-skills dest '${dest}'";

        skillFiles = lib.optionalAttrs skillsEnabled (
          lib.mapAttrs' (
            _: t:
            lib.nameValuePair (staticSkillDest t.dest) {
              source = agentSkills.bundlePath;
              recursive = true;
              force = true;
            }
          ) (lib.filterAttrs (_: t: t.enable or false) agentSkills.targets)
        );
      in
      lib.optionalAttrs dc.enable {
        # home-manager's activation runs as the guest `home-manager-agent`
        # system service at boot. Its very first step (`setupVars` in
        # home-manager's activation-init.sh) aborts with
        #   "Could not find suitable profile directory"
        # (exit 1, before ANY dotfile is linked) unless a per-user Nix
        # profile directory exists — it probes, in order,
        # `~/.local/state/nix/profiles` then
        # `/nix/var/nix/profiles/per-user/<user>`.
        #
        # On a persistent host those dirs were created long ago by
        # nix-daemon the first time the user ran nix, and survive reboots.
        # This guest, by contrast, has an EPHEMERAL tmpfs rootfs rebuilt
        # every boot, and the unprivileged `agent` user never runs nix
        # before the activation service fires — so nix-daemon never lazily
        # creates the dir and nixpkgs' own tmpfiles rules do not pre-create
        # it either. The result was a guest home containing only the runtime
        # dirs `fish` makes for itself and NONE of the provisioned coding-
        # agent dotfiles / skills / extensions.
        #
        # Pre-create the `agent` per-user profile + gcroots dirs so
        # `setupVars` finds a suitable profile directory and activation
        # proceeds to link the dotfiles. systemd-tmpfiles runs at
        # sysinit.target, well before the multi-user `home-manager-agent`
        # service, so the dirs are guaranteed present in time. `/nix/var` is
        # on the writable rootfs (only `/nix/store` is the read-only store
        # disk), so these are writable. Owned by `agent` to match what
        # nix-daemon would have created on first use.
        systemd.tmpfiles.rules = [
          "d /nix/var/nix/profiles/per-user/agent 0755 agent users - -"
          "d /nix/var/nix/gcroots/per-user/agent 0755 agent users - -"
        ];

        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          users.agent = {
            home.stateVersion = "25.11";
            # Copy the allowlisted, already-rendered file entries verbatim
            # (their `source` fields are store paths, so no host home module
            # is re-evaluated inside the guest), plus the framework skill
            # bundle deployed per enabled agent target (see `skillFiles`).
            home.file = (pick primaryHome.home.file dc.homeFilePrefixes) // skillFiles;
            xdg.configFile = pick primaryHome.xdg.configFile dc.xdgConfigPrefixes;
          };
        };
      };
  };
}
