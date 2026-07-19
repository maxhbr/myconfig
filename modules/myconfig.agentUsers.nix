# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# Defines a configurable list of isolated "agent" users.  Each agent
# inherits the full home-manager sharedModules config of the primary user
# (tmux, coding agents, shell, dev tools) but gets an *ephemeral* home
# (state lost on reboot, no impermanence), and a single persistent
# `workdir/` under the work impermanence tree.
#
# Permission model: the primary user is added to every agent's primary
# group (see nixos.user.nix) and agent homes are 0750, so the primary
# user can read agent data while agents cannot read the primary user or
# each other.
{
  config,
  pkgs,
  lib,
  myconfig,
  ...
}:
let
  agents = config.myconfig.agentUsers.names;

  # Launch the agent's persistent tmux session in the current terminal.
  # `sudo -u <name> -i` starts a login shell as the agent (full PATH,
  # home-manager fish config) and `tmux new-session -A` attaches to an
  # existing session or creates a fresh one.
  mkAgentTmux =
    name:
    pkgs.writeShellScriptBin "${name}-tmux" ''
      set -euo pipefail
      exec sudo -u ${name} -i -- tmux new-session -A -s ${name}
    '';

  # Open a new alacritty window running the agent's tmux session.
  # Alacritty runs as the primary user (so it can reach the display),
  # the shell inside switches to the agent via <name>-tmux.
  mkAgentAlacrittyTmux =
    name:
    let
      scriptName = "${name}-alacritty-tmux";
      windowName = "${name}-tmux";
      tmuxScript = lib.getExe (mkAgentTmux name);
    in
    pkgs.writeShellScriptBin scriptName ''
      set -euo pipefail
      exec alacritty \
        --title "${windowName}" \
        --class "Alacritty:${windowName}" \
        --command "${tmuxScript}"
    '';

  agentTmuxScripts = lib.concatMap (name: [
    (mkAgentTmux name)
    (mkAgentAlacrittyTmux name)
  ]) agents;

  agentLauncherCommands = map (name: "${name}-alacritty-tmux") agents;

  agentUserInherit = config.myconfig.agentUsers.inheritFromMainUser;
in
{
  options.myconfig.agentUsers = lib.mkOption {
    type = lib.types.submodule {
      options = {
        names = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ "agent" ];
          description = ''
            Usernames for isolated agent users with ephemeral homes.
            Each user gets the same home-manager sharedModules config as the
            primary user (tmux, coding agents, shell, dev tools) but with an
            ephemeral home (state lost on reboot), no secrets, and a
            persistent `workdir` under the work impermanence tree.
          '';
        };
        inheritFromMainUser = {
          sessionVariables = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ ];
            description = ''
              Session variable names to copy from the main user's
              `home.sessionVariables` to each agent user.
              Example: `[ "SKAINET_TOKEN" "TAIA_BEARER_TOKEN" ]`
            '';
          };
          homeFiles = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ ];
            description = ''
              `home.file` attribute names to copy from the main user's
              config to each agent user. Supports prefix matching — if no
              exact match exists, all keys starting with the prefix are
              included (useful for recursive file trees like
              `.pi/agent/extensions/trustedtokens-provider`).
              Example: `[ ".pi/agent/extensions/skainet-provider.ts" ".pi/agent/extensions/trustedtokens-provider" ]`
            '';
          };
          # NOTE: there is intentionally no "homeConfig" option here.
          # Copying evaluated home-manager option subtrees (e.g.
          # `programs.bash`) from the main user into an agent user as
          # raw definitions causes infinite recursion: the subtrees are
          # produced by `home-manager.sharedModules`, which apply to the
          # agent too, so re-inserting the already-merged value as a
          # definition creates a cycle through the module system's
          # `mapAttrsRecursiveCond` option evaluation.  Anything set in a
          # sharedModule is already inherited by every agent; anything
          # set directly on `home-manager.users.<mainUser>` can be
          # copied leaf-by-leaf via `sessionVariables` / `homeFiles`.
        };
      };
    };
  };

  config = lib.mkIf (agents != [ ]) {
    users = {
      # one primary group per agent (so the primary user can be added to it).
      # Static gids so the impermanence initrd can chown agent homes and file
      # ownership stays stable across reboots on persistent volumes.
      extraGroups = lib.listToAttrs (
        lib.imap0 (i: name: lib.nameValuePair name { gid = 1001 + i; }) agents
      );

      extraUsers = lib.listToAttrs (
        lib.imap0 (
          i: name:
          lib.nameValuePair name {
            isNormalUser = true;
            group = name;
            # static uid: stable ownership on persistent volumes + initrd chown
            uid = 1001 + i;
            home = "/home/${name}";
            homeMode = "0750"; # group-readable: primary user is in the group
            createHome = true;
            shell = "/run/current-system/sw/bin/fish";
            hashedPassword = "!"; # locked: no password login
            linger = true; # let coding-agent user services run without login
            # intentionally NO extraGroups: no wheel, no keys, no docker, ...
          }
        ) agents
      );
    };

    # empty attrset per agent is enough to activate home-manager, which
    # then applies all of home-manager.sharedModules to the agent.
    # The primary user additionally gets the agent tmux launch scripts
    #   <name>-tmux           — run in the current terminal
    #   <name>-alacritty-tmux — open a new alacritty window
    # (mkMerge: two definitions of home-manager.users in one module).
    home-manager.users = lib.mkMerge [
      (lib.genAttrs agents (_: { }))

      # Copy selected sessionVariables from the main user to each agent.
      # Only copies variables that actually exist (skips silently if absent).
      (lib.genAttrs agents (
        name:
        let
          mainVars = config.home-manager.users."${myconfig.user}".home.sessionVariables or { };
          available = builtins.filter (v: builtins.hasAttr v mainVars) agentUserInherit.sessionVariables;
        in
        {
          home.sessionVariables = lib.genAttrs available (var: mainVars.${var});
        }
      ))

      # Copy selected home.file entries from the main user to each agent.
      # Supports both exact matches and prefix matching (for recursive dirs).
      (lib.genAttrs agents (
        name:
        let
          mainFile = config.home-manager.users."${myconfig.user}".home.file;
          allKeys = lib.attrNames mainFile;
          resolve =
            prefix:
            if lib.hasAttr prefix mainFile then [ prefix ] else lib.filter (k: lib.hasPrefix prefix k) allKeys;
          matchedKeys = lib.unique (lib.concatMap resolve agentUserInherit.homeFiles);
        in
        {
          home.file = lib.attrsets.getAttrs matchedKeys mainFile;
        }
      ))

      { "${myconfig.user}".home.packages = agentTmuxScripts; }
    ];
    myconfig.desktop.wayland.launcherCommands = agentLauncherCommands;

    # Let agents connect to the nix daemon: home-manager activation needs
    # store access, but agents are in no group nix allows by default
    # (@wheel/@builders) and have minimal extraGroups.  Add them to
    # `allowed-users` only — deliberately NOT `trusted-users`, so a
    # compromised agent cannot redirect builds to a malicious substituter
    # or import unsigned store paths.
    nix.settings.allowed-users = config.myconfig.agentUsers.names;

    systemd.tmpfiles.rules = lib.concatMap (name: [
      "d /home/${name} 0750 ${name} ${name} - -"
      "d /persistent/cache/home/${name} 0750 ${name} ${name} - -"
    ]) agents;

    # An agent named "offline" is network-isolated: all egress except
    # loopback is rejected, so it can only reach local services (the nix
    # daemon over its unix socket, local LLMs on 127.0.0.1, …).  This is
    # applied automatically wherever an "offline" agent is declared —
    # there is no per-host opt-in, by design (fail-closed).
    #
    # NOTE: the match is on the owning socket's uid, so only the offline
    # user's *own* processes are blocked.  The nix daemon runs as root
    # over a unix socket (unfiltered) and can still fetch store paths —
    # a known limitation if true air-gapping is required.
    networking.firewall = lib.mkIf (builtins.elem "offline" agents) (
      let
        offlineUid = toString config.users.users."offline".uid;
      in
      {
        extraCommands = ''
          # Block all network egress from the "offline" agent except
          # loopback.  iptables does not filter unix sockets, so the
          # nix daemon and the local X/Wayland socket keep working.
          iptables -A OUTPUT -m owner --uid-owner ${offlineUid} -o lo -j RETURN
          iptables -A OUTPUT -m owner --uid-owner ${offlineUid} -j REJECT
        ''
        + lib.optionalString config.networking.enableIPv6 ''
          ip6tables -A OUTPUT -m owner --uid-owner ${offlineUid} -o lo -j RETURN
          ip6tables -A OUTPUT -m owner --uid-owner ${offlineUid} -j REJECT
        '';
      }
    );
  };
}
