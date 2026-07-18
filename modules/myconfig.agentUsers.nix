# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# Defines a configurable list of isolated "agent" users.  Each agent
# inherits the full home-manager sharedModules config of the primary user
# (tmux, coding agents, shell, dev tools) but gets an *ephemeral* home
# (state lost on reboot, no impermanence), no secrets, and a single
# persistent `workdir/` under the work impermanence tree.
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
  agents = config.myconfig.agentUsers;

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
in
{
  options.myconfig.agentUsers = lib.mkOption {
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
      { "${myconfig.user}".home.packages = agentTmuxScripts; }
    ];
    myconfig.desktop.wayland.launcherCommands = agentLauncherCommands;

    # Let agents connect to the nix daemon: home-manager activation needs
    # store access, but agents are in no group nix allows by default
    # (@wheel/@builders) and have minimal extraGroups.  Add them to
    # `allowed-users` only — deliberately NOT `trusted-users`, so a
    # compromised agent cannot redirect builds to a malicious substituter
    # or import unsigned store paths.
    nix.settings.allowed-users = config.myconfig.agentUsers;

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
