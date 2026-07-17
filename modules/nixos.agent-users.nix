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
  lib,
  ...
}:
let
  agents = config.myconfig.agentUsers;
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
    home-manager.users = lib.genAttrs agents (_: { });

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
  };
}
