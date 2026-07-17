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
      # one primary group per agent (so mhuber can be added to it)
      extraGroups = lib.genAttrs agents (_: { });

      extraUsers = lib.genAttrs agents (name: {
        isNormalUser = true;
        group = name;
        home = "/home/${name}";
        homeMode = "0750"; # group-readable: primary user is in the group
        createHome = true;
        shell = "/run/current-system/sw/bin/fish";
        hashedPassword = "!"; # locked: no password login
        linger = true; # let coding-agent user services run without login
        # intentionally NO extraGroups: no wheel, no keys, no docker, ...
      });
    };

    # empty attrset per agent is enough to activate home-manager, which
    # then applies all of home-manager.sharedModules to the agent.
    home-manager.users = lib.genAttrs agents (_: { });

    systemd.tmpfiles.rules = lib.concatMap (name: [
      "d /home/${name} 0750 ${name} ${name} - -"
      "d /persistent/work/home/${name} 0750 ${name} ${name} - -"
    ]) agents;
  };
}
