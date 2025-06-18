{ config, pkgs, lib, myconfig, ... }:
{
  config = {
    programs.ssh.startAgent = true;
    services.gnome.gcr-ssh-agent.enable = lib.mkForce false;
    home-manager.sharedModules = [{
      home.packages = with pkgs; [ 
        eternal-terminal
        sshfs
      ];
      home.file = {
        ".ssh/config".text = ''
          ControlMaster auto
          ControlPath ~/.ssh/control:%h:%p:%r
          Include ~/.ssh/imports/*.config

          Host localhost
              StrictHostKeyChecking no
              UserKnownHostsFile=/dev/null

          Host 127.0.0.1
              StrictHostKeyChecking no
              UserKnownHostsFile=/dev/null
        '';
        ".ssh/imports/wireguard.config".text = ''
          Host 10.199.199.*
              User ${myconfig.user}
        '';
      };
    }];
  };
}
