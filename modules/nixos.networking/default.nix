# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }: {
  imports = [
    # ./extrahosts
    # ./service.stubby.nix
  ];
  config = {
    boot.kernel.sysctl."net.ipv4.ip_forward" = 1;
    boot.kernel.sysctl."net.ipv6.conf.all.forwarding" = "1";
    environment.interactiveShellInit = ''
      myPorts() { /run/wrappers/bin/sudo ${pkgs.iproute2}/bin/ss -tulpen; }
      killPort() { kill $(${pkgs.lsof}/bin/lsof -t -i:$1); }
    '';

    home-manager.sharedModules = [{
      programs.fish = {
        functions = {
          myPorts = "/run/wrappers/bin/sudo ${pkgs.iproute2}/bin/ss -tulpen";
          killPort = "kill $(${pkgs.lsof}/bin/lsof -t -i:$1)";
        };
      };
    }];

    networking = {
      networkmanager.enable = true;
      firewall = {
        enable = true;
        allowPing = false;
      };
    };
  };
}
