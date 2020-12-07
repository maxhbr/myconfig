# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
let
  user = config.myconfig.user;
  jsonFile = ./. + "/home-manager.json";
  json = builtins.fromJSON (builtins.readFile jsonFile);
  home-manager = builtins.fetchGit {
    url = "https://github.com/rycee/home-manager.git";
    inherit (json) rev ref;
  };
in {
  imports = [ "${home-manager}/nixos" ];
  config = {
    home-manager.users."${user}" = {
      nixpkgs.overlays = config.nixpkgs.overlays;
    };
    system.activationScripts.genProfileManagementDirs =
      "mkdir -m 0755 -p /nix/var/nix/{profiles,gcroots}/per-user/${user}";
    systemd.services.mk-hm-dirs = {
      serviceConfig.Type = "oneshot";
      script = ''
        mkdir -m 0755 -p /nix/var/nix/{profiles,gcroots}/per-user/${user}
        chown ${user} /nix/var/nix/{profiles,gcroots}/per-user/${user}
      '';
      wantedBy = [ "home-manager-${user}.service" ];
    };
  };
}

