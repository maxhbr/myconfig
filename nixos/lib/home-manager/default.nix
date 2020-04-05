# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:

let
  jsonFile = ./. + "/home-manager.json";
  json = builtins.fromJSON (builtins.readFile jsonFile);
  home-manager = builtins.fetchGit {
    url = "https://github.com/rycee/home-manager.git";
    inherit (json) rev ref;
  };
in {
  imports = [
    "${home-manager}/nixos"
  ];
  config = {
    system.activationScripts.genProfileManagementDirs = "mkdir -m 0755 -p /nix/var/nix/{profiles,gcroots}/per-user/mhuber";
    systemd.services.mk-hm-dirs =
      { serviceConfig.Type = "oneshot";
        script = ''
          mkdir -m 0755 -p /nix/var/nix/{profiles,gcroots}/per-user/mhuber
          chown mhuber /nix/var/nix/{profiles,gcroots}/per-user/mhuber
        '';
        wantedBy = ["home-manager-mhuber.service"];
      };
  };
}

