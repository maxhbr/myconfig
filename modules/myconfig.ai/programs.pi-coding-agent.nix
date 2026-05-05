# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  pkgs,
  ...
}:

let
  callLib = file: import file { inherit lib pkgs; };
in
{
  options.myconfig = with lib; {
    ai.pi-coding-agent = {
      enable = mkEnableOption "myconfig.ai.pi-coding-agent";
    };
  };
  config = lib.mkIf config.myconfig.ai.pi-coding-agent.enable {
    home-manager.sharedModules = [
      {
        myconfig.persistence.directories = [ ".pi" ];
        home.packages = [
          pkgs.nixos-unstable.pi-coding-agent
          (callLib ./fns/sandboxed-app.nix {
            name = "pi";
            pkg = pkgs.nixos-unstable.pi-coding-agent;
            writableDirs = [
              ".pi"
            ];
          })
        ];
      }
    ];
  };
}
