# Copyright 2021 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
{
  # imports = [
  #   inputs.nix-index-database.nixosModules.default
  # ]
  config = {
    home-manager.sharedModules = [
      # When using this module do not also include nix-index in your environment.systemPackages list as this will conflict with the nix-index wrapper provided by this project.
      inputs.nix-index-database.homeModules.default
      {
        programs.nix-index-database.comma.enable = true;
        programs.nix-index.enable = true;
      }
    ];
  };
}
