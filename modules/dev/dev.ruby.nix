# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.dev.ruby;
in
{
  config = lib.mkIf cfg.enable {
    # `ruby` already ships bin/rake; a second rake would collide in the
    # strict mysbx tool env.
    myconfig.ai.dev.sandboxTools.extraPackages = with pkgs; [
      ruby
      rubyPackages.rspec
    ];
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          ruby
          rubyPackages.rspec
          rubyPackages.rake
        ];
      }
    ];
  };
}
