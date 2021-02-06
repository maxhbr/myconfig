# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
let
  user = config.myconfig.user;
in {
  config = {
    system.extraSystemBuilderCmds = ''
      mkdir -p $out/pkgs/
      ln -s ${pkgs.nodejs-12_x} $out/pkgs/nodejs-12_x
      ln -s ${pkgs.nodejs-14_x} $out/pkgs/nodejs-14_x
      ln -s ${pkgs.nodejs-15_x} $out/pkgs/nodejs-15_x
    '';

    environment = {
      systemPackages = with pkgs; [ nodejs ];
      variables = {
        NODEJS_12_HOME = "/run/current-system/pkgs/nodejs-12_x";
        NODEJS_14_HOME = "/run/current-system/pkgs/nodejs-14_x";
        NODEJS_15_HOME = "/run/current-system/pkgs/nodejs-15_x";
      };
    };
    home-manager.users."${user}" = {
      home.file = {
        ".gradle/nodejs/node-v12.16.1-linux-x64" = {
        };
      };
    };
  };
}
