# Copyright 2017-2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:

{
  options = {
    myconfig.roles.dev = {
      enable = lib.mkEnableOption "Dev role";
    };
    myconfig.roles.imagework = {
      enable = lib.mkEnableOption "Imagework role";
    };
    myconfig.roles.wine = {
      enable = lib.mkEnableOption "Wine role";
    };
    myconfig.roles.tex = {
      enable = lib.mkEnableOption "Tex role";
    };
  };
}
