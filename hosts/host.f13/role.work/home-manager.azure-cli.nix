# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  ...
}:
let
  my-azure-cli =
    with pkgs;
    azure-cli.override {
      withExtensions = with azure-cli-extensions; [
        bastion
        ssh
      ];
    };
in
{
  home.packages = with pkgs; [ my-azure-cli ];
  home.file = {
    ".azure/config" = {
      text = ''
        [cloud]
        name = AzureCloud

        [extension]
        use_dynamic_install = no
        dynamic_install_allow_preview = false
      '';
    };
  };
}
