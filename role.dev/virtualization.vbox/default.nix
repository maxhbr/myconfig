# Copyright 2019-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ ... }:

{ imports = [ ./vagrant.nix ];
  config = {
    virtualisation.virtualbox.host.enable = true;
    # virtualisation.virtualbox.host.enableExtensionPack = true;
  };
}
