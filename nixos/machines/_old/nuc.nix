# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  imports = [
    ## hardware:
    ./hardware/efi.nix
    
    ## software:
    ../profiles/virtualization.nix
    ../profiles/openssh.nix
    ../profiles/terminal.nix
    # ../profiles/vsftp.nix
    # ../profiles/taskserver.nix
  ];

  # services.xservervideoDrivers = [ "intel" ];
}
