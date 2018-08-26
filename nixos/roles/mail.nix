# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:

{
  options = {
    myconfig.roles.mail = {
      enable = lib.mkEnableOption "Mail role";
    };
  };

  config = lib.mkIf config.myconfig.roles.mail.enable {
    environment.systemPackages = with pkgs; [
      neomutt
      offlineimap msmtp gnupg abook urlview unstable.notmuch
      sxiv
      procmail
    ];

    services.offlineimap = {
      enable = true;
      path = with pkgs; [ unstable.notmuch ];
    };
  };
}
