# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  config,
  lib,
  myconfig,
  ...
}:
let
  cfg = config.myconfig;
in
{
  config = (
    lib.mkIf cfg.desktop.wayland.enable {
      services.physlock.enable = lib.mkForce false;

      home-manager.sharedModules = [
        ({config, ...}:
        {
          programs.swaylock = {
            enable = true;
            settings = {
              color = "808080";
              font-size = 24;
              indicator-idle-visible = false;
              indicator-radius = 100;
              line-color = "ffffff";
              show-failed-attempts = true;
            };
          };
          services.swayidle = {
            enable = true;
            timeouts = [
              { timeout = 300; command = "''${config.programs.swaylock.package}/bin/swaylock -fF -c 654321"; }
              # { timeout = 450; command = "''${pkgs.systemd}/bin/systemctl suspend"; }
            ];
            events = [
              { event = "before-sleep"; command = "''${config.programs.swaylock.package}/bin/swaylock -fF -c 123456"; }
              { event = "lock"; command = "lock"; }
            ];
          };
        })
      ];

      # https://github.com/NixOS/nixpkgs/issues/143365
      security.pam.services.swaylock.text = ''
        # Account management.
        account required pam_unix.so

        # Authentication management.
        auth sufficient pam_unix.so   likeauth try_first_pass
        auth required pam_deny.so

        # Password management.
        password sufficient pam_unix.so nullok sha512

        # Session management.
        session required pam_env.so conffile=/etc/pam/environment readenv=0
        session required pam_unix.so
      '';
    }
  );
}
