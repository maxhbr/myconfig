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
  user = myconfig.user;
  wlrLibinputNoDevices = "1";
in
{
  options.myconfig = with lib; {
    desktop.wayland.cage = {
      enable = mkEnableOption "cage";
      url = mkOption {
        type = types.string;
        default = "";
        description = ''
          url to open in Firefox
        '';
      };
    };
  };
  config = (
    lib.mkIf
      (cfg.desktop.wayland.enable && cfg.desktop.wayland.cage.enable && cfg.desktop.wayland.desktop == "")
      {
        # environment.systemPackages = with pkgs; [ cage firefox ];
        services.cage = {
          enable = true;
          inherit user;
          program =
            if cfg.desktop.wayland.cage.url != "" then
              "${pkgs.firefox}/bin/firefox -kiosk -private-window '${cfg.desktop.wayland.cage.url}'"
            else
              "${pkgs.foot}/bin/foot ${pkgs.tmux}/bin/tmux";
          environment = {
            WLR_LIBINPUT_NO_DEVICES = wlrLibinputNoDevices;
          }
          // lib.optionalAttrs (config.environment.variables ? GDK_PIXBUF_MODULE_FILE) {
            GDK_PIXBUF_MODULE_FILE = config.environment.variables.GDK_PIXBUF_MODULE_FILE;
          };
        };
        # wait for network and DNS
        systemd.services."cage-tty1" = {
          environment.WLR_LIBINPUT_NO_DEVICES = wlrLibinputNoDevices;
          after = [
            "network-online.target"
            "systemd-resolved.service"
          ];
        };
      }
  );
}
