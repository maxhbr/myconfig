inputs:
{ pkgs, config, lib, ... }:
let cfg = config.myconfig;
in {
  options.myconfig = with lib; {
    desktop.wayland.dwl = { enable = mkEnableOption "dwl"; };
  };
  imports = [ inputs.mydwl.nixosModules.mydwl ];
  config =
    (lib.mkIf (cfg.desktop.wayland.enable && cfg.desktop.wayland.dwl.enable) {
      mydwl = {
        enable = true;
        barCommand = null;
        autostartCommands = ''
          ${cfg.desktop.wayland.autostartCommands}
          pkill waybar ; ${config.programs.waybar.package}/bin/waybar > /tmp/dwl.''${XDG_VTNR}.''${USER}.waybar.log 2>&1 &disown
          ${pkgs.dbus}/bin/dbus-update-activation-environment --systemd --all
        '';
      };
      myconfig.desktop.wayland.greetdSettings = {
        dwl_session = {
          command = config.mydwl.startCommand;
          user = "mhuber";
        };
      };
    });
}
