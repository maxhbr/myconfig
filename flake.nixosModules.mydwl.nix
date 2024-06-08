{ inputs, pkgs, config, lib, ... }:
let cfg = config.myconfig;
in {
  imports = [ inputs.mydwl.nixosModules.mydwl ];
  config = (lib.mkIf (cfg.desktop.wayland.enable
    && builtins.elem "dwl" cfg.desktop.wayland.selectedSessions) {
      mydwl = {
        enable = true;
        barCommand = null;
        autostartCommands = ''
          ${cfg.desktop.wayland.autostartCommands}
          pkill waybar ; ${config.programs.waybar.package}/bin/waybar > /tmp/dwl.''${XDG_VTNR}.''${USER}.waybar.log 2>&1 &disown
          ${pkgs.dbus}/bin/dbus-update-activation-environment --systemd --all
        '';
      };
      myconfig.desktop.wayland.sessions = {
        dwl = {
          command = config.mydwl.startCommand;
        };
      };
    });
}
