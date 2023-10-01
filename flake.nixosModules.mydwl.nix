inputs: { pkgs, config, lib, ... }:
let
  cfg = config.myconfig;
in {
  options.myconfig = with lib; {
    desktop.wayland.dwl = { enable = mkEnableOption "dwl"; };
  };
  imports = [ inputs.mydwl.nixosModules.mydwl ];
  config =
    (lib.mkIf (cfg.desktop.wayland.enable && cfg.desktop.wayland.dwl.enable) {
      mydwl = {
        enable = true;
        # barCommand = null;
        autostartCommands = cfg.desktop.wayland.autostartCommands;
      };
      myconfig.desktop.wayland.greetdSettings = {
        dwl_session = {
          command = config.mydwl.startCommand;
          user = "mhuber";
        };
      };
    });
}
