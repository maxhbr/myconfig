{ pkgs, lib, config, ... }:
let
  user = config.myconfig.user;
in {
  config = (lib.mkIf config.services.xserver.enable {
    home-manager.users."${user}" =
      {config, ...}:
      (lib.mkIf config.programs.kitty.enable {
        programs.kitty = {
          settings = {
            scrollback_lines = 10000;
            enable_audio_bell = false;
            update_check_interval = 0;
            foreground = "#383a42";
            background = "#f9f9f9";
          };
        };
      });
  });
}
