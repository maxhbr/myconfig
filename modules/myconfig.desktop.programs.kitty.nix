{
  pkgs,
  lib,
  config,
  ...
}:
{
  config = (
    lib.mkIf config.services.xserver.enable {
      home-manager.sharedModules = [
        {
          programs.kitty = {
            settings = {
              scrollback_lines = 10000;
              enable_audio_bell = false;
              update_check_interval = 0;
              foreground = "#383a42";
              background = "#f9f9f9";
            };
          };
        }
      ];
    }
  );
}
