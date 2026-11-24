{
  pkgs,
  lib,
  config,
  ...
}:
{
  config = (
    lib.mkIf config.myconfig.desktop.enable {
      home-manager.sharedModules = [
        {
          programs.wezterm = {
            enable = true;
            extraConfig = ''
              local wezterm = require 'wezterm'
              return {
                keys = {
                  -- free Alt+Enter for TUI applications (pi-coding-agent)
                  { key = "Enter", mods = "ALT", action = wezterm.action.DisableDefaultAssignment },
                },
              }
            '';
          };
        }
      ];
    }
  );
}
