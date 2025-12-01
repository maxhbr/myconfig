let
  hm =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = {
        programs.thunderbird = {
          enable = true;
          profiles = {
            "default" = {
              isDefault = true;
            };
          };
        };
        myconfig.desktop.wayland.launcherCommands = [ "thunderbird" ];
        myconfig.persistence.directories = [
          ".thunderbird/default"
        ];
        myconfig.persistence.cache-directories = [
          ".cache/thunderbird/default"
        ];
        # myconfig.persistence.directories = [
        #   ".mozilla"
        # ];
        myconfig.homeManagerEmailConfig = [
          {
            thunderbird = {
              enable = true;
              settings = _: {
                "mail.identity.default.archive_enabled" = true;
                "mail.identity.default.archive_keep_folder_structure" = true;
                "mail.identity.default.compose_html" = false;
                "mail.identity.default.protectSubject" = true;
                "mail.identity.default.reply_on_top" = 1;
                "mail.identity.default.sig_on_reply" = false;

                "mail.identity.id1.include_sig_on_reply" = true;
                "mail.identity.id1.include_sig_on_forward" = true;

                "gfx.webrender.all" = true;
                "gfx.webrender.enabled" = true;

                "browser.display.use_system_colors" = true;
                # "browser.theme.dark-toolbar-theme" = true;
              };
            };
          }
        ];
      };
    };
in
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig;
in
{
  config =
    lib.mkIf (cfg.desktop.enable && cfg.email.enable && (builtins.elem "thunderbird" cfg.email.clients))
      {
        home-manager.sharedModules = [ hm ];
      };
}
