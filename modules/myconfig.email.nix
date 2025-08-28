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
  options.myconfig = with lib; {
    email = {
      enable = mkEnableOption "myconfig.email";
      indexer = mkOption {
        type = types.enum [
          "notmuch"
          "mu"
        ];
        default = "notmuch";
      };
      syncer = mkOption {
        type = types.enum [
          "mbsync"
        ];
        default = "mbsync";
      };
      clients = mkOption {
        type = types.listOf types.str;
        description = lib.mdDoc ''
          List of email clients to enable.
        '';
        default = [
          "neomutt"
          "aerc"
        ];
      };
    };
  };
  config = lib.mkIf cfg.email.enable {
    home-manager.sharedModules = [
      (
        { config, lib, ... }:
        {
          config = {
            home.packages = with pkgs; [
              abook
              aba
              dante # Socksify for rendering HTML
              extract_url
              urlscan
            ];
            programs.lieer.enable = true; # gmail sync
            programs.msmtp.enable = true;
            programs.afew.enable = true;
          };
        }
      )
    ];
    programs.evolution.enable = lib.mkDefault true;
  };
}
