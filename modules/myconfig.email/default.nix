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
    email.enable = mkEnableOption "myconfig.email";
  };
  imports = [
    ./neomutt
    ./aerc
    ./alot.nix
    ./mbsync.nix
    ./himalaya.nix
  ];
  config = lib.mkIf cfg.email.enable {
    home-manager.sharedModules = [ 
      (
        { config, lib, ... }:
        let
          mailAccountsToPersist =
            guard:
            lib.map (a: a.maildir.absPath) (
              lib.filter (a: a.mbsync.enable && guard a) (lib.attrValues config.accounts.email.accounts)
            );
        in
          {
            config = {
              myconfig.persistence.directories = mailAccountsToPersist (a: a.name != "tng");
              myconfig.persistence.work-directories = mailAccountsToPersist (a: a.name == "tng");
              myconfig.persistence.cache-directories = [ ".cache/mu" ];
              home.packages = with pkgs; [
                abook
                aba
                dante # Socksify for rendering HTML
                extract_url
                urlscan
              ];
              programs.aerc.enable = true;
              programs.himalaya.enable = true;
              programs.lieer.enable = true; # gmail sync
              programs.mbsync.enable = true;
              programs.meli.enable = false;
              programs.msmtp.enable = true;
              programs.notmuch = {
                enable = true;
                # hooks = {
                # #   preNew = "${config.programs.mbsync.package}/bin/mbsync --all";
                #   postNew = if config.programs.afew.enable
                #             then "afew --tag --new"
                #             else "";
                # };
              };
              programs.afew.enable = true;
              programs.alot.enable = false;
              programs.mu.enable = true;
              programs.astroid.enable = false;
              programs.neomutt.enable = true;
            };
          };
      )
    ];
    programs.evolution.enable = lib.mkDefault true;
    myconfig.desktop.wayland.launcherCommands = [ "aerc-alacritty" ];
  };
}
