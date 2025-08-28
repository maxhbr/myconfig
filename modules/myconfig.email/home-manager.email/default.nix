{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ./neomutt
    ./aerc.nix
    ./alot.nix
    ./mbsync.nix
    ./himalaya.nix
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
        };
      }
    )
  ];
  config = {
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
      #   preNew = "${config.programs.mbsync.package}/bin/mbsync --all";
      #   # postNew = if config.programs.afew.enable
      #   #           then "afew --tag --new"
      #   #           else "";
      # };
    };
    programs.afew.enable = true;
    programs.alot.enable = false;
    programs.mu.enable = true;
    myconfig.persistence.cache-directories = [ ".cache/mu" ];
    programs.astroid.enable = false;
    programs.neomutt.enable = true;
  };
}
