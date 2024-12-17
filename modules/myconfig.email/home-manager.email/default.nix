{ config, lib, pkgs, ... }:

{
  imports = [
    ./neomutt
    ./aerc.nix
    ./alot.nix
    ({ config, ... }: {
      config = lib.mkIf config.programs.mbsync.enable {
        services.mbsync = {
          enable = true;
          package = config.programs.mbsync.package;
          preExec = let
            mbsync-preExec = pkgs.writeShellScriptBin "mbsync-preExec" ''
              ${pkgs.coreutils}/bin/date > "$HOME/Maildir/mbsync-start-pre-exec"
            '';
          in "${mbsync-preExec}/bin/mbsync-preExec";
          postExec = let
            mbsync-postExec = pkgs.writeShellScriptBin "mbsync-postExec" ''
              ${pkgs.coreutils}/bin/date > "$HOME/Maildir/mbsync-start-post-exec"
              ${if config.programs.notmuch.enable then
                "${pkgs.notmuch}/bin/notmuch new --no-hooks --verbose"
              else
                ""}
              ${if config.programs.mu.enable then
                "${pkgs.mu}/bin/mu index"
              else
                ""}
            '';
          in "${mbsync-postExec}/bin/mbsync-postExec";
        };
      };
    })
  ];
  config = {
    home.packages = with pkgs; [ abook extract_url urlscan ];
    programs.mbsync.enable = true;
    programs.msmtp.enable = true;
    programs.lieer.enable = true; # gmail sync
    programs.aerc.enable = true;
    programs.notmuch = {
      enable = true;
      hooks = {
        preNew = "${config.programs.mbsync.package}/bin/mbsync --all";
        # postNew = if config.programs.afew.enable
        #           then "afew --tag --new"
        #           else "";
      };
    };
    programs.afew.enable = true;
    programs.alot.enable = false;
    programs.mu.enable = true;
    programs.astroid.enable = false;
    programs.neomutt.enable = true;
  };
}
