{ config, lib, pkgs, ... }:

{
  imports = [
    ./neomutt
    ./astroid.nix
    ./mu
    ({config,...}: {
      config = lib.mkIf config.programs.mbsync.enable {
        services.mbsync = {
          enable = true;
          package = config.programs.mbsync.package;
          postExec = let
            mbsync-postExec = pkgs.writeShellScriptBin "mbsync-postExec" ''
${if config.programs.notmuch.enable then "${pkgs.notmuch}/bin/notmuch new" else ""}
${if config.programs.mu.enable then "${pkgs.mu}/bin/mu index" else ""}
'';
            in "${mbsync-postExec}/bin/mbsync-postExec";
        };
      };
    })
  ];
  config = {
    home.packages = with pkgs; [abook urlview];
    programs.mbsync.enable = true;
    programs.msmtp.enable = true;
    programs.notmuch = {
      enable = true;
      hooks = { preNew = "mbsync --all"; };
    };
    programs.mu.enable = true;
    programs.astroid.enable = true;
    programs.neomutt.enable = true;
  };
}
