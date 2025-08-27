{ config, lib, pkgs, ... }:
{
  config = lib.mkIf config.programs.mbsync.enable {
    services.mbsync = {
      enable = true;
      package = config.programs.mbsync.package;
      preExec =
        let
          mbsync-preExec = pkgs.writeShellScriptBin "mbsync-preExec" ''
            ${pkgs.coreutils}/bin/date > "$HOME/Maildir/mbsync-start-pre-exec"
          '';
        in
        "${mbsync-preExec}/bin/mbsync-preExec";
      postExec =
        let
          mbsync-postExec = pkgs.writeShellScriptBin "mbsync-postExec" ''
            ${pkgs.coreutils}/bin/date > "$HOME/Maildir/mbsync-start-post-exec"
            ${
              if config.programs.notmuch.enable then
                "${pkgs.notmuch}/bin/notmuch new --no-hooks --verbose"
              else
                ""
            }
            ${if config.programs.mu.enable then "${pkgs.mu}/bin/mu index" else ""}
          '';
        in
        "${mbsync-postExec}/bin/mbsync-postExec";
    };
  };
}