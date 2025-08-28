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
  config = lib.mkIf (cfg.email.enable && (cfg.email.syncer == "mbsync")) {
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
            programs.mbsync.enable = true;
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
      )
    ];
  };
}
