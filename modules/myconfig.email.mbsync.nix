{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.myconfig;

  mbsync-get-status = pkgs.writeShellScriptBin "mbsync-get-status" ''
    tsNow=$(date +%s)

    if [[ ! -f "$HOME/Maildir/mbsync.preExec.timestamp" ]]; then
      echo '{"text": "🖂✗", "tooltip": "mbsync not-starting","class":"error"}'
      exit 0
    fi
    tsPreExec=$(cat "$HOME/Maildir/mbsync.preExec.timestamp")
    timeSinceLastStart=$((tsNow - tsPreExec))
    # >&2 echo "timeSinceLastStart: $timeSinceLastStart"

    if [[ ! -f "$HOME/Maildir/mbsync.postExec.start.timestamp" ]]; then
      if [[ "$timeSinceLastStart" -lt 30 ]]; then
        exit 0
      fi
      echo '{"text": "🖂✗", "tooltip": "mbsync not-syncing","class":"error"}'
      exit 0
    fi
    tsPostExecStart=$(cat "$HOME/Maildir/mbsync.postExec.start.timestamp")
    timeSinceLastHookStart=$((tsNow - tsPostExecStart))
    # >&2 echo "timeSinceLastHookStart: $timeSinceLastHookStart"

    if [[ ! -f "$HOME/Maildir/mbsync.postExec.end.timestamp" ]]; then
      if [[ "$timeSinceLastStart" -lt 30 ]]; then
        exit 0
      fi
      echo '{"text": "🖂✗", "tooltip": "mbsync not-indexing","class":"error"}'
      exit 0
    fi
    tsPostExecEnd=$(cat "$HOME/Maildir/mbsync.postExec.end.timestamp")
    timeSinceLastHookEnd=$((tsNow - tsPostExecEnd))
    # >&2 echo "timeSinceLastHookEnd: $timeSinceLastHookEnd"

    if [[ "$tsPostExecStart" -ge "$tsPreExec" ]]; then
      # sync was successful
      if [[ "$tsPostExecEnd" -ge "$tsPostExecStart" ]]; then
        # hook was successful
        if [[ "$timeSinceLastStart" -lt 3000 ]]; then
          echo '{"text": "🖂✓", "tooltip": "mbsync successful","class":"success"}'
          exit 0
        else
          echo '{"text": "🖂!", "tooltip": "mbsync stale, since '"$timeSinceLastStart"'s","class":"warning"}'
          exit 0
        fi
      else
        echo '{"text": "🖂!", "tooltip": "mbsync hook-failed, since '"$timeSinceLastHookEnd"'s","class":"warning"}'
        exit 0
      fi
    else
      echo '{"text": "🖂✗", "tooltip": "mbsync sync-failed, since '"$timeSinceLastHookStart"'s","class":"error"}'
      exit 0
    fi
  '';
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
              verbose = false;
              package = config.programs.mbsync.package;
              preExec =
                let
                  mbsync-preExec = pkgs.writeShellScriptBin "mbsync-preExec" ''
                    echo "$(${pkgs.coreutils}/bin/date +%s)" > "$HOME/Maildir/mbsync.preExec.timestamp"
                  '';
                in
                "${mbsync-preExec}/bin/mbsync-preExec";
              postExec =
                let
                  mbsync-postExec = pkgs.writeShellScriptBin "mbsync-postExec" ''
                    echo "$(${pkgs.coreutils}/bin/date +%s)" > "$HOME/Maildir/mbsync.postExec.start.timestamp"
                    ${
                      if config.programs.notmuch.enable then
                        "${pkgs.notmuch}/bin/notmuch new --no-hooks --verbose >/tmp/mbsync.notmuch.log"
                      else
                        ""
                    }
                    ${if config.programs.mu.enable then "${pkgs.mu}/bin/mu index >/tmp/mbsync.mu.log" else ""}
                    echo "$(${pkgs.coreutils}/bin/date +%s)" > "$HOME/Maildir/mbsync.postExec.end.timestamp"
                  '';
                in
                "${mbsync-postExec}/bin/mbsync-postExec";
            };
            home.packages = with pkgs; [
              mbsync-get-status
            ];
            programs.waybar = {
              settings = {
                mainBar = {
                  modules-center = [
                    "custom/mbsync-get-status"
                  ];
                  "custom/mbsync-get-status" = {
                    format = "{}";
                    exec = "${mbsync-get-status}/bin/mbsync-get-status";
                    return-type = "json";
                    interval = 30;
                    # rotation = 90;
                    on-click = "alacritty -e journalctl --user -f --unit mbsync.service";
                  };
                };
              };
            };
          };
        }
      )
    ];
  };
}
