{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.myconfig;

  mbsync-tmux-session = "mbsync";
  mbsync-tmux-session-script = pkgs.writeShellScriptBin "mbsync-tmux-session" ''
    if tmux has-session -t ${mbsync-tmux-session}; then
      tmux kill-session -t ${mbsync-tmux-session} || true
    fi
    if ! tmux has-session -t ${mbsync-tmux-session}; then
      tmux new-session -d -s ${mbsync-tmux-session}
      tmux send-keys -t ${mbsync-tmux-session}:1 "journalctl -f --user -u mbsync.service" C-m
      tmux split-window -v -t ${mbsync-tmux-session}
      tmux send-keys -t ${mbsync-tmux-session}:1 "systemctl --user restart mbsync.service" C-m
      tmux split-window -v -t ${mbsync-tmux-session}
      tmux send-keys -t ${mbsync-tmux-session}:1 "watch ${mbsync-get-status}/bin/mbsync-get-status verbose" C-m
    fi
    exec tmux attach-session -t ${mbsync-tmux-session}
  '';
  mbsync-get-status = pkgs.writeShellScriptBin "mbsync-get-status" ''
    set -euo pipefail
    if [[ "$#" -gt 0 && "$1" == "verbose" ]]; then
      VERBOSE="verbose"
    else
      VERBOSE=""
    fi
    tsNow=$(date +%s)

    if [[ ! -f "$HOME/Maildir/mbsync.preExec.timestamp" ]]; then
      echo '{"text": "🖂✗", "tooltip": "mbsync not-starting","class":"error"}'
      exit 0
    fi
    tsPreExec=$(cat "$HOME/Maildir/mbsync.preExec.timestamp")
    timeSinceLastStart=$((tsNow - tsPreExec))
    if [[ "$VERBOSE" == "verbose" ]]; then
      >&2 echo "timeSinceLastStart: $timeSinceLastStart @ $tsPreExec"
    fi

    if [[ ! -f "$HOME/Maildir/mbsync.postExec.start.timestamp" ]]; then
      if [[ "$timeSinceLastStart" -lt 30 ]]; then
        exit 0
      fi
      echo '{"text": "🖂✗", "tooltip": "mbsync not-syncing","class":"error"}'
      exit 0
    fi
    tsPostExecStart=$(cat "$HOME/Maildir/mbsync.postExec.start.timestamp")
    timeSinceLastHookStart=$((tsNow - tsPostExecStart))
    if [[ "$VERBOSE" == "verbose" ]]; then
      >&2 echo "timeSinceLastHookStart: $timeSinceLastHookStart @ $tsPostExecStart"
    fi

    if [[ ! -f "$HOME/Maildir/mbsync.postExec.end.timestamp" ]]; then
      if [[ "$timeSinceLastStart" -lt 30 ]]; then
        exit 0
      fi
      echo '{"text": "🖂✗", "tooltip": "mbsync not-indexing","class":"error"}'
      exit 0
    fi
    tsPostExecEnd=$(cat "$HOME/Maildir/mbsync.postExec.end.timestamp")
    timeSinceLastHookEnd=$((tsNow - tsPostExecEnd))
    if [[ "$VERBOSE" == "verbose" ]]; then
      >&2 echo "timeSinceLastHookEnd: $timeSinceLastHookEnd @ $tsPostExecEnd"
    fi

    if [[ "$timeSinceLastHookEnd" -lt 300 ]]; then
      echo '{"text": "🖂✓", "tooltip": "mbsync successful","class":"okay"}'
      exit 0
    fi

    if [[ "$tsPostExecStart" -ge "$tsPreExec" ]]; then
      if [[ "$VERBOSE" == "verbose" ]]; then
        >&2 echo "...sync was successful"
      fi
      if [[ "$tsPostExecEnd" -ge "$tsPostExecStart" ]]; then
        if [[ "$VERBOSE" == "verbose" ]]; then
          >&2 echo "...hook was successful"
        fi
        if [[ "$timeSinceLastStart" -lt 3000 ]]; then
          echo '{"text": "🖂✓", "tooltip": "mbsync successful","class":"okay"}'
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
              mbsync-tmux-session-script
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
                    interval = 5;
                    # rotation = 90;
                    on-click = "alacritty -e ${mbsync-tmux-session-script}/bin/mbsync-tmux-session";
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
