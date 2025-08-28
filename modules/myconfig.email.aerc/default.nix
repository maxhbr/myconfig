let 
  hm = { config, lib, pkgs, ...  }:
        {
          config = {
            home.packages = with pkgs; [
              w3m
              (writeShellScriptBin "aerc-tmux" ''
                if ${tmux}/bin/tmux has-session -t "aerc" 2>/dev/null; then
                  ${tmux}/bin/tmux attach -d -t "aerc" \; \
                      split-window -l 2 '${config.programs.mbsync.package}/bin/mbsync --all && exit' \; \
                      last-pane
                else
                  ${tmux}/bin/tmux -2 \
                      new-session -s "aerc" "command ${config.programs.aerc.package}/bin/aerc" \; \
                      set-option status \; \
                      set set-titles-string "aerc@tmux" \; \
                      split-window -l 2 '${config.programs.mbsync.package}/bin/mbsync --all && exit' \; \
                      last-pane \;
                fi
              '')
              (writeShellScriptBin "aerc-foot" ''
                exec ${foot}/bin/foot \
                  -T aerc-foot \
                  -a aerc-foot \
                  aerc-tmux
              '')
              (writeShellScriptBin "aerc-alacritty" ''
                exec ${alacritty}/bin/alacritty \
                  --title aerc-alacritty \
                  --class aerc-alacritty \
                  --command aerc-tmux
              '')
            ];
            programs.aerc = {
              enable = true;
              # see for example https://man.archlinux.org/man/aerc-config.5.en
              extraConfig = {
                general = {
                  unsafe-accounts-conf = true;
                };
                ui = {
                  mouse-enabled = true;
                  # reverse-thread-order = true;
                  threading-enabled = true;
                };
                viewer = {
                  pager = "${pkgs.less}/bin/less -Rc";
                  show-headers = false;
                  always-show-mime = true;
                };
                filters = {
                  "text/plain" = "${pkgs.aerc}/libexec/aerc/filters/colorize";
                  "text/calendar" = "${pkgs.gawk}/bin/awk -f ${pkgs.aerc}/libexec/aerc/filters/calendar";
                  "text/html" =
                    "${pkgs.aerc}/libexec/aerc/filters/html | ${pkgs.aerc}/libexec/aerc/filters/colorize"; # Requires w3m, dante
                  # "text/*" =
                  #   ''${pkgs.bat}/bin/bat -fP --file-name="$AERC_FILENAME "'';
                  "message/delivery-status" = "${pkgs.aerc}/libexec/aerc/filters/colorize";
                  "message/rfc822" = "${pkgs.aerc}/libexec/aerc/filters/colorize";
                  "application/x-sh" = "${pkgs.bat}/bin/bat -fP -l sh";
                  "application/pdf" = "${pkgs.zathura}/bin/zathura -";
                  "audio/*" = "${pkgs.mpv}/bin/mpv -";
                  "image/*" = "${pkgs.feh}/bin/feh -";
                };
                compose = {
                  # editor = config.home.sessionVariables.EDITOR;
                  # address-book-cmd = "aba ls \"%s\"";
                  empty-subject-warning = true;
                };
              };
            };
            myconfig.desktop.wayland.launcherCommands = [ "aerc-alacritty" ];
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
  config = lib.mkIf (cfg.email.enable && (builtins.elem "aerc" cfg.email.clients)) {
    home-manager.sharedModules = [ hm ];
  };
}
