{
  config,
  lib,
  pkgs,
  ...
}:
let
  # from https://codeberg.org/dnkl/foot/src/branch/master/themes
  theme = pkgs.writeText "foot-theme" ''
    # -*- conf -*-
    # PaperColor Light
    # Palette based on https://github.com/NLKNguyen/papercolor-theme

    [cursor]
    color=eeeeee 444444

    [colors]
    background=eeeeee
    foreground=444444
    regular0=eeeeee  # black
    regular1=af0000  # red
    regular2=008700  # green
    regular3=5f8700  # yellow
    regular4=0087af  # blue
    regular5=878787  # magenta
    regular6=005f87  # cyan
    regular7=764e37  # white
    bright0=bcbcbc   # bright black
    bright1=d70000   # bright red
    bright2=d70087   # bright green
    bright3=8700af   # bright yellow
    bright4=d75f00   # bright blue
    bright5=d75f00   # bright magenta
    bright6=4c7a5d   # bright cyan
    bright7=005faf   # bright white
    # selection-foreground=eeeeee
    # selection-background=0087af
  '';
  footclient =
    if config.programs.foot.server.enable then
      "${config.programs.foot.package}/bin/footclient"
    else
      "${config.programs.foot.package}/bin/foot";
in
{
  config = (
    lib.mkIf config.programs.foot.enable {
      programs.foot = {
        server.enable = false; # footclient fails to find comands like grep
        settings = {
          main = {
            font = "monospace:size=10";
            dpi-aware = "no"; # "yes";
            include = "${theme}";
          };
          mouse = {
            hide-when-typing = "yes";
          };
          colors = {
            alpha = "0.85";
          };
        };
      };
      home.packages =
        with pkgs;
        [
          (writeShellScriptBin "tfoot" ''
            exec ${footclient} ${tmux}/bin/tmux
          '')
          (
            let
              tmux-scratch = writeShellScriptBin "tmux-scratch" ''
                NAME="tmux-scratch"
                tmux has-session -t $NAME 2>/dev/null
                [[ "$?" -eq 1 ]] && tmux new-session -d -s $NAME
                tmux attach-session -t $NAME
              '';
            in
            writeShellScriptBin "foot-scratch" ''
              exec ${footclient} \
                -T tmux-scratch \
                -a tmux-scratch \
                ${tmux-scratch}/bin/tmux-scratch
            ''
          )
          (writeShellScriptBin "tfoot-reattach" ''
            ${tmux}/bin/tmux ls |
                ${gnugrep}/bin/grep -v '(attached)' |
                cut -f 1 -d ":" |
                while read SESSION; do
                    (set -x;
                    ${footclient} ${tmux}/bin/tmux attach -t "$SESSION" & disown)
                done
          '')
        ]
        ++ (
          let
            mkFootTuiCmd =
              cmd: package:
              (writeShellScriptBin "foot-${cmd}" ''
                exec ${footclient} \
                  -T foot-${cmd} \
                  -a foot-${cmd} \
                  ${package}/bin/${cmd}
              '');
          in
          [
            (mkFootTuiCmd "bluetuith" bluetuith)
            (mkFootTuiCmd "htop" htop)
            (mkFootTuiCmd "btop" btop)
            (mkFootTuiCmd "nmtui" networkmanager)
          ]
        );
      myconfig.desktop.wayland.launcherCommands = [
        "foot"
        "tfoot"
        "tfoot-reattach"
        "foot-scratch"
        "foot-bluetuith"
        "foot-htop"
        "foot-btop"
        "foot-nmtui"
      ];
    }
  );
}
