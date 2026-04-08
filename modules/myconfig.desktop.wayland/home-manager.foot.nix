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

    [colors]
    cursor=eeeeee 444444
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
        let
          tmux-session =
            name:
            {
              initialCommand ? "",
            }:
            writeShellScriptBin "${name}-tmux" ''
              tmux has-session -t ${name} 2>/dev/null
              [[ "$?" -eq 1 ]] && tmux new-session -d -s ${name} ${initialCommand}

              if [[ -n "$TMUX" ]]; then
                tmux switch-client -t ${name}
              else
                tmux attach-session -t ${name}
              fi
            '';
          foot-tmux-session =
            name: args:
            writeShellScriptBin "foot-${name}" ''
              exec ${footclient} \
                -T tmux-${name} \
                -a tmux-${name} \
                ${tmux-session name args}/bin/${name}-tmux
            '';
        in
        [
          (writeShellScriptBin "tfoot" ''
            exec ${footclient} ${tmux}/bin/tmux
          '')
          (tmux-session "zero" { initialCommand = "${btop}/bin/btop"; })
          (foot-tmux-session "zero" { initialCommand = "${btop}/bin/btop"; })
          (
            writeShellScriptBin "foot-host-tmux-session" ''
              exec ${footclient} \
                -T foot-host-tmux-session \
                -a foot-host-tmux-session \
                host-tmux-session
            '')
          (foot-tmux-session "scratch" { })
          (tmux-session "greetd" { })
          (foot-tmux-session "greetd" { })
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
        "foot-zero"
        "foot-scratch"
        "tfoot"
        "tfoot-reattach"
        "foot-bluetuith"
        "foot-htop"
        "foot-btop"
        "foot-nmtui"
      ];
    }
  );
}
