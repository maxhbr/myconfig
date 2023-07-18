{ config, lib, pkgs, ... }: let
# from https://codeberg.org/dnkl/foot/src/branch/master/themes
  theme = pkgs.writeText "foot-theme" ''
# -*- conf -*-
# Material Amber
# Based on material.io guidelines with Amber 50 background

[cursor]
color=fff8e1 21201d

[colors]
foreground = 21201d
background = fff8e1

regular0 = 21201d # black
regular1 = cd4340 # red
regular2 = 498d49 # green
regular3 = fab32d # yellow
regular4 = 3378c4 # blue
regular5 = b83269 # magenta
regular6 = 21929a # cyan
regular7 = ffd7d7 # white

bright0 = 66635a # bright black
bright1 = dd7b72 # bright red
bright2 = 82ae78 # bright green
bright3 = fbc870 # bright yellow
bright4 = 73a0cd # bright blue
bright5 = ce6f8e # bright magenta
bright6 = 548c94 # bright cyan
bright7 = ffe1da # bright white

dim0 = 9e9a8c # dim black
dim1 = e9a99b # dim red
dim2 = b0c99f # dim green
dim3 = fdda9a # dim yellow
dim4 = a6c0d4 # dim blue
dim5 = e0a1ad # dim magenta
dim6 = 3c6064 # dim cyan
dim7 = ffe9dd # dim white
'';
in {
  config = (lib.mkIf config.programs.foot.enable {
    programs.foot = {
      server.enable = true;
      settings =  {
        main = {
          font = "monospace:size=6";
          dpi-aware = "yes";
          include = "${theme}";
        };
        colors = {
          alpha = "0.85";
        };
      };
    };
    home.packages = with pkgs; [
      (writeShellScriptBin "tfoot" ''
        exec ${foot}/bin/foot ${tmux}/bin/tmux
      '')
      (let
        tmux-scratch = writeShellScriptBin "tmux-scratch" ''
          NAME="tmux-scratch"
          tmux has-session -t $NAME 2>/dev/null
          [[ "$?" -eq 1 ]] && tmux new-session -d -s $NAME
          tmux attach-session -t $NAME
        '';
      in writeShellScriptBin "foot-scratch" ''
        exec ${foot}/bin/foot \
          -T tmux-scratch \
          -a tmux-scratch \
          ${tmux-scratch}/bin/tmux-scratch
      '')
      (writeShellScriptBin "tfoot-reattach" ''
        ${tmux}/bin/tmux ls |
            ${gnugrep}/bin/grep -v '(attached)' |
            cut -f 1 -d ":" |
            while read SESSION; do
                (set -x;
                ${foot}/bin/foot ${tmux}/bin/tmux attach -t "$SESSION" & disown)
            done
      '')
      (writeShellScriptBin "tfoot-bluetuith" ''
        exec ${foot}/bin/foot \
          -T foot-bluetuith \
          -a foot-bluetuith \
          ${bluetuith}/bin/bluetuith
      '')
    ];
  });
}
