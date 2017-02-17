{pkgs}:
with pkgs; let
  name = "cliEnv";
  paths = [
    ranger
    emacs vim
    elinks w3m
    tmux
    manpages
    taskwarrior
    pass
  ];
in buildEnv { inherit name paths; }
