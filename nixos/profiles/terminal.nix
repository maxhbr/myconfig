{ config, pkgs, ... }:

let
  unstable = (import <unstable> {});
in {
  environment.systemPackages = with pkgs; [
    emacs vim
    elinks w3m
    unstable.tmux
    unstable.htop iftop iotop
    mkpasswd
    manpages
    taskwarrior
    pass
  ];
}
