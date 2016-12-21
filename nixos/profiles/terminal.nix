{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    emacs vim
    elinks w3m
    tmux
    htop iftop iotop
    mkpasswd
    manpages
    taskwarrior
    pass
  ];
}
