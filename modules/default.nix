{ config, lib, pkgs, ... }:
let
  user = config.myconfig.user;
  modules = [ # modules
       ./core.nix
       ./desktop.autorandr.nix
       ./desktop.big-cursor.nix
       ./desktop.fonts
       ./desktop.kernel.nix
       ./desktop.mkscreenshot.nix
       ./desktop.my-wallpapers
       ./desktop.nix
       ./desktop.printing.nix
       ./desktop.programs.chromium.nix
       ./desktop.programs.obs.nix
       ./desktop.programs.xss-lock.nix
       ./desktop.pulseaudio
       ./desktop.st
       ./desktop.xclip.nix
       ./desktop.xmonad
       ./gnupg.nix
       ./make-linux-fast-again.nix
       ./mybackup.nix
       ./myconfig.service.deconz.nix
       ./nixos.gc.nix
       ./nixos.networking
       ./nixos.nix.nix
       ./nixos.user.nix
       ./pass
       ./programs.emacs
       ./service.openssh.nix
       ./service.postgresql.nix
       ./service.syncthing.nix
       ./shell.common
       ./shell.dic.nix
       ./shell.fish
       ./shell.git
       ./shell.tmux
       ./shell.vim
       ./shell.zsh
       ./virtualization.docker
       ./virtualization.lxc.nix
       ./virtualization.qemu.nix
       ./virtualization.vbox.nix
  ];
  hm-modules = [# home-manager modules
    ./desktop.programs.firefox.hm.nix
    ./desktop.programs.zathura.hm.nix
  ];
in {
  imports = [./lib ./myconfig] ++ modules;
  config = {
    home-manager.users."${user}" = {
      imports = hm-modules;
    };
    assertions = [
      {
        assertion = config.networking.hostId != null;
        message = "config.networking.hostId should be set!";
      }
      {
        assertion = config.networking.hostName != "nixos";
        message = "config.networking.hostName should be set!";
      }
    ];
  };
}
