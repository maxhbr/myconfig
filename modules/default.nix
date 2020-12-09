{ config, lib, pkgs, ... }:
let
  user = config.myconfig.user;
  modules = [ # modules
       ./core.nix
       ./gnupg.nix
       ./make-linux-fast-again.nix
       ./mybackup.nix
       ./myconfig.service.deconz.nix
       ./nixos.gc.nix
       ./nixos.networking
       ./nixos.nix.nix
       ./nixos.user.nix
       ./programs.emacs
       ./programs.pass
       ./services.netdata.nix
       ./services.openssh.nix
       ./services.postgresql.nix
       ./services.syncthing.nix
       ./services.vsftp.nix
       ./services.xserver.autorandr.nix
       ./services.xserver.big-cursor.nix
       ./services.xserver.fonts
       ./services.xserver.kernel.nix
       ./services.xserver.mkscreenshot.nix
       ./services.xserver.my-wallpapers
       ./services.xserver.nix
       ./services.xserver.printing.nix
       ./services.xserver.programs.chromium.nix
       ./services.xserver.programs.obs.nix
       ./services.xserver.programs.xss-lock.nix
       ./services.xserver.pulseaudio
       ./services.xserver.st
       ./services.xserver.xclip.nix
       ./services.xserver.xmonad
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
