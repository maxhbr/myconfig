{ pkgs, lib, config, ... }: {
  imports = [
    ../role.core
    # modules
    # ./desktop.Wayland.sway
    # ./desktop.X.xfce.nix
    ./desktop.X.xmonad.nix
    ./pulseaudio
    ./emacs
  ];
  boot.kernelPackages = pkgs.linuxPackages_latest;
}
