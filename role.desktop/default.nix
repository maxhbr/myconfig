{ pkgs, lib, config, ... }: {
  imports = [
    ../modules
    # modules
    # ./desktop.Wayland.sway
    # ./desktop.X.xfce.nix
    ./desktop.X.xmonad
    ./pulseaudio
    ./emacs
  ];
  boot.kernelPackages = pkgs.linuxPackages_latest;
}
