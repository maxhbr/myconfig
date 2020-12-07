{ pkgs, lib, config, ... }: {
  imports = [
    ../modules
    # modules
    # ./desktop.Wayland.sway
    # ./desktop.X.xfce.nix
    ./desktop.X.xmonad
  ];
  boot.kernelPackages = pkgs.linuxPackages_latest;
}
