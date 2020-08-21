{ pkgs, lib, config, ...}:
{ imports = [
    ../role.core
    # modules
    # ./desktop.Wayland.sway
    # ./desktop.X.xfce.nix
    ./desktop.X.xmonad.nix
    ./pulseaudio
    ./emacs
  ];
  config = lib.mkIf (! config.virtualisation.virtualbox.host.enable) {
    boot.kernelPackages = pkgs.linuxPackages_latest;
  };
}
