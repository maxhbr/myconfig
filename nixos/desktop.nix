{...}: {
  imports = [
    ./base.nix
    # modules
    ./modules/desktop.Wayland.sway
    ./modules/desktop.X.xfce.nix
    ./modules/desktop.X.xmonad
    ./modules/pulseaudio

    ./modules/service.syncthing.nix
  ];
}
