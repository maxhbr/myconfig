{...}: {
  imports = [
    ../base
    # modules
    ./modules/Wayland.sway
    ./modules/X.xfce.nix
    ./modules/X.xmonad
    ./modules/pulseaudio
    
    ./modules/service.syncthing.nix
  ];
}
