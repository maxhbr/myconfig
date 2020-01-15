{...}: {
  imports = [
    ../emacs
    ./Wayland.sway
    ./X.xfce.nix
    ./X.xmonad
    ./pulseaudio.nix
  ];
}
