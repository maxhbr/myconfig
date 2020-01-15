{...}: {
  imports = [
    ../emacs
    ./desktop.Wayland.sway
    ./desktop.X.xfce.nix
    ./desktop.X.xmonad
  ];
}
