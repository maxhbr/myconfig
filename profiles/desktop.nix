{...}: {
  imports = [
    ../modules/emacs
    ../modules/desktop.Wayland.sway
    ../modules/desktop.X.xfce.nix
    ../modules/desktop.X.xmonad
  ];
}
