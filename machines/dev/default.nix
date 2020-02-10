{...}: {
  imports = [
    ../desktop
    # modules
    ./modules/dev.nix
    ./modules/dev.haskell
    ./modules/dev.iot.nix
    ./modules/dev.python.nix
    ./modules/virtualization.docker
    ./modules/virtualization.qemu.nix
    ./modules/virtualization.vbox
  ];
}
