{ pkgs, ...}: {
  imports = [
    ./desktop.nix
    # modules
    ../modules/dev.core
    ../modules/dev.haskell
    ../modules/dev.iot.nix
    ../modules/dev.python.nix
    ../modules/dev.network.nix
    ../modules/virtualization.docker
    ../modules/virtualization.qemu.nix
    ../modules/virtualization.vbox
    # ../modules/virtualization.lxc.nix
    ../modules/programs.license-compliance-toolbox.nix
  ];
}
