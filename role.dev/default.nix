{ pkgs, ...}: {
  imports = [
    ../role.desktop
    # modules
    ./dev.core
    ./dev.haskell
    ./dev.iot.nix
    ./dev.network.nix
    # ./dev.tex.nix
    ./programs.license-compliance-toolbox.nix
    ./virtualization.docker
    ./virtualization.qemu
    ./virtualization.vbox
    # ./virtualization.lxc.nix
  ];
}
