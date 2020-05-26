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
  ];
  config = {
    environment.systemPackages = [
      (pkgs.callPackage ../pkgs/license-compliance-toolbox { inherit pkgs; })
    ];
  };
}
