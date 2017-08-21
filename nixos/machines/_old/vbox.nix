{ config, pkgs, ... }:

{
  imports = [
    ## hardware:
    ./hardware/grub.nix

    ./vm.nix

    ## software:
    # ../profiles/dev.nix
    ../profiles/virtualization/docker.nix
  ];
}
