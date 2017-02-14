{ config, pkgs, ... }:

{
  imports = [
    ## hardware:
    ./hardware/grub.nix

    ## software:
    # ../profiles/dev.nix
    ../profiles/virtualization/docker.nix
    ../profiles/openssh.nix
  ];
}
