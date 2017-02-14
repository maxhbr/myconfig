{ config, pkgs, ... }:

{
  imports = [
    ## hardware:
    ./hardware/grub.nix

    ## software:
    # ../profiles/dev.nix
    # ../profiles/virtualization.nix
    ../profiles/openssh.nix
  ];
}
