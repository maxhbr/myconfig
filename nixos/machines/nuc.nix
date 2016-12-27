{ config, pkgs, ... }:

{
  imports = [
    ## hardware:
    ./hardware/efi.nix
    
    ## software:
    ../profiles/virtualization.nix
    ../profiles/openssh.nix
    ../profiles/terminal.nix
    # ../profiles/vsftp.nix
  ];

  # services.xservervideoDrivers = [ "intel" ];
}
