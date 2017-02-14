{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    vagrant
  ];

  virtualisation.virtualbox.host.enable = true;
  # users.extraGroups.vboxusers.members = [ "mhuber" ];
}
