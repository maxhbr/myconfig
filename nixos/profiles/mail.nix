{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [ muttEnv ];

  services.offlineimap = {
    enable = true;
  };
}
