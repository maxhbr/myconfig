{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    hplipWithPlugin
  ];
  services.printing = {
    enable = true;
    drivers = [ pkgs.gutenprint pkgs.hplip ];
  };
}
