{ config, pkgs, ... }:

{
  services.printing = {
    enable = true;
    drivers = [ pkgs.gutenprint pkgs.hplip ];
  };
}
