{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    imageworkEnv
  ];
}
