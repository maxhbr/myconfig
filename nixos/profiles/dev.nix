{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [ devEnv ];
}
