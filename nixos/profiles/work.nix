{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [ workEnv ];
}
