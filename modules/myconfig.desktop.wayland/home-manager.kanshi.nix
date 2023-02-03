{ config, lib, pkgs, ... }:

{
  config = lib.mkIf (config.services.kanshi.enable) {
    # services.kanshi = { systemdTarget = "default.target"; };
  };
}
