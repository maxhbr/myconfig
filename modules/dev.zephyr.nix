{
  inputs,
  pkgs,
  config,
  lib,
  ...
}@args:
let
  cfg = config.myconfig;
in
{
  options.myconfig = with lib; {
    dev.zephyr.enable = mkEnableOption "zephyr";
  };
  config = (lib.mkIf (cfg.dev.zephyr.enable) (inputs.zephyr-flake.nixosModules.zephyr args));
}
