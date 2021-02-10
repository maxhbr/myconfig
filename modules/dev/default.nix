{ pkgs, lib, config, ... }:
let cfg = config.myconfig.dev;
in {
  options.myconfig.dev = with lib; {
    enable = mkEnableOption "myconfig.dev";
    haskell.enable = mkEnableOption "myconfig.dev.haskell";
    iot.enable = mkEnableOption "myconfig.dev.iot";
    network.enable = mkEnableOption "myconfig.dev.network";
    tex.enable = mkEnableOption "myconfig.dev.tex";
    compliance.enable = mkEnableOption "myconfig.dev.compliance";
  };

  imports = [
    ./dev.core
    ./dev.haskell
    ./dev.iot.nix
    ./dev.network.nix
    ./dev.tex.nix
    ./programs.license-compliance-toolbox
  ];
  config = {
    myconfig.dev.enable = cfg.haskell.enable || cfg.iot.enable
      || cfg.network.enable || cfg.tex.enable || cfg.compliance.enable;
  };
}
