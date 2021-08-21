{ pkgs, lib, config, ... }:
let cfg = config.myconfig.dev;
in {
  options.myconfig.dev = with lib; {
    enable = mkEnableOption "myconfig.dev";
    haskell.enable = mkEnableOption "myconfig.dev.haskell";
    go.enable = mkEnableOption "myconfig.dev.go";
    ruby.enable = mkEnableOption "myconfig.dev.ruby";
    network.enable = mkEnableOption "myconfig.dev.network";
    tex.enable = mkEnableOption "myconfig.dev.tex";
    compliance.enable = mkEnableOption "myconfig.dev.compliance";
    nodejs.enable = mkEnableOption "myconfig.dev.nodejs";
  };

  imports = [
    ./dev.core
    ./dev.haskell
    ./dev.network.nix
    ./dev.tex.nix
    ./dev.go.nix
    ./dev.ruby.nix
    ./dev.nodejs.nix
    ./programs.license-compliance-toolbox.nix
  ];
  config = {
    myconfig.dev.enable = cfg.haskell.enable || cfg.network.enable
      || cfg.tex.enable || cfg.compliance.enable
      || cfg.go.enable || cfg.ruby.enable || cfg.nodejs.enable;
  };
}
