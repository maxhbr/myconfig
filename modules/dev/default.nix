{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.myconfig.dev;
in
{
  options.myconfig.dev = with lib; {
    enable = mkEnableOption "myconfig.dev";
    compliance.enable = mkEnableOption "myconfig.dev.compliance";
    elixir.enable = mkEnableOption "myconfig.dev.elixir";
    embedded.enable = mkEnableOption "myconfig.dev.embedded";
    go.enable = mkEnableOption "myconfig.dev.go";
    haskell.enable = mkEnableOption "myconfig.dev.haskell";
    network.enable = mkEnableOption "myconfig.dev.network";
    nodejs.enable = mkEnableOption "myconfig.dev.nodejs";
    python.enable = mkEnableOption "myconfig.dev.python";
    ruby.enable = mkEnableOption "myconfig.dev.ruby";
    rust.enable = mkEnableOption "myconfig.dev.rust";
    tex.enable = mkEnableOption "myconfig.dev.tex";
  };

  imports = [
    ./dev.core
    ./dev.elixir.nix
    ./dev.embedded.nix
    ./dev.go.nix
    ./dev.haskell
    ./dev.network.nix
    ./dev.nodejs.nix
    ./dev.python.nix
    ./dev.ruby.nix
    ./dev.rust.nix
    ./dev.tex.nix
  ];
  config = {
    myconfig.dev.enable =
      cfg.elixir.enable
      || cfg.embedded.enable
      || cfg.haskell.enable
      || cfg.network.enable
      || cfg.tex.enable
      || cfg.compliance.enable
      || cfg.go.enable
      || cfg.ruby.enable
      || cfg.python.enable
      || cfg.rust.enable
      || cfg.nodejs.enable;
  };
}
