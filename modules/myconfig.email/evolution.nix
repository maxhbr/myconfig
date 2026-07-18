# see:
# https://discourse.nixos.org/t/running-evolution-without-gnome-is-it-sane-possible/8328
# https://github.com/NixOS/nixpkgs/issues/12756
# https://github.com/NixOS/nixpkgs/pull/17926/files
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig;
in
{
  # NOTE: dead code — `cfg.email.clients` is a `listOf` (see
  # modules/myconfig.email/default.nix), so the comparison
  # `cfg.email.clients == "evolution"` always evaluates to false and this
  # block never applies. If this is ever fixed (e.g. to
  # `builtins.elem "evolution" cfg.email.clients`), a main-user guard
  # (`config.home.username == myconfig.user`) will be needed here too,
  # since this is NixOS-level config, not a home-manager sharedModule.
  config = lib.mkIf (cfg.desktop.enable && cfg.email.enable && (cfg.email.clients == "evolution")) {
    programs.evolution.enable = true;
    services.gnome = {
      evolution-data-server.enable = lib.mkDefault true;
      gnome-keyring.enable = lib.mkDefault true;
    };
    programs.dconf.enable = true;
    myconfig.persistence.directories = [
      ".local/share/evolution"
      ".config/evolution"
      ".local/share/org.gnome.Evolution"
    ];
    myconfig.persistence.cache-directories = [ ".cache/evolution" ];
    myconfig.desktop.wayland.launcherCommands = [ "evolution" ];
  };
}
