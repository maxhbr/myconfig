# see:
# https://discourse.nixos.org/t/running-evolution-without-gnome-is-it-sane-possible/8328
# https://github.com/NixOS/nixpkgs/issues/12756
# https://github.com/NixOS/nixpkgs/pull/17926/files
{ config, lib, pkgs, ... }:
let user = config.myconfig.user;
in {
  config = lib.mkIf
    (config.programs.evolution.enable && config.myconfig.desktop.enable) {
      services.gnome = {
        evolution-data-server.enable = lib.mkDefault true;
        gnome-keyring.enable = lib.mkDefault true;
        # # optional to use google/nextcloud calendar
        # gnome-online-accounts.enable = lib.mkDefault true;
      };
      programs.dconf.enable = true;
      # programs.seahorse.enable = true;

      # nixpkgs.overlays = [
      #   (self: super: {
      #     # Spamassasin tests currently fail:
      #     # TODO: remove
      #     spamassassin = super.spamassassin.overrideAttrs (old: {
      #       doCheck = false;
      #       doInstallCheck = false;
      #     });
      #   })
      # ];
      myconfig.persistence.directories =
        [ ".local/share/evolution" ".config/evolution" ];
      myconfig.persistence.cache-directories = [ ".cache/evolution" ];
    };
}
