{
  pkgs,
  config,
  lib,
  ...
}:
{
  config = (
    lib.mkIf config.services.gnome.gnome-keyring.enable {
      services.gnome = {
        gnome-settings-daemon.enable = lib.mkDefault true;
      };
      # `gcr` was removed from nixpkgs (2026-09-03); an explicit ABI version is
      # required now. gnome-keyring in this nixpkgs still propagates gcr_3, and
      # only gcr_3 ships the `gcr-prompter` / `gcr-viewer` UI components
      # (gcr_4 only provides `gcr-viewer-gtk4` and the gcr-ssh-agent).
      environment.systemPackages = with pkgs; [ gcr_3 ];

      programs.dconf.enable = lib.mkDefault true;

      # programs.seahorse.enable = lib.mkDefault true;

      # impermanence already stores ".local/share/keyrings" via other config^
    }
  );
}
