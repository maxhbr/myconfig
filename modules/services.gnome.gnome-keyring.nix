{ pkgs, config, lib, ... }: {
  config = (lib.mkIf config.services.gnome.gnome-keyring.enable {
    services.gnome = {
      gnome-settings-daemon.enable = lib.mkDefault true;
    };
    environment.systemPackages = with pkgs; [ gcr ];

    programs.dconf.enable = lib.mkDefault true;
    programs.seahorse.enable = lib.mkDefault true;
  });
}
