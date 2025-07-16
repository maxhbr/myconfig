{config, lib, pkgs, ...}:
{
  config = lib.mkIf config.services.clamav.scanner.enable {
    environment.systemPackages = [
      pkgs.clamav
    ];
    services.clamav = {
      daemon.enable = true;
      updater.enable = true;
      fangfrisch.enable = true;
    };
  };
}