{
  pkgs,
  lib,
  config,
  ...
}:
{
  config = (
    lib.mkIf config.services.xserver.enable {
      boot.kernelPackages = pkgs.linuxPackages_latest;
    }
  );
}
