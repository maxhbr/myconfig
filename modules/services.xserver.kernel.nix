{ pkgs, lib, config, ... }: {
  config = (lib.mkIf config.services.xserver.enable {
    boot.kernelPackages = pkgs.linuxPackages_testing; # TODO: back to latest, once the thunderbolt dock problem is fixed
  });
}
