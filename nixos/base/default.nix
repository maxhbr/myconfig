{ pkgs, lib, config, ...}: {
  imports = [
    ../core
    # modules
    ./modules/emacs
    ./modules/mail
  ];
  config = lib.mkIf (! config.virtualisation.virtualbox.host.enable) {
    boot.kernelPackages = pkgs.linuxPackages_latest;
  };
}
