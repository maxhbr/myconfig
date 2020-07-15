{ pkgs, lib, config, ...}: {
  imports = [
    ./core.nix
    # modules
    ../modules/emacs
    ../modules/mail
  ];
  config = lib.mkIf (! config.virtualisation.virtualbox.host.enable) {
    boot.kernelPackages = lib.mkDefault pkgs.linuxPackages_latest;
  };
}
