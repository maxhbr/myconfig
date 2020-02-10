{ pkgs, ...}: {
  imports = [
    ../core
    # modules
    ./modules/emacs
    ./modules/mail
  ];
  config = {
    boot.kernelPackages = pkgs.unstable.linuxPackages_5_4;
  };
}
