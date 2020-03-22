{ pkgs, ...}: {
  imports = [
    ../core
    # modules
    ./modules/emacs
    ./modules/mail
  ];
  # config = {
  #   boot.kernelPackages = pkgs.linuxPackages_latest;
  # };
}
