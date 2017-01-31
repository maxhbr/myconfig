{ config, pkgs, ... }:

{
  imports = [
    ../../pkgs/services/urxvtd.nix
  ];

  environment.systemPackages = with pkgs; [
    rxvt_unicode_with-plugins rxvt_unicode.terminfo
  ];
  services.urxvtd = {
    enable = true;
    users = [ "mhuber" ];
    urxvtPackage = pkgs.rxvt_unicode_with-plugins;
  };

  fonts.fonts = with pkgs; [
    inconsolata
  ];
}
