{ config, lib, pkgs, ... }:
let user = config.myconfig.user;
in {
  config = (lib.mkIf config.services.xserver.enable {
    services.printing = {
      enable = config.myconfig.desktop.full;
      drivers = with pkgs; [ gutenprint hplipWithPlugin ];
      # add hp-printer with:
      # $ nix run nixpkgs.hplipWithPlugin -c sudo hp-setup
    };
    programs.system-config-printer.enable = config.services.printing.enable;
  });
}
