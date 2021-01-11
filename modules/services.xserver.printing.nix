{ config, lib, pkgs, ... }:
let user = config.myconfig.user;
in {
  config = (lib.mkIf config.services.xserver.enable {
    services.printing = {
      enable = true;
      drivers = with pkgs; [ gutenprint hplip ];
      # add hp-printer with:
      # $ nix run nixpkgs.hplipWithPlugin -c sudo hp-setup
    };
    programs.system-config-printer.enable = config.services.printing.enable;
  });
}
