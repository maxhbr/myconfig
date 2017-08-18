{ config, lib, pkgs, ... }:

{
  options = {
    myconfig.roles.openssh = {
      enable = lib.mkEnableOption "Openssh role";
    };
  };

  config = lib.mkIf config.myconfig.roles.openssh.enable {
    services.openssh.enable = true;
  };
}
