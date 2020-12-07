{ config, lib, pkgs, ... }:

{
  options.myconfig = with lib; {
    user = mkOption {
      type = types.str;
      default = "mhuber";
      example = "mhuber";
      description = ''
        The username of the main user
      '';
    };
  };
}
