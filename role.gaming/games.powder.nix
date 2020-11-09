{ pkgs, config, ... }:
let
  user = config.myconfig.user;
  powder = pkgs.callPackage ../pkgs/powder { };
in {
  config = { home-manager.users."${user}" = { home.packages = [ powder ]; }; };
}
