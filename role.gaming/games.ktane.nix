{ pkgs, config, lib, ... }:
let user = config.myconfig.user;
in {
  config = lib.mkIf (builtins.pathExists
    ../../pkgs/Keep_Talking_and_Nobody_Explodes/Keep_Talking_and_Nobody_Explodes_1.8.3_-_StandaloneLinuxUniversal_Default.zip) {
      home-manager.users."${user}" = let
        ktane =
          pkgs.unstable.callPackage ../../pkgs/Keep_Talking_and_Nobody_Explodes
          { };
      in { home.packages = [ ktane ]; };
    };
}
