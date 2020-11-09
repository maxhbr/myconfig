{ pkgs, config, lib, ... }:
let user = config.myconfig.user;
in {
  config = lib.mkIf
    (builtins.pathExists ../pkgs/starsector/starsector_linux-0.9.1a-RC8.zip) {
      home-manager.users."${user}" =
        let starsector = pkgs.unstable.callPackage ../pkgs/starsector { };
        in { home.packages = [ starsector ]; };
    };
}
