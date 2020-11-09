{ pkgs, config, lib, ... }:
let user = config.myconfig.user;
in {
  config = {
    home-manager.users."${user}" = {
      home.packages = with pkgs.unstable; [ lutris ];
    };
  };
}
