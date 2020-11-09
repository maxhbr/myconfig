{ pkgs, config, ... }:
let user = config.myconfig.user;
in {
  config = {
    home-manager.users."${user}" = {
      home.packages = with pkgs; [ retroarchBare ];
    };
  };
}
