{ pkgs, ... }:
{
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        retroarchBare
      ];
    };
  };
}
