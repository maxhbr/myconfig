{ pkgs, ... }:
let
  powder = pkgs.callPackage ./powder {};
in {
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        powder
      ];
    };
  };
}
