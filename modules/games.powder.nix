{ pkgs, ... }:
let
  powder = pkgs.callPackage ../pkgs/powder {};
in {
  config = {
    home-manager.users.mhuber = {
      home.packages = [ powder ];
    };
  };
}
