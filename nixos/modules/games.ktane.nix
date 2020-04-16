{ pkgs, ... }:
let
  ktane = pkgs.callPackage ../pkgs/Keep_Talking_and_Nobody_Explodes {};
in {
  config = {
    home-manager.users.mhuber = {
      home.packages = [ ktane ];
    };
  };
}
