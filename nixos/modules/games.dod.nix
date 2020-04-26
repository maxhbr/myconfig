{ pkgs, ... }:
let
  dod = pkgs.callPackage ../pkgs/DungeonsofDredmor {};
in {
  config = {
    home-manager.users.mhuber = {
      home.packages = [ dod ];
    };
  };
}
