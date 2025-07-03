{ pkgs, ... }:
let
  footswitch = pkgs.callPackage ../pkgs/footswitch { };
in
{
  config = {
    home-manager.users.mhuber = {
      home.packages = [ footswitch ];
    };
  };
}
