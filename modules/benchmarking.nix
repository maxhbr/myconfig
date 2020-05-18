{ pkgs, ... }:
let
  blender-benchmark-launcher-cli = pkgs.callPackage ../pkgs/blender-benchmark-launcher-cli {};
  blender-benchmark-launcher = pkgs.callPackage ../pkgs/blender-benchmark-launcher {};
in {
  config = {
    home-manager.users.mhuber = {
      home.packages = [ ];
    };
  };
}
