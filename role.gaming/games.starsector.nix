{ config, lib, pkgs, ... }:

{
  config = {
    home-manager.users.mhuber =
      let starsector = pkgs.unstable.callPackage ../pkgs/starsector { };
      in { home.packages = [ starsector ]; };
  };
}
