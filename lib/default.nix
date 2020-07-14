{ config, ... }:
{
  imports = [
    ./helper.nix
    ./nixpkgs-unstable
    ./home-manager
  ];

  config = {
    system.copySystemConfiguration = true;

    nixpkgs = {
      config = pkgs: {
        allowUnfree = true;
      };
      overlays =
        [(self: super:
            { unstable = super.unstable or {} // (import ../nixpkgs-unstable { config = config.nixpkgs.config; });
            }
        )] ++
        ( let
            path = ./overlays;
            content = builtins.readDir path;
          in if builtins.pathExists path
            then map (n: import (path + ("/" + n)))
                  (builtins.filter (n: builtins.match ".*\\.nix" n != null || builtins.pathExists (path + ("/" + n + "/default.nix")))
                    (builtins.attrNames content))
            else []);
    };
  };
}
