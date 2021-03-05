{ config, lib, ... }: {
  imports = [ ./helper.nix ./nix-nixPath ./home-manager ];

  config = {
    system.copySystemConfiguration = true;
    boot.initrd.secrets = {
      "/etc/nixos" = lib.cleanSource ./../..;
    };

    nixpkgs = {
      config = pkgs: { allowUnfree = true; };
      overlays = (let
        path = ./overlays;
        content = builtins.readDir path;
      in if builtins.pathExists path then
        map (n: import (path + ("/" + n))) (builtins.filter (n:
          builtins.match ".*\\.nix" n != null
          || builtins.pathExists (path + ("/" + n + "/default.nix")))
          (builtins.attrNames content))
      else
        [ ]);
    };
  };
}
