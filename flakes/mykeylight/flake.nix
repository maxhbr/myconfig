{
  description = "A very basic flake";

  outputs = { self, nixpkgs }: let
        pkgs = import nixpkgs { system =  "x86_64-linux"; };
  in {

    packages.x86_64-linux.mykeylights = pkgs.callPackage ./derivation.nix {};

    defaultPackage.x86_64-linux = self.packages.x86_64-linux.mykeylights;

    nixosModule = { config, lib, pkgs, ... }: {
      nixpkgs.overlays = [(self: super: {
        mykeylights = self.defaultPackage.x86_64-linux;
      })];
      home-manager.sharedModules = [({
        config = {
          home.packages = [ self.defaultPackage.x86_64-linux ];
        };
      })];
    };
  };
}
