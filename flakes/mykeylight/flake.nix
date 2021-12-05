{
  description = "A flake for mykeylight.py";

  outputs = { self, nixpkgs }:
    let pkgs = import nixpkgs { system = "x86_64-linux"; };
    in {

      packages.x86_64-linux.mykeylight = pkgs.callPackage ./derivation.nix { };

      defaultPackage.x86_64-linux = self.packages.x86_64-linux.mykeylight;

      nixosModule = { config, lib, pkgs, ... }: {
        nixpkgs.overlays = [
         (final: prev: {
            mykeylight = self.defaultPackage.x86_64-linux;
          })
         (final: prev: {
            mykeylight-on = (final.writeShellScriptBin "mykeylight-on"
              "${final.mykeylight}/bin/mykeylight.py --on");
            mykeylight-off = (final.writeShellScriptBin "mykeylight-off"
              "${final.mykeylight}/bin/mykeylight.py --off");
          })
        ];
        home-manager.sharedModules = [({
          config = {
            home.packages = with pkgs; [
              mykeylight
              mykeylight-on
              mykeylight-off
            ];
          };
        })];
      };
    };
}
