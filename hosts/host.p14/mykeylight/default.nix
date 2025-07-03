{
  config,
  lib,
  pkgs,
  ...
}:
{
  nixpkgs.overlays = [
    (final: prev: {
      mykeylight =
        let
          mykeylight = pkgs.callPackage ./derivation.nix { };
        in
        pkgs.symlinkJoin {
          name = "mykeylight-${mykeylight.version}";
          paths = [
            mykeylight
            (pkgs.writeShellScriptBin "mykeylight-on" "${mykeylight}/bin/mykeylight.py --on")
            (pkgs.writeShellScriptBin "mykeylight-off" "${mykeylight}/bin/mykeylight.py --off")
          ];
          strictDeps = false;
          passthru = { inherit (mykeylight.passthru) tests; };

          inherit (mykeylight) meta;
        };
    })
  ];
  home-manager.sharedModules = [
    ({
      config = {
        home.packages = with pkgs; [ mykeylight ];
      };
    })
  ];
}
