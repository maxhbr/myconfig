{ config, lib, pkgs, ... }: {
  nixpkgs.overlays = [
    (final: prev: { mykeylight = pkgs.callPackage ./derivation.nix { }; })
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
}
