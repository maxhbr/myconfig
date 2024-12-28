{pkgs, ...}: let
  flags = "-Wno-incompatible-pointer-types -Wno-implicit-function-declaration -Wno-int-conversion";
  ovettiyeAttrs = (old: {
    NIX_CFLAGS_COMPILE = if builtins.hasAttr "NIX_CFLAGS_COMPILE" old 
                         then "${old.NIX_CFLAGS_COMPILE} ${flags}"
                         else flags;
  });
in {
  nixpkgs.overlays = (map (prog: 
    (final: prev: {
      "${prog}" = prev."${prog}".overrideAttrs ovettiyeAttrs;
    })) ["mplayer" "wol" "catimg" "pdm"]) ++ [
    (final: prev: {
      evolution = pkgs.stable.evolution;
    })
    ];
}
