{pkgs}:
with pkgs; let
  name = "xmonadEnv";
  paths = [
    dmenu unclutter
    slock
  ] ++ (with haskellPackages; [
    xmonad xmobar yeganesh
  ]);
in buildEnv { inherit name paths; }
