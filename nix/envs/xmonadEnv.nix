{pkgs,unstable}:
with pkgs; let
  name = "xmonadEnv";
  paths = [
    unstable.dmenu unclutter
    slock
  ] ++ (with unstable.haskellPackages; [
    xmonad xmobar yeganesh
  ]);
in buildEnv { inherit name paths; }
