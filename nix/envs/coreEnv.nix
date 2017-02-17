{pkgs}:
with pkgs; let
  name = "coreEnv";
  paths = [
    cliEnv
    adminEnv
    wget curl
    git git-lfs
  ];
in buildEnv { inherit name paths; }
