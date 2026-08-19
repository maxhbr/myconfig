# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  ...
}:
let
  force-connect-Xvlcwkhgfq = pkgs.writeShellApplication {
    name = "force-connect-Xvlcwkhgfq";
    runtimeInputs = with pkgs; [
      networkmanager
      gnugrep
      coreutils
    ];
    text = builtins.readFile ./force-connect-Xvlcwkhgfq.sh;
  };
in
{
  home-manager.sharedModules = [
    { home.packages = [ force-connect-Xvlcwkhgfq ]; }
  ];
}
