{ pkgs, config, lib, ... }:
lib.mkIf (builtins.pathExists ./pkgs/starsector/starsector_linux-0.9.1a-RC8.zip)
(let starsector = pkgs.unstable.callPackage ./pkgs/starsector { };
in { home.packages = [ starsector ]; })
