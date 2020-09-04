{ pkgs, ... }:
let
  nixos-shell = super.callPackage (import (super.builtins.fetchGit {
    url = "https://github.com/Mic92/nixos-shell";
  }));

in
{ config =
    { environment.systemPackages =
        [ nixos-shell
        ];
    };
}
