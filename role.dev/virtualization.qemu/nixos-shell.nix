{ pkgs, ... }:
{ config =
    { environment.systemPackages =
        with pkgs.unstable;
        [ nixos-shell
        ];
    };
}
