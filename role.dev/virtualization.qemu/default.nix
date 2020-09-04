{ pkgs, ... }:
{ imports =
    [ ./nixos-shell.nix
    ];
  config =
    { environment.systemPackages = with pkgs;
        [ qemu aqemu
        ];
      boot.kernelModules =
        [ "kvm-amd" "kvm-intel"
        ];
      virtualisation.libvirtd.enable = true;
    };
}
