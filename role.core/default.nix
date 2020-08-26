{ config, pkgs, lib, ... }:
let
  mybackup = pkgs.callPackage ../pkgs/mybackup
    { inherit pkgs;
    };
in
{
  imports =
    [ ./lib
      ./core.nix
      ./gnupg.nix
      ./vim
      ./zsh
      ./tmux
      ./git
      ./pass
      ./nixos.networking
      ./nixos.nix.nix
      ./user.mhuber.nix
      ./dic.nix
      ./service.openssh.nix
      ./service.syncthing.nix
    ];

  config =
    { environment.systemPackages =
        [ mybackup
        ];
      assertions =
        [ { assertion = config.networking.hostId != null;
            message = ''
              hostid should be set!
              generate it with
              $ cksum /etc/machine-id | while read c rest; do printf "%x" $c; done
            '';
          }
          { assertion = config.networking.hostName != "nixos";
            message = "hostname should be set!";
          }
        ];
    };
}
