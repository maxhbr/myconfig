{ config, pkgs, lib, ... }:
let
  mybackup = pkgs.callPackage ../pkgs/mybackup
    { inherit pkgs;
    };
in
{
  imports =
    [ ../lib
      ../modules/core.nix
      ../modules/gnupg.nix
      ../modules/vim
      ../modules/zsh
      ../modules/tmux
      ../modules/git
      ../modules/pass
      ../modules/nixos.networking
      ../modules/nixos.nix.nix
      ../modules/user.mhuber.nix
      ../modules/dic.nix
      ../modules/service.openssh.nix
    ];

  config =
    { environment =
        { systemPackages =
            [ upg-pull
              mybackup
            ];
          shellAliases =
            { upg = "~/myconfig/rebuild.sh";
              upg-fast = "~/myconfig/rebuild.sh --fast";
              upg-fast-no-tmux = "~/myconfig/rebuild.sh --no-tmux --no-git --fast";
              upg-dry = "~/myconfig/rebuild.sh --dry-run";
              upg-get-hostId = "cksum /etc/machine-id | while read c rest; do printf \"%x\" $c; done";
            };
      };
      nix.nixPath = [ ("nixpkgs=" + ../nixpkgs) "nixos-config=/dev/null" ];
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
