{ config, pkgs, lib, ... }: let
  # cksum /etc/machine-id | while read c rest; do printf "%x" $c; done | sudo tee ./hostid
  upg-pull = pkgs.writeShellScriptBin "upg-pull" ''
  set -e
  if [ "$(id -u)" -ne "1000" ]; then
    echo "you should run this script as the user, which owns $0"
    exec sudo su -c "$0" "$(id -nu 1000)"
  fi
  set -x
  myconfigDir=$HOME/myconfig
  if [[ -d "$myconfigDir" ]]; then
    if [[ ! -d "$myconfigDir/.git" ]]; then
      exit 1
    fi
    cd "$myconfigDir"
    BRANCH=$(${pkgs.git}/bin/git rev-parse --abbrev-ref HEAD)
    if [[ "$BRANCH" == "master" ]]; then
      if [ -z "$(${pkgs.git}/bin/git diff-index --name-only HEAD --)" ]; then
        ${pkgs.git}/bin/git pull origin master
      fi
    fi
  else
    ${pkgs.git}/bin/git clone https://github.com/maxhbr/myconfig "$myconfigDir"
  fi
  '';
in {
  imports = [
    ./lib
    ./modules/core.nix
    ./modules/gnupg.nix
    ./modules/vim
    ./modules/zsh
    ./modules/tmux
    ./modules/git
    ./modules/pass
    ./modules/myborgbackup
    ./modules/nixos.networking
    ./modules/nixos.nix.nix
    ./modules/user.mhuber.nix
    ./modules/dic.nix
    ./modules/service.openssh.nix
  ];

  config =
    { environment =
        { systemPackages = [ upg-pull ];
          shellAliases =
            { upg = "~/myconfig/rebuild.sh";
              upg-fast = "~/myconfig/rebuild.sh --fast";
              upg-fast-no-tmux = "~/myconfig/rebuild.sh --no-tmux --no-git --fast";
              upg-dry = "~/myconfig/rebuild.sh --dry-run";
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
