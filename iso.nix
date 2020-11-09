# see also: https://nixos.mayflower.consulting/blog/2018/09/11/custom-images/
{ system ? "x86_64-linux", hostConfig ? "roles/dev.nix"
, secondaryHostConfig ? hostConfig }:
let
  nixpkgs = ./nixpkgs;
  evalNixos = configuration:
    import "${nixpkgs}/nixos" { inherit system configuration; };
  myisoconfig = { lib, pkgs, config, ... }:
    let
      user = config.myconfig.user;
      bootstrap = pkgs.writeShellScriptBin "bootstrap" ''
        if [[ "$(hostname)" != "myconfig" ]]; then
            echo "hostname missmatch"
            exit 1
        fi
        sudo BOOTSTRAP=YES ${./scripts/bootstrap.sh} $@
      '';
    in {
      imports = [
        "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
        "${nixpkgs}/nixos/modules/installer/cd-dvd/channel.nix"
        (./. + "/${hostConfig}")
        (lib.mkIf (hostConfig != secondaryHostConfig) (let
          preBuiltConfig = (evalNixos
            (import (./. + "/${secondaryHostConfig}") {
              pkgs = nixpkgs;
              inherit lib;
            })).system;
          bootstrap-install = pkgs.writeShellScriptBin "bootstrap-install" ''
            if [[ ! -d "/mnt/etc/nixos/" ]]; then
              echo "folder /mnt/etc/nixos/ is missing"
              exit 1
            fi
            sudo nixos-install --no-root-passwd --system ''${1:-${preBuiltConfig}}
          '';
        in {
          environment.systemPackages = [ bootstrap-install ];
          isoImage.storeContents = [ preBuiltConfig ];
        }))
      ];

      config = {
        networking.hostName = "myconfig";
        networking.wireless.enable = false;

        environment.systemPackages = [ bootstrap ];

        services.xserver.displayManager.autoLogin = {
          enable = true;
          user = "${user}";
        };

        # OpenSSH is forced to have an empty `wantedBy` on the installer system[1], this won't allow it
        # to be automatically started. Override it with the normal value.
        # [1] https://github.com/NixOS/nixpkgs/blob/9e5aa25/nixos/modules/profiles/installation-device.nix#L76
        systemd.services.sshd.wantedBy =
          lib.mkOverride 40 [ "multi-user.target" ];

        # add myconfig to iso
        isoImage = {
          contents = [{
            source = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
            target = "myconfig";
          }];
          isoBaseName = "nixos-myconfig";
        };
      };
    };

in { iso = (evalNixos myisoconfig).config.system.build.isoImage; }

