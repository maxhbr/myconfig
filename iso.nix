# see also: https://nixos.mayflower.consulting/blog/2018/09/11/custom-images/
{ system ? "x86_64-linux", hostConfig ? null }:
let
  nixpkgs = ./nixpkgs;
  evalNixos = configuration:
    import "${nixpkgs}/nixos" { inherit system configuration; };

  myisoconfig = { lib, pkgs, config, ... }@args:
    let
      user = config.myconfig.user;

      forceSSHModule = {
        # OpenSSH is forced to have an empty `wantedBy` on the installer system[1], this won't allow it
        # to be automatically started. Override it with the normal value.
        # [1] https://github.com/NixOS/nixpkgs/blob/9e5aa25/nixos/modules/profiles/installation-device.nix#L76
        systemd.services.sshd.wantedBy =
          lib.mkOverride 40 [ "multi-user.target" ];
      };

      xautologinModule = {
        # autologin
        services.xserver.displayManager.autoLogin = {
          enable = true;
          inherit user;
        };
      };

      bootstrapModule = (let
        bootstrap = pkgs.writeShellScriptBin "bootstrap" ''
          set -euxo pipefail
          if [[ "$(hostname)" != "myconfig" ]]; then
              echo "hostname missmatch"
              exit 1
          fi
          sudo BOOTSTRAP=YES ${./scripts/bootstrap.sh} $@
          echo "you should run bootstrap-install next"
        '';
      in { environment.systemPackages = [ bootstrap ]; });

      bootstrapInstallModule = (lib.mkIf (hostConfig != null) (let
        preBuildConfigRoot = ./. + "/${hostConfig}";
        preBuiltConfig = (evalNixos (import preBuildConfigRoot {
          pkgs = nixpkgs;
          inherit lib config;
        })).system;
        bootstrap-install = pkgs.writeShellScriptBin "bootstrap-install" ''
          set -euxo pipefail
          if [[ "$(hostname)" != "myconfig" ]]; then
              echo "hostname missmatch"
              exit 1
          fi
          if [[ ! -d "/mnt/etc/nixos/" ]]; then
            echo "folder /mnt/etc/nixos/ is missing"
            echo "you should run bootstrap first"
            exit 1
          fi
          sudo nixos-install --no-root-passwd --system ''${1:-${preBuiltConfig}}
        '';
      in {
        environment.systemPackages = [ bootstrap-install ];
        isoImage.storeContents = [ preBuiltConfig ];
      }));

    in {
      imports = [
        "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
        "${nixpkgs}/nixos/modules/installer/cd-dvd/channel.nix"
        forceSSHModule
        xautologinModule
        bootstrapModule
        bootstrapInstallModule
        {
          # add myconfig to iso
          isoImage = {
            contents = [{
              source = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
              target = "myconfig";
            }];
            isoBaseName = "nixos-myconfig";
          };
        }
        ./modules
      ];

      config = {
        myconfig = {
          desktop.enable = true;
          virtualisation.enable = true;
        };

        networking.hostName = "myconfig";
        networking.wireless.enable = false;
      };
    };

in { iso = (evalNixos myisoconfig).config.system.build.isoImage; }
