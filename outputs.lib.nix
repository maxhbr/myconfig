{ self, ... }@inputs:
let
  inherit (inputs.nixpkgs) lib;
  mkMetadatalib = metadataOverride:
    let
      json = builtins.fromJSON (builtins.readFile (./hosts + "/metadata.json"));
      metadata = lib.recursiveUpdate json metadataOverride;
    in {
      fixIp = deviceName:
        ({ config, ... }:
          let hostName = config.networking.hostName;
          in {
            networking = rec {
              interfaces."${deviceName}".ipv4.addresses = [{
                address = metadata.hosts."${hostName}".ip4;
                prefixLength = 24;
              }];
              defaultGateway =
                metadata.networks."${metadata.hosts."${hostName}".network}".defaultGateway;
              nameservers = [ defaultGateway "8.8.8.8" "8.8.4.4" ];
            };
          });

      announceHost = otherHostName:
        let otherHostMetadata = metadata.hosts."${otherHostName}";
        in { pkgs, lib, ... }: {
          config =
            (lib.mkIf (lib.attrsets.hasAttrByPath [ "ip4" ] otherHostMetadata)
              (let otherHostIp = otherHostMetadata.ip4;
              in {
                networking.extraHosts = ''
                  ${otherHostIp} ${otherHostName}
                  ${otherHostIp} ${otherHostName}.maxhbr.de
                '';
                home-manager.users.mhuber = {
                  home.file = {
                    ".ssh/imports/my-${otherHostName}.config".text = ''
                      Host ${otherHostName}
                        HostName ${otherHostIp}
                        User mhuber
                    '';
                  };
                  home.packages = [
                    (pkgs.writeShellScriptBin "suspend-${otherHostName}"
                      "ssh ${otherHostName} sudo systemctl suspend")
                  ] ++ (if (lib.attrsets.hasAttrByPath [ "mac" ]
                    otherHostMetadata) then
                    [
                      (pkgs.writeShellScriptBin "wake-${otherHostName}"
                        "${pkgs.wol}/bin/wol ${otherHostMetadata.mac}")
                    ]
                  else
                    [ ]);
                };
              }));
        };

      setupAsWireguardClient = wgInterface: privateKey:
        (let
          wgNetwork = metadata.networks."${wgInterface}";
          wgPeerMetadata = metadata.hosts."${wgNetwork.peer}";
        in { config, lib, pkgs, ... }@args:
        (lib.mkIf (lib.attrsets.hasAttrByPath [ "ip4" ] wgPeerMetadata) {

          myconfig.secrets = {
            "wireguard.private" = {
              source = privateKey;
              dest = "/etc/wireguard/wg-private";
            };
          };

          environment.systemPackages = [ pkgs.wireguard ];
          networking.wireguard.interfaces = {
            "${wgInterface}" = {
              ips = [
                (metadata.hosts."${config.networking.hostName}".wireguard."${wgInterface}".ip4
                  + "/24")
              ]; # Determines the IP address and subnet of the server's end of the tunnel interface.
              privateKeyFile = "/etc/wireguard/wg-private";
              peers = [{
                publicKey = wgPeerMetadata.wireguard."${wgInterface}".pubkey;
                # allowedIPs = [ "0.0.0.0/0" ];
                # Or forward only particular subnets
                allowedIPs = wgNetwork.allowedIPs;
                endpoint = (wgPeerMetadata.ip4 + ":51820");
                persistentKeepalive =
                  25; # Send keepalives every 25 seconds. Important to keep NAT tables alive.
              }];
            };
          };
        }));

      setupAsBuildMachine = authorizedKeys: {
        users.extraUsers.nixBuild = {
          name = "nixBuild";
          isSystemUser = true;
          useDefaultShell = true;
          openssh.authorizedKeys.keys = authorizedKeys;
        };
        nix = {
          allowedUsers = [ "nixBuild" ];
          trustedUsers = [ "nixBuild" ];
        };
      };

      setupBuildSlave = host: speedFactor: systems: privateKey:
        let keyName = "nixBuildPrivKey.${host}";
        in {
          myconfig.secrets = {
            "${keyName}" = {
              source = privateKey;
              dest = "/etc/nix/${keyName}";
            };
          };
          nix.buildMachines = map (system: {
            hostName = "builder.${host}";
            maxJobs = 6;
            supportedFeatures =
              [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
            mandatoryFeatures = [ ];
            sshUser = "nixBuild";
            sshKey = "/etc/nix/${keyName}";
            inherit speedFactor system;
          }) systems;
          nix.distributedBuilds = true;
          # optional, useful when the builder has a faster internet connection than yours
          nix.extraOptions = ''
            builders-use-substitutes = true
          '';
          services.openssh.knownHosts = {
            "builder.${host}".publicKey =
              metadata.hosts."${host}".pubkeys."/etc/ssh/ssh_host_ed25519_key.pub";
          };
          programs.ssh.extraConfig = ''
            Host builder.${host}
                HostName ${host}
                User nixBuild
                IdentitiesOnly yes
                IdentityFile /etc/nix/${keyName}
                StrictHostKeyChecking accept-new
                ConnectTimeout 2
          '';
        };

      setupNixServe = hosts:
        let
          keys =
            map (host: metadata.hosts."${host}".pubkeys."id_rsa.pub") hosts;
        in {
          nix.sshServe = {
            enable = true;
            inherit keys;
          };
          users.extraUsers.nix-ssh = {
            openssh.authorizedKeys = { inherit keys; };
          };
        };

      setupAsBackupTarget = home: hosts:
        let
          keys =
            map (host: metadata.hosts."${host}".pubkeys."id_ed25519.pub") hosts;
        in {
          config = {
            users = {
              extraUsers.backup = {
                isNormalUser = true;
                group = "backup";
                uid = 1100;
                inherit home;
                createHome = false;
                openssh.authorizedKeys = { inherit keys; };
              };
              extraGroups.backup.gid = 1100;
            };
          };
        };

      get = metadata;
    };
in rec {
  importall = path:
    if builtins.pathExists path then
      let content = builtins.readDir path;
      in map (n: import (path + ("/" + n))) (builtins.filter (n:
        builtins.match ".*\\.nix" n != null
        || builtins.pathExists (path + ("/" + n + "/default.nix")))
        (builtins.attrNames content))
    else
      [ ];

  mkConfiguration = system: hostName: nixosModules: metadataOverride:
    let
      pkgs = self.legacyPackages.${system};

      specialArgs = {
        myconfig = {
          user = "mhuber";
          metadatalib = mkMetadatalib metadataOverride;
        };
        flake = self;

        modules = nixosModules ++ [
          ({ config, ... }: {
            config = {
              nixpkgs = {
                inherit pkgs;
                inherit (pkgs) config system;
              };
              environment.etc."myconfig".source = lib.cleanSource ./.;
              environment.etc."myconfig.current-system-packages".text = let
                packages = builtins.map (p: "${p.name}")
                  config.environment.systemPackages;
                sortedUnique =
                  builtins.sort builtins.lessThan (lib.unique packages);
                formatted = builtins.concatStringsSep "\n" sortedUnique;
              in formatted;
            };
          })

          # home manager:
          self.nixosModules.activateHomeManager
          ({ config, lib, ... }: {
            config = {
              home-manager = {
                extraSpecialArgs = specialArgs // { super = config; };
              };
            };
          })

          ({ config, lib, myconfig, ... }: {
            config = {
              system.activationScripts.genProfileManagementDirs =
                "mkdir -m 0755 -p /nix/var/nix/{profiles,gcroots}/per-user/${myconfig.user}";
              systemd.services.mk-hm-dirs = {
                serviceConfig.Type = "oneshot";
                script = ''
                  mkdir -m 0755 -p /nix/var/nix/{profiles,gcroots}/per-user/${myconfig.user}
                  chown ${myconfig.user} /nix/var/nix/{profiles,gcroots}/per-user/${myconfig.user}
                '';
                wantedBy = [ "home-manager-${myconfig.user}.service" ];
              };
            };
          })

          ({ config, ... }: {
            environment.etc."machine-id".text =
              builtins.hashString "md5" hostName;
            networking = { inherit hostName; };

            assertions = [{
              assertion = config.networking.hostName == hostName;
              message = "hostname should be set!";
            }];
          })

          { _module.args = specialArgs; }
        ];

        extraModules = [{
          nix.package = lib.mkDefault pkgs.nixFlakes;
          nix.extraOptions = ''
            experimental-features = nix-command flakes ca-references recursive-nix
            show-trace = true
            builders-use-substitutes = true
            preallocate-contents = true
          '';

          nix.registry = lib.mapAttrs (id: flake: {
            inherit flake;
            from = {
              inherit id;
              type = "indirect";
            };
          }) (inputs // { nixpkgs = inputs.master; });
          nix.nixPath = lib.mapAttrsToList (k: v: "${k}=${toString v}") {
            nixpkgs = "${inputs.nixpkgs}/";
            nixos = "${self}/";
            home-manager = "${inputs.home}/";
          };
          system.configurationRevision = self.rev or "dirty";
        }];
      };
    in {
      inherit system specialArgs;
      modules = specialArgs.modules ++ specialArgs.extraModules;
    };

  evalConfiguration = system: hostName: nixosModules: metadataOverride:
    (let
      cfg = self.lib.mkConfiguration system hostName (nixosModules)
        metadataOverride;
    in lib.nixosSystem (lib.recursiveUpdate cfg {
      modules = cfg.modules ++ [ (./hosts/host + ".${hostName}") ];
    }));

  # see also: https://nixos.mayflower.consulting/blog/2018/09/11/custom-images/
  mkISO =
    { system ? "x86_64-linux"
    , hostName ? "myconfig"
    , nixosModules ? [] # some custom configuration
    , metadataOverride ? {}
    , bootstrappedConfig ? null # path to config to include for bootstrapping
    }:
    let
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
              enable = config.services.xserver.enable;
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

          bootstrapInstallModule = (lib.mkIf (bootstrappedConfig != null) (let
            evalNixos = configuration:
              import "${inputs.nixpkgs}/nixos" { inherit system configuration; };
            preBuildConfigRoot = bootstrappedConfig;
            preBuiltConfig = (evalNixos (import preBuildConfigRoot {
              pkgs = inputs.nixpkgs;
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
            "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-base.nix"
            "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/channel.nix"
            forceSSHModule
            xautologinModule
            bootstrapModule
            bootstrapInstallModule
            {
              networking.wireless.enable = false; # managed by network manager
            }
          ];

          config = {
            # add myconfig to iso
            isoImage = {
              # contents = [{
              #   source = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
              #   target = "myconfig";
              # }];
              isoBaseName = "nixos-myconfig";
            };
          };
        };

    in (evalConfiguration system hostName ([myisoconfig] ++ nixosModules) metadataOverride).config.system.build.isoImage;
}
