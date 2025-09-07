{ self, ... }@inputs:
let
  inherit (inputs.nixpkgs) lib;
  mkMetadata = metadataOverride: rec {
    json = builtins.fromJSON (builtins.readFile (./hosts + "/metadata.json"));
    metadata = lib.recursiveUpdate json metadataOverride;
    hasWg =
      hostName: lib.attrsets.hasAttrByPath [ "wireguard" "wg0" "ip4" ] metadata.hosts."${hostName}";
    getIp = hostName: metadata.hosts."${hostName}".ip4;
    getWgIp = hostName: metadata.hosts."${hostName}".wireguard.wg0.ip4;
  };
  mkMetadatalib = metadataOverride: rec {
    inherit (mkMetadata metadataOverride)
      metadata
      getIp
      getWgIp
      hasWg
      ;
    fixIp =
      deviceName:
      (
        { config, ... }:
        let
          hostName = config.networking.hostName;
        in
        {
          networking = rec {
            interfaces."${deviceName}".ipv4.addresses = [
              {
                address = getIp hostName;
                prefixLength = 24;
              }
            ];
            defaultGateway = metadata.networks."${metadata.hosts."${hostName}".network}".defaultGateway;
            nameservers = [
              defaultGateway
              "8.8.8.8"
              "8.8.4.4"
            ];
          };
        }
      );

    announceHost =
      otherHostName:
      let
        otherHostMetadata = metadata.hosts."${otherHostName}";
      in
      {
        pkgs,
        lib,
        myconfig,
        ...
      }:
      {
        imports = [
          (lib.mkIf (lib.attrsets.hasAttrByPath [ "ip4" ] otherHostMetadata) (
            let
              otherHostIp = otherHostMetadata.ip4;
            in
            {
              networking.extraHosts = ''
                ${otherHostIp} ${otherHostName}
                ${otherHostIp} ${otherHostName}.maxhbr.local
              '';
              home-manager.users."${myconfig.user}" = {
                home.file = {
                  ".ssh/imports/my-${otherHostName}.config".text = ''
                    Host ${otherHostName}
                      HostName ${otherHostIp}
                      User ${myconfig.user}
                  '';
                };
                home.packages = [
                  (pkgs.writeShellScriptBin "suspend-${otherHostName}" "ssh ${otherHostName} sudo systemctl suspend")
                ]
                ++ (
                  if (lib.attrsets.hasAttrByPath [ "mac" ] otherHostMetadata) then
                    [
                      (pkgs.writeShellScriptBin "wake-${otherHostName}" "${pkgs.wol}/bin/wol ${otherHostMetadata.mac}")
                    ]
                  else
                    [ ]
                );
              };
            }
          ))
          (lib.mkIf (lib.attrsets.hasAttrByPath [ "wireguard" "wg0" "ip4" ] otherHostMetadata) (
            let
              otherHostWgIp = otherHostMetadata.wireguard.wg0.ip4;
            in
            {
              networking.extraHosts = ''
                ${otherHostWgIp} ${otherHostName}.wg0
                ${otherHostWgIp} ${otherHostName}.wg0.maxhbr.local
              '';
            }
          ))
        ];
      };
    announceOtherHosts = thisHost: {
      imports = lib.map (host: announceHost host) (
        lib.filter (name: name != thisHost) (lib.attrNames metadata.hosts)
      );
    };

    addEternalTerminalCmd =
      host:
      {
        pkgs,
        lib,
        myconfig,
        ...
      }:
      {
        config = {
          home-manager.users."${myconfig.user}" = {
            home.packages = [
              (
                with pkgs;
                writeShellScriptBin "et-${host}" ''
                  set -euo pipefail
                  set -x
                  exec ${eternal-terminal}/bin/et "$@" ${myconfig.user}@${getWgIp host}:22022 
                ''
              )
            ];
          };
        };
      };

    setupAsWireguardClient =
      wgInterface: privateKey:
      (
        let
          wgNetwork = metadata.networks."${wgInterface}";
          wgPeerMetadata = metadata.hosts."${wgNetwork.peer}";
          privateKeyFile = "/etc/wireguard/${wgInterface}-private";
        in
        {
          config,
          lib,
          pkgs,
          ...
        }@args:
        (lib.mkIf (lib.attrsets.hasAttrByPath [ "ip4" ] wgPeerMetadata) {

          myconfig.secrets = {
            "wireguard.private" = {
              source = privateKey;
              dest = privateKeyFile;
              wantedBy = [ "wireguard-${wgInterface}.service" ];
            };
          };

          environment.systemPackages = [ pkgs.wireguard-tools ];
          networking.wireguard.interfaces = {
            "${wgInterface}" = {
              ips = [
                # (getWgIp config.networking.hostName) + "/24")
                (metadata.hosts."${config.networking.hostName}".wireguard."${wgInterface}".ip4 + "/24")
              ]; # Determines the IP address and subnet of the server's end of the tunnel interface.
              inherit privateKeyFile;
              mtu = 1380;
              peers = [
                {
                  publicKey = wgPeerMetadata.wireguard."${wgInterface}".pubkey;
                  # allowedIPs = [ "0.0.0.0/0" ];
                  # Or forward only particular subnets
                  allowedIPs = wgNetwork.allowedIPs;
                  endpoint = (wgPeerMetadata.ip4 + ":51820");
                  persistentKeepalive = 25; # Send keepalives every 25 seconds. Important to keep NAT tables alive.
                }
              ];
            };
          };
        })
      );

    getOtherWgHosts =
      thisHost:
      lib.filter (peer: peer != null) (
        lib.mapAttrsToList (
          name: host:
          if
            (lib.attrsets.hasAttrByPath [ "wireguard" "wg0" "pubkey" ] host)
            && (lib.attrsets.hasAttrByPath [ "wireguard" "wg0" "ip4" ] host)
            && (name != thisHost)
          then
            {
              name = name;
              publicKey = host.wireguard.wg0.pubkey;
              ip4 = host.wireguard.wg0.ip4;
            }
          else
            null
        ) metadata.hosts
      );

    setupAsSyncthingClient = cert: key: devices: folders: {
      myconfig.secrets = {
        "syncthing.cert.pem" = {
          source = cert;
          dest = "/etc/syncthing/cert.pem";
        };
        "syncthing.key.pem" = {
          source = key;
          dest = "/etc/syncthing/key.pem";
        };
      };
      services.syncthing = {
        enable = true;
        cert = "/etc/syncthing/cert.pem";
        key = "/etc/syncthing/key.pem";
        settings = { inherit devices folders; };
      };
    };

    mkSyncthingDevice =
      hostName: introducer:
      let
        hostMetadata = metadata.hosts."${hostName}";
        addressesFromIp =
          if (lib.attrsets.hasAttrByPath [ "ip4" ] hostMetadata) then
            [ "tcp://${hostMetadata.ip4}" ]
          else
            [ ];
        otherAddresses =
          if (lib.attrsets.hasAttrByPath [ "syncthing" "addresses" ] hostMetadata) then
            hostMetadata.syncthing.addresses
          else
            [ ];
      in
      # lib.mkIf (lib.attrsets.hasAttrByPath [ "syncthing" "id" ] hostMetadata)
      ({
        "${hostName}" = {
          name = hostName;
          id = hostMetadata.syncthing.id;
          addresses = addressesFromIp ++ otherAddresses;
          inherit introducer;
        };
      });

    setupAsBuildMachine = authorizedKeys: {
      users.extraUsers.nixBuild = {
        name = "nixBuild";
        isSystemUser = true;
        useDefaultShell = true;
        openssh.authorizedKeys.keys = authorizedKeys;
      };
      users.users.nixBuild.group = "nixBuild";
      users.groups.nixBuild = { };
      nix.settings = {
        trusted-users = [ "nixBuild" ];
        allowed-users = [ "nixBuild" ];
      };
    };

    setupBuildSlave =
      {
        host,
        speedFactor ? 2,
        systems ? [ "x86_64-linux" ],
        privateKey,
        useWg ? false,
      }:
      let
        name = if (useWg && (hasWg host)) then "builder.${host}.wg" else "builder.${host}";
        hostIp = if (useWg && (hasWg host)) then getWgIp host else getIp host;
      in
      {
        myconfig.secrets = {
          "${name}" = {
            source = privateKey;
            dest = "/etc/nix/${name}";
          };
        };
        nix.buildMachines = map (system: {
          hostName = name;
          maxJobs = 6;
          supportedFeatures = [
            "nixos-test"
            "benchmark"
            "big-parallel"
            "kvm"
            "aarch64-linux"
            "armv6l-linux"
          ];
          mandatoryFeatures = [ ];
          sshUser = "nixBuild";
          sshKey = "/etc/nix/${name}";
          inherit speedFactor system;
        }) systems;
        nix.distributedBuilds = true;
        # optional, useful when the builder has a faster internet connection than yours
        nix.extraOptions = ''
          builders-use-substitutes = true
        '';
        services.openssh.knownHosts = {
          "${name}".publicKey = metadata.hosts."${host}".pubkeys."/etc/ssh/ssh_host_ed25519_key.pub";
        };
        programs.ssh.extraConfig = ''
          Host ${name}
              HostName ${hostIp}
              User nixBuild
              IdentitiesOnly yes
              IdentityFile /etc/nix/${name}
              StrictHostKeyChecking accept-new
              ConnectTimeout 2
        '';
      };

    setupNixServe =
      hosts:
      let
        keys = map (host: metadata.hosts."${host}".pubkeys."id_rsa.pub") hosts;
      in
      {
        nix.sshServe = {
          enable = true;
          inherit keys;
        };
        users.extraUsers.nix-ssh = {
          openssh.authorizedKeys = { inherit keys; };
        };
      };

    setupAsBorgbackupClient = backupkey: passphrase: {
      myconfig.secrets = {
        "borgbackup.ssh" = {
          source = backupkey;
          dest = "/etc/borgbackup/ssh_key";
        };
        "borgbackup.passphrase" = {
          source = passphrase;
          dest = "/etc/borgbackup/passphrase";
        };
      };
    };

    mkBackupJob =
      configOverwrites:
      {
        encryption = {
          mode = "repokey-blake2";
          passCommand = "cat /etc/borgbackup/passphrase";
        };
        compression = "auto,lzma";
        startAt = [ ]; # "daily";
        prune.keep = {
          within = "1d"; # Keep all archives from the last day
          daily = 7;
          weekly = 4;
          monthly = -1; # Keep at least one archive for each month
        };
      }
      // configOverwrites;
    mkHddBackupJob =
      name: hddid: configOverwrites:
      (
        {
          pkgs,
          lib,
          config,
          ...
        }:
        let
          device = "/dev/disk/by-uuid/${hddid}";
          repomnt = "/mnt/backup/${hddid}";
          serviceName = "${name}@${hddid}";
          repodir = "${repomnt}/borgbackup/${config.networking.hostName}-${name}";
        in
        {
          fileSystems."${repomnt}" = {
            device = device;
            fsType = "ext4";
            options = [ "noauto,nofail,x-systemd.device-timeout=1,sync,users,rw" ];
          };
          services.borgbackup.jobs."${serviceName}" = mkBackupJob (
            {
              removableDevice = true;
              repo = repodir;
            }
            // configOverwrites
          );

          environment.systemPackages = [
            (pkgs.writeShellScriptBin "mkBackup-${serviceName}" ''
              set -euo pipefail
              set -x
              sudo mkdir -p "${repomnt}"
              sudo mount ${device} || true
              if ! ( mount | grep -q "${repomnt}" ); then
                exit 1
              fi
              sudo mkdir -p "${repodir}"
              (set -x; sudo systemctl restart borgbackup-job-${serviceName}.service)
              if [[ ! -f "${repodir}.key" ]]; then
                sleep 20
                sudo borg key export "${repodir}" "${repodir}.key"
              fi
              set -x
              journalctl -u borgbackup-job-${serviceName}.service --since "2 minutes ago" -f
            '')
          ];
        }
      );
    mkRemoteBackupJob =
      name: host: configOverwrites:
      (
        {
          pkgs,
          lib,
          config,
          ...
        }:
        let
          newName = "${name}@${host}";
          hostHasWg = hasWg host;
          hostWgIp = getWgIp host;
          newWgName = "${name}@${host}-wg";
          mkScript =
            name:
            pkgs.writeShellScriptBin "mkBackup-${name}" ''
              set -euo pipefail
              set -x
              sudo systemctl restart borgbackup-job-${name}.service
              journalctl -u borgbackup-job-${name}.service --since "2 minutes ago" -f
            '';
        in
        {
          services.borgbackup.jobs = {
            "${newName}" = mkBackupJob (
              {
                repo = "borg@${host}:./${config.networking.hostName}-${newName}.borg";
                environment.BORG_RSH = "ssh -i /etc/borgbackup/ssh_key"; # -o StrictHostKeyChecking=no
              }
              // configOverwrites
            );
            "${newWgName}" = lib.mkIf hostHasWg (
              mkBackupJob (
                {
                  repo = "borg@${hostWgIp}:./${config.networking.hostName}-${newWgName}.borg";
                  environment.BORG_RSH = "ssh -i /etc/borgbackup/ssh_key"; # -o StrictHostKeyChecking=no
                }
                // configOverwrites
              )
            );
          };
          environment.systemPackages = [
            (mkScript newName)
          ]
          ++ (if hostHasWg then [ (mkScript newWgName) ] else [ ]);
        }
      );

    setupAsBackupTarget =
      home: hosts:
      let
        keys = map (host: metadata.hosts."${host}".pubkeys."id_ed25519.pub") hosts;
      in
      {
        config = {
          users = {
            extraUsers.backup = {
              isSystemUser = true;
              group = "backup";
              uid = 1100;
              inherit home;
              createHome = false;
              openssh.authorizedKeys = { inherit keys; };
            };
            extraGroups.backup.gid = 1100;
          };
          services.borgbackup.repos."repo" = {
            path = home + "/borgbackup";
            user = "borg";
            group = "borg";
            authorizedKeys = keys;
            allowSubRepos = true;
          };
        };
      };

    get = metadata;
  };
in
rec {
  importall =
    path:
    if builtins.pathExists path then
      let
        content = builtins.readDir path;
      in
      map (n: import (path + ("/" + n))) (
        builtins.filter (
          n: builtins.match ".*\\.nix" n != null || builtins.pathExists (path + ("/" + n + "/default.nix"))
        ) (builtins.attrNames content)
      )
    else
      [ ];

  mkConfiguration =
    system: hostName: nixosModules: metadataOverride:
    let
      # pkgs = self.legacyPackages.${system};

      specialArgs = {
        inherit inputs;
        myconfig = {
          user = "mhuber";
          metadatalib = mkMetadatalib metadataOverride;
        };
        flake = self;

        modules = nixosModules ++ [
          {
            _module.args = inputs;
          }
          # ({ config, pkgs, ... }: {
          #   config = {
          #     # nixpkgs = {
          #     #   # inherit pkgs;
          #     #   inherit (pkgs) config system;
          #     # };
          #     environment.etc."myconfig".source = lib.cleanSource ./.;
          #     environment.etc."myconfig.current-system-packages".text = let
          #       packages = builtins.map (p: "${p.name}")
          #         config.environment.systemPackages;
          #       sortedUnique =
          #         builtins.sort builtins.lessThan (lib.unique packages);
          #       formatted = builtins.concatStringsSep "\n" sortedUnique;
          #     in formatted;
          #   };
          # })

          # home manager:
          (
            { config, lib, ... }:
            {
              imports = [
                # home manager:
                inputs.home.nixosModules.home-manager
              ];

              config = {
                home-manager = {
                  useUserPackages = true;
                  useGlobalPkgs = true;
                  extraSpecialArgs = specialArgs // {
                    super = config;
                  };
                  backupFileExtension =
                    let
                      rev = toString (self.shortRev or self.dirtyShortRev or self.lastModified or "unknown");
                    in
                    "${rev}.homeManagerBackup";
                  sharedModules = [
                    (
                      { pkgs, ... }:
                      {
                        home.stateVersion = lib.mkDefault (config.system.stateVersion);
                        home.packages = [
                          pkgs.dconf
                        ]; # see: https://github.com/nix-community/home-manager/issues/3113
                      }
                    )
                    (
                      {
                        pkgs,
                        lib,
                        config,
                        ...
                      }:
                      let
                        hmReady = "/tmp/home-manager.ready.${config.home.username}";
                      in
                      {
                        home.activation.markHmUnready = lib.hm.dag.entryBefore [ "writeBoundary" ] ''
                          rm -f ${hmReady} ${hmReady}.tmp
                        '';
                        home.activation.markHmReady = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
                          if [[ ! -f ${hmReady} ]]; then
                            date > ${hmReady}.tmp && mv ${hmReady}.tmp ${hmReady}
                          fi
                        '';
                      }
                    )
                  ];
                };
                # wait for home-manager to be ready
                systemd.services.greetd.unitConfig.ConditionPathExists = "/tmp/home-manager.ready.mhuber"; # TODO: hardcoded username
              };
            }
          )

          (
            { config, lib, ... }:
            {
              imports = [ inputs.agenix.nixosModules.default ];
              config = {
                home-manager.sharedModules = [
                  inputs.agenix.homeManagerModules.default
                  (
                    { pkgs, config, ... }:
                    {
                      home.packages = [
                        pkgs.age
                        inputs.agenix.packages."${system}".default
                      ];
                    }
                  )
                ];
              };
            }
          )

          (
            {
              config,
              lib,
              myconfig,
              ...
            }:
            {
              config = {
                system.activationScripts.genProfileManagementDirs = "mkdir -m 0755 -p /nix/var/nix/{profiles,gcroots}/per-user/${myconfig.user}";
                systemd.services.mk-hm-dirs = {
                  serviceConfig.Type = "oneshot";
                  script = ''
                    mkdir -m 0755 -p /nix/var/nix/{profiles,gcroots}/per-user/${myconfig.user}
                    chown ${myconfig.user} /nix/var/nix/{profiles,gcroots}/per-user/${myconfig.user}
                  '';
                  wantedBy = [ "home-manager-${myconfig.user}.service" ];
                };
              };
            }
          )

          (
            { config, ... }:
            {
              environment.etc."machine-id".text = builtins.hashString "md5" hostName;
              networking = { inherit hostName; };

              assertions = [
                {
                  assertion = config.networking.hostName == hostName;
                  message = "hostname should be set!";
                }
              ];
            }
          )

          { _module.args = specialArgs; }
        ];

        extraModules = [
          (
            { pkgs, ... }:
            {
              # ca-references
              nix.extraOptions = ''
                experimental-features = nix-command flakes recursive-nix
                keep-outputs = true
                keep-derivations = true
                show-trace = true
                builders-use-substitutes = true
                preallocate-contents = true
              '';

              # nix.registry = lib.mapAttrs (id: flake: {
              #   inherit flake;
              #   from = {
              #     inherit id;
              #     type = "indirect";
              #   };
              # }) (inputs // { nixpkgs = inputs.master; });
              nix.nixPath = lib.mapAttrsToList (k: v: "${k}=${toString v}") {
                nixpkgs = "${inputs.nixpkgs}/";
                nixos = "${self}/";
                home-manager = "${inputs.home}/";
              };
              system.configurationRevision = self.rev or "dirty";
            }
          )
        ];
      };
    in
    {
      inherit system specialArgs;
      modules = specialArgs.modules ++ specialArgs.extraModules;
    };

  evalConfiguration =
    system: hostName: nixosModules: metadataOverride:
    (
      let
        cfg = self.lib.mkConfiguration system hostName nixosModules metadataOverride;
      in
      lib.nixosSystem (
        lib.recursiveUpdate cfg {
          modules = cfg.modules ++ [ (./hosts/host + ".${hostName}") ];
        }
      )
    );

  mkDeploy =
    hostName: nixosConfigurations: metadataOverride:
    let
      getIp = (mkMetadata metadataOverride).getIp;
    in
    {
      # sshOpts = [ "-p" "2221" ];
      hostname = getIp hostName;
      fastConnection = true;

      profiles = {
        system = {
          # sshUser = "admin";
          path = inputs.deploy-rs.lib.x86_64-linux.activate.nixos (nixosConfigurations."${hostName}");
          user = "root";
        };
      };
    };

  # see also: https://nixos.mayflower.consulting/blog/2018/09/11/custom-images/
  mkISO =
    {
      system ? "x86_64-linux",
      hostName ? "myconfig",
      nixosModules ? [ ], # some custom configuration
      metadataOverride ? { },
      bootstrappedConfig ? null, # path to config to include for bootstrapping
    }:
    let
      myisoconfigModules =
        let
          forceSSHModule = {
            # OpenSSH is forced to have an empty `wantedBy` on the installer system[1], this won't allow it
            # to be automatically started. Override it with the normal value.
            # [1] https://github.com/NixOS/nixpkgs/blob/9e5aa25/nixos/modules/profiles/installation-device.nix#L76
            systemd.services.sshd.wantedBy = lib.mkOverride 40 [ "multi-user.target" ];
            services.openssh = {
              enable = lib.mkForce true;
              listenAddresses = [
                {
                  addr = "0.0.0.0";
                  port = 22;
                }
              ];
            };
          };

          bootstrapModule =
            { pkgs, config, ... }@args:
            (
              let
                bootstrap = pkgs.writeShellScriptBin "bootstrap" ''
                  set -euxo pipefail
                  if [[ "$(hostname)" != "myconfig" &&  "$(hostname)" != "iso" ]]; then
                      echo "hostname missmatch"
                      exit 1
                  fi
                  sudo \
                    BOOTSTRAP=YES \
                    BTRFS=${"BTRFS:-true"} \
                    EFI=${"EFI:-true"} \
                    ${./scripts/bootstrap.sh} $@
                  echo "you should run bootstrap-install next"
                '';
              in
              {
                environment.systemPackages = [ bootstrap ];
              }
            );

          bootstrapInstallModule =
            {
              lib,
              pkgs,
              config,
              ...
            }@args:
            (lib.mkIf (bootstrappedConfig != null) (
              let
                evalNixos =
                  configuration:
                  import "${inputs.nixpkgs}/nixos" {
                    inherit system configuration;
                  };
                preBuiltConfig =
                  (evalNixos (
                    import bootstrappedConfig {
                      pkgs = inputs.nixpkgs;
                      inherit lib config;
                    }
                  )).system;
                bootstrap-install = pkgs.writeShellScriptBin "bootstrap-install" ''
                  set -euxo pipefail
                  if [[ "$(hostname)" != "myconfig" &&  "$(hostname)" != "iso" ]]; then
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
              in
              {
                environment.systemPackages = [ bootstrap-install ];
                isoImage.storeContents = [ preBuiltConfig ];
              }
            ));
          addConfigModule =
            {
              pkgs,
              ...
            }:
            let
              myconfigContent = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
            in
            {
              system.userActivationScripts = {
                lnmyconfig.text = ''
                  if [[ ! -d "$HOME/myconfig-in-store" ]]; then
                    ln -s ${myconfigContent} "$HOME/myconfig-in-store"
                  fi
                '';
              };
            };

        in
        [
          "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-base.nix"
          "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/channel.nix"
          {
            image.baseName = lib.mkForce "nixos-myconfig-${hostName}";
          }
          forceSSHModule
          bootstrapModule
          bootstrapInstallModule
          addConfigModule
          {
            networking.wireless.enable = false; # managed by network manager
          }
        ];

    in
    (evalConfiguration system hostName (myisoconfigModules ++ nixosModules) metadataOverride)
    .config.system.build.isoImage;

}
