{ self, ... }@inputs:
let inherit (inputs.nixpkgs) lib;
    mkMetadatalib =
      metadataOverride:
      let
        json = builtins.fromJSON (builtins.readFile (./hosts + "/metadata.json"));
        metadata = lib.recursiveUpdate json metadataOverride;
      in
        {
          fixIp = deviceName: ({config, ...}: let
            hostName = config.networking.hostName;
          in {
            networking = rec {
              interfaces."${deviceName}".ipv4.addresses = [{
                address = metadata.hosts."${hostName}".ip4;
                prefixLength = 24;
              }];
              defaultGateway = metadata.networks."${metadata.hosts."${hostName}".network}".defaultGateway;
              nameservers = [ defaultGateway "8.8.8.8" "8.8.4.4" ];
            };
          });

          announceHost = otherHostName:
            (let otherHostMetadata = metadata.hosts."${otherHostName}";
             in { pkgs, lib, ... }: {
               config = (lib.mkIf (lib.attrsets.hasAttrByPath ["ip4"] otherHostMetadata) (let
                 otherHostIp = otherHostMetadata.ip4;
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
                    ] ++ (if (lib.attrsets.hasAttrByPath ["mac"] otherHostMetadata)
                          then [(pkgs.writeShellScriptBin "wake-${otherHostName}"
                            "${pkgs.wol}/bin/wol ${otherHostMetadata.mac}")]
                          else [ ]);
                  };
               }));
             };
            );

          setupAsWireguardClient = wgInterface: privateKey:
            ( let
              wgNetwork = metadata.networks."${wgInterface}";
              wgPeerMetadata = metadata.hosts."${wgNetwork.peer}";
            in { config, lib, pkgs, ... }@args:
              (lib.mkIf (lib.attrsets.hasAttrByPath ["ip4"] wgPeerMetadata) {

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
                      (metadata.hosts."${config.networking.hostName}".wireguard."${wgInterface}".ip4 + "/24")
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

          get = metadata;
        };
in {
  importall = path:
    if builtins.pathExists path then
      let content = builtins.readDir path;
      in map (n: import (path + ("/" + n))) (builtins.filter (n:
        builtins.match ".*\\.nix" n != null
        || builtins.pathExists (path + ("/" + n + "/default.nix")))
        (builtins.attrNames content))
    else
      [ ];

  mkConfiguration = system: hostName:
    nixosModules:
    metadataOverride:
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
              boot.initrd.secrets = { "/etc/myconfig" = lib.cleanSource ./.; };
              environment.etc."myconfig.current-system-packages".text = let
                packages =
                  builtins.map (p: "${p.name}") config.environment.systemPackages;
                sortedUnique = builtins.sort builtins.lessThan (lib.unique packages);
                formatted = builtins.concatStringsSep "\n" sortedUnique;
              in formatted;
            };
          })

          # home manager:
          inputs.home.nixosModules.home-manager
          ({ config, lib, ... }: {
            config = {
              home-manager = {
                extraSpecialArgs = specialArgs // { super = config; };
                useUserPackages = true;
                useGlobalPkgs = true;
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
            environment.etc."machine-id".text = builtins.hashString "md5" hostName;
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
            experimental-features = nix-command flakes
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

  evalConfiguration = system: hostName:
    nixosModules:
    metadataOverride:
    (let cfg = self.lib.mkConfiguration system hostName (nixosModules) metadataOverride;
     in lib.nixosSystem (lib.recursiveUpdate cfg {
       modules = cfg.modules ++ [ (./hosts/host + ".${hostName}") ];
     }));
}
