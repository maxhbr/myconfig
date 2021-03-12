{ self, ... }@inputs:
let inherit (inputs.nixpkgs) lib;
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
          metadatalib = import ./hosts/metadata.lib.nix { inherit lib metadataOverride; };
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

          ({ config, lib, ... }: {
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

  evalConfiguration = system: hostName: nixosModules:
    (let cfg = self.lib.mkConfiguration system hostName (nixosModules);
     in lib.nixosSystem (lib.recursiveUpdate cfg {
       modules = cfg.modules ++ [ (./hosts/host + ".${hostName}") ];
     }));
}
