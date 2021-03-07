{inputs, ...}: let
  inherit (inputs.nixpkgs) lib;
  importall = path:
    if builtins.pathExists path then
      let content = builtins.readDir path;
      in map (n: import (path + ("/" + n))) (builtins.filter (n:
        builtins.match ".*\\.nix" n != null
        || builtins.pathExists (path + ("/" + n + "/default.nix")))
        (builtins.attrNames content))
    else
      [ ];

  myconfig = {
    user = "mhuber"; # TODO
    lib = rec {
      secretsDir = ./secrets;
      getSecretPath = hostName: fileName: (secretsDir + "/${hostName}/${fileName}");
      getSecret = hostName: fileName:
        builtins.readFile (getSecretPath hostName fileName);

      getSecretNoNewline = hostName: fileName:
        lib.removeSuffix "\n" (getSecret hostName fileName);

      makeOptionalBySecrets = conf:
        lib.mkIf (builtins.pathExists (secretsDir + "/README.md")) conf;

      makeOptionalListBySecrets = list:
        if (builtins.pathExists (secretsDir + "/README.md")) then list else [ ];

      fixIp = hostName: deviceName: {
        networking = {
          interfaces."${deviceName}".ipv4.addresses = [{
            address = let newIpPath = getSecretPath hostName "newIp";
                      in if builtins.pathExists newIpPath
                         then getSecretNoNewline hostName "newIp"
                         else getSecretNoNewline hostName "ip";
            prefixLength = 24;
          }];
          defaultGateway = "192.168.1.1";
          nameservers = [ "192.168.1.1" "1.1.1.1" "8.8.8.8" "8.8.4.4" ];
        };
      };
    };
  };

  getModulesFromSecrets = hostName: (let
      hostsSecretsDir = (./secrets + "/${hostName}");
  in (if builtins.pathExists "${hostsSecretsDir}/default.nix"
       then [hostsSecretsDir]
      else []) ++ (importall ("${hostsSecretsDir}/imports")));
in {
  nixosConfigurations = let
    mkConfiguration = system: hostName: customConfig:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};

        nixpkgs = { config, ... }: {
          config.nixpkgs = {
            inherit pkgs;
            inherit (pkgs) config;
            inherit system;
            overlays = [
              (self: super: {
                unstable = super.unstable or { }
                  // inputs.master.legacyPackages.${system};
                nixos-unstable = super.nixos-unstable or { }
                  // inputs.large.legacyPackages.${system};
                nixos-unstable-small = super.nixos-unstable-small or { }
                  // inputs.small.legacyPackages.${system};
                nixos-2003-small = super.unstable or { }
                  // inputs.rel2003.legacyPackages.${system};
                nixos-2009-small = super.unstable or { }
                  // inputs.rel2009.legacyPackages.${system};
              })

              # # nix:
              # inputs.nix.overlay

              # nur:
              inputs.nur.overlay
            ];
          };
        };

        # Final modules set
        modules = [
          nixpkgs

          ({ config, ... }: {
            boot.initrd.secrets = {
              "/etc/myconfig" = lib.cleanSource ./.;
            };
            environment.etc."myconfig.current-system-packages".text = let
              packages = builtins.map (p: "${p.name}")
                config.environment.systemPackages;
              sortedUnique =
                builtins.sort builtins.lessThan (lib.unique packages);
              formatted = builtins.concatStringsSep "\n" sortedUnique;
            in formatted;
          })

          # home manager:
          inputs.home.nixosModules.home-manager
          {
            home-manager = {
              useUserPackages = true;
              useGlobalPkgs = true;
            };
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
          }

          inputs.myemacs.module
        ];

        hmModules = [
          inputs.myemacs.hmModule
        ];

        specialArgs = {
          inherit myconfig;
          flake = inputs.self;

          modules = modules ++ [
            ({config, ...}: {
              assertions = [{
                assertion = config.networking.hostName == hostName;
                message = "hostname should be set!";
              }];
            })
            {
              home-manager.users."${myconfig.user}" = { ... }: {
                imports = hmModules;
              };
            }
            {
              environment.etc."machine-id".text =
                builtins.hashString "md5" hostName;
              networking = { inherit hostName; };
            }
            { _module.args = specialArgs; }
          ];

          # Modules to propagate to containers
          extraModules = [
            {
              nix.package = lib.mkDefault pkgs.nixFlakes;
              nix.registry = lib.mapAttrs (id: flake: {
                inherit flake;
                from = {
                  inherit id;
                  type = "indirect";
                };
              }) (inputs // { nixpkgs = inputs.master; });
              nix.nixPath = lib.mapAttrsToList (k: v: "${k}=${toString v}") {
                nixpkgs = "${inputs.nixpkgs}/";
                nixos = "${inputs.self}/";
                home-manager = "${inputs.home}/";
              };
              system.configurationRevision = inputs.self.rev or "dirty";
            }
          ];
        };
      in lib.nixosSystem {
        inherit system specialArgs;
        modules = modules ++ specialArgs.extraModules ++ [
          (./host + ".${hostName}")
          customConfig
        ] ++ (getModulesFromSecrets hostName);
      };
  in {
    x1extremeG2 = mkConfiguration "x86_64-linux" "x1extremeG2"
      {
        imports = [

        ];
      };
    workstation = mkConfiguration "x86_64-linux" "workstation"
      {
        imports = [
          (myconfig.lib.fixIp "workstation" "enp39s0")
        ];
      };
  };

}
