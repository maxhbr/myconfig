{inputs, channels, ...}: let
  inherit (channels.lib) lib;
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
  };
in {
  nixosConfigurations = let
    mkConfiguration = system: hostName: customConfig:
      let
        pkgs = channels.modules.legacyPackages.${system};

        nixpkgs = { config, ... }: {
          config.nixpkgs = {
            # inherit pkgs;
            inherit system;
            overlays = [
              (self: super: {
                unstable = super.unstable or { }
                  // import inputs.master { inherit system; };
                nixos-unstable = super.nixos-unstable or { }
                  // import channels.staging { inherit system; };
                nixos-unstable-small = super.nixos-unstable-small or { }
                  // import inputs.small { inherit system; };
                nixos-2003-small = super.unstable or { }
                  // import inputs.rel2003 { inherit system; };
                nixos-2009-small = super.unstable or { }
                  // import inputs.rel2009 { inherit system; };
              })
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
                nixpkgs = "${channels.pkgs}/";
                nixos = "${inputs.self}/";
                home-manager = "${inputs.home}/";
              };
              system.configurationRevision = inputs.self.rev or "dirty";
            }
          ];
        };
      in lib.nixosSystem {
        inherit system specialArgs;
        modules = modules ++ [
          (./host + ".${hostName}")
          customConfig
        ] ++ (let
          hostsSecretsDir = (./secrets + "/${hostName}");
        in if builtins.pathExists hostsSecretsDir
           then ([hostsSecretsDir] ++ (importall ("${hostsSecretsDir}/imports")))
           else []);
      };
  in {
    x1extremeG2 = mkConfiguration "x86_64-linux" "x1extremeG2" {};
    workstation = mkConfiguration "x86_64-linux" "workstation" {};
  };

}
