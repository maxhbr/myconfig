{ self, ... }@inputs:
let
  inherit (inputs.nixpkgs) lib;

  myconfig = {
    user = "mhuber"; # TODO
    lib = rec {
      secretsDir = ../secrets;
      getSecretPath = hostName: fileName:
        (secretsDir + "/${hostName}/${fileName}");
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
            in if builtins.pathExists newIpPath then
              getSecretNoNewline hostName "newIp"
            else
              getSecretNoNewline hostName "ip";
            prefixLength = 24;
          }];
          defaultGateway = "192.168.1.1";
          nameservers = [ "192.168.1.1" "1.1.1.1" "8.8.8.8" "8.8.4.4" ];
        };
      };
    };
  };
in system: hostName:
{ nixosModules ? [ ], hmModules ? [ ] }:
let
  pkgs = self.legacyPackages.${system};

  # Final modules set
  finalModules = nixosModules ++ [
    (self.lib.mkNixpkgsModule pkgs)

    ({ config, ... }: {
      boot.initrd.secrets = { "/etc/myconfig" = lib.cleanSource ./.; };
      environment.etc."myconfig.current-system-packages".text = let
        packages =
          builtins.map (p: "${p.name}") config.environment.systemPackages;
        sortedUnique = builtins.sort builtins.lessThan (lib.unique packages);
        formatted = builtins.concatStringsSep "\n" sortedUnique;
      in formatted;
    })

    # home manager:
    ({ config, lib, ... }: {
      options.home-manager.users = lib.mkOption {
        type = with lib.types;
          attrsOf (submoduleWith {
            specialArgs = specialArgs // { super = config; };
            modules = hmModules;
          });
      };
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
  ];

  specialArgs = {
    inherit myconfig;
    flake = self;

    modules = finalModules ++ [
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

    # Modules to propagate to containers
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
  modules = finalModules ++ specialArgs.extraModules;
}
