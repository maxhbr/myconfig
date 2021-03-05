{
  description = "myconfig";

  inputs = {
    master.url = "github:nixos/nixpkgs/master"; # |.
    staged.url = "github:nixos/nixpkgs/staging"; # | |-- Nix
    small.url = "github:nixos/nixpkgs/nixos-unstable-small"; # | |--   pkgs
    large.url = "github:nixos/nixpkgs/nixos-unstable"; # |'
    rel2009.url = "github:nixos/nixpkgs/nixos-20.09"; # | Stable
    rel2003.url = "github:nixos/nixpkgs/nixos-20.03"; # | Old Stable

    home.url = "github:nix-community/home-manager"; # |- Home-manager
    home.inputs.nixpkgs.follows = "/master"; # |

    nix.url = "github:nixos/nix/flakes";
    nix.inputs.nixpkgs.follows = "master";

    nur.url = "github:nix-community/NUR";

    emacs.url = "github:nix-community/emacs-overlay";
    utils.url = "github:numtide/flake-utils";

    flake-utils.url = "github:numtide/flake-utils";

    hardware.url = "github:nixos/nixos-hardware";
  };

  outputs = inputs:
    let
      allSystems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      config = { allowUnfree = true; };
      channels = with inputs; {
        pkgs = small; # For packages
        modules = master; # For nixos modules
        lib = master; # For flake-wide lib
      };
      inherit (channels.lib) lib; # this ^

      eachDefaultSystem = inputs.flake-utils.lib.eachSystem [ "x86_64-linux" ];

      # inherit (inputs.self.passthru) secrets;

      user = mhuber; # TODO
    in {

      nixosConfigurations = let
        mkConfiguration = system: hostName:
          let
            myconfig = { };

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
                      // import channels.pkgs { inherit system; };
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

              {
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
              }

              # home manager:
              inputs.home-manager.nixosModules.home-manager
              {
                home-manager = {
                  useUserPackages = true;
                  useGlobalPkgs = true;
                };
                system.activationScripts.genProfileManagementDirs =
                  "mkdir -m 0755 -p /nix/var/nix/{profiles,gcroots}/per-user/${user}";
                systemd.services.mk-hm-dirs = {
                  serviceConfig.Type = "oneshot";
                  script = ''
                    mkdir -m 0755 -p /nix/var/nix/{profiles,gcroots}/per-user/${user}
                    chown ${user} /nix/var/nix/{profiles,gcroots}/per-user/${user}
                  '';
                  wantedBy = [ "home-manager-${user}.service" ];
                };
              }
            ];

            specialArgs = {
              inherit myconfig;
              flake = inputs.self;

              # inherit (secrets) hosts domains;

              modules = modules ++ [
                {
                  environment.etc."machine-id".text =
                    builtins.hashString "md5" hostName;
                  networking = { inherit hostName; };
                }
                { _module.args = specialArgs; }
              ];

              # Modules to propagate to containers
              extraModules = [{
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
              }];
            };
          in lib.nixosSystem {
            inherit system specialArgs;
            modules = modules ++ [ (./host + ".${hostName}") ];
          };
      in { x1extremeG2 = mkConfiguration "x86_64-linux" "x1extremeG2"; };

    } // (eachDefaultSystem (system: {
      devShell = let pkgs = import channels.pkgs { inherit system; };
      in pkgs.mkShell {
        nativeBuildInputs = with pkgs; [ git git-crypt git-secrets nixfmt ];
        shellHook = ''
          mkdir -p secrets
        '';

        NIX_CONF_DIR = with pkgs;
          let
            nixConf = ''
              ${lib.optionalString (builtins.pathExists /etc/nix/nix.conf)
              (builtins.readFile /etc/nix/nix.conf)}
              experimental-features = nix-command flakes ca-references
              print-build-logs = true
            ''; # access-tokens = "github.com=${secrets.git.github.oauth-token}"
          in linkFarm "nix-conf-dir" ([
            {
              name = "nix.conf";
              path = writeText "flakes-nix.conf" nixConf;
            }
            {
              name = "registry.json";
              path = /etc/nix/registry.json;
            }
            {
              name = "machines";
              path = /etc/nix/machines;
            }
          ]);
      };

      legacyPackages = import channels.pkgs { inherit system; };
      passthru = rec { inherit inputs channels; };
    }));
}
