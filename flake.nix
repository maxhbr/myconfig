{
  description = "myconfig";

  inputs = {
    master.url = "github:nixos/nixpkgs/master";
    # staged.url = "github:nixos/nixpkgs/staging";
    nixos-unstable-small.url = "github:nixos/nixpkgs/nixos-unstable-small";
    nixos-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    rel2009.url = "github:nixos/nixpkgs/nixos-20.09";
    rel2003.url = "github:nixos/nixpkgs/nixos-20.03";
    rel2105.url = "github:nixos/nixpkgs/release-21.05";

    home.url = "github:nix-community/home-manager";
    home.inputs.nixpkgs.follows = "nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";

    # nix.url = "github:nixos/nix/flakes";
    # nix.inputs.nixpkgs.follows = "nixpkgs";

    nur.url = "github:nix-community/NUR";

    hardware.url = "github:nixos/nixos-hardware";

    # mine

    emacs.url = "github:nix-community/emacs-overlay";
    nix-doom-emacs.url = "github:vlaci/nix-doom-emacs";
    nix-doom-emacs.inputs.nixpkgs.follows = "nixpkgs";

    myemacs.url = "path:flakes/myemacs/";
    myemacs.inputs.nixpkgs.follows = "nixpkgs";
    myemacs.inputs.nix-doom-emacs.follows = "nix-doom-emacs";

    myfish.url = "path:flakes/myfish/";

    myxmonad.url = "path:flakes/myxmonad/";
    myxmonad.inputs.nixpkgs.follows = "nixpkgs";
    myxmonad.inputs.flake-utils.follows = "flake-utils";

    license-compliance-toolbox.url = "path:flakes/license-compliance-toolbox/";
    license-compliance-toolbox.inputs.nixpkgs.follows = "nixpkgs";

    zephyrproject.url = "path:flakes/zephyrproject/";
    # zephyrproject.inputs.nixpkgs.follows = "nixpkgs";
    zephyrproject.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let inherit (inputs.nixpkgs) lib;
    in lib.recursiveUpdate {
      aggregatedInputs = inputs;
      lib = import ./outputs.lib.nix inputs;

      ##########################################################################
      ## profiles and modules ##################################################
      ##########################################################################

      nixosModules = {
        activateHomeManager = { config, lib, ... }: {
          imports = [
            # home manager:
            inputs.home.nixosModules.home-manager
          ];

          config = {
            home-manager = {
              useUserPackages = true;
              useGlobalPkgs = true;
            };
          };
        };
        core = { ... }: {
          imports = [
            ({ pkgs, ... }: {
              config.nixpkgs = {
                overlays = let
                  mkSubPkgsOverlay = targetName: input:
                    (self: super: {
                      "${targetName}" = super."${targetName}" or { }
                        // import input { inherit (pkgs) config system; };
                    });
                in [
                  (mkSubPkgsOverlay "master" inputs.master)
                  (mkSubPkgsOverlay "nixos-unstable" inputs.nixos-unstable)
                  (mkSubPkgsOverlay "nixos-unstable-small" inputs.nixos-unstable-small)
                  (mkSubPkgsOverlay "nixos-2003" inputs.rel2003)
                  (mkSubPkgsOverlay "nixos-2009" inputs.rel2009)
                  (mkSubPkgsOverlay "nixos-2105" inputs.rel2105)
                ];
              };
            })

            inputs.myxmonad.nixosModule
            inputs.myfish.nixosModule
            inputs.myemacs.nixosModule
          ] ++ (import ./modules/_list.nix);
          config = {
            hardware.enableRedistributableFirmware = true;
            nixpkgs.overlays = [ inputs.nur.overlay ];
          };
        };
      };

      nixosConfigurationsGen = {
        host-x1extremeG2 = moreModules: metadataOverride:
          (self.lib.evalConfiguration "x86_64-linux" "x1extremeG2" ([
            { config = { hardware.enableRedistributableFirmware = true; }; }
            self.nixosModules.core
            inputs.license-compliance-toolbox.nixosModule
            ({ myconfig, ... }: {
              imports = [
                (myconfig.metadatalib.announceHost "workstation")
                (myconfig.metadatalib.announceHost "nas")
                (myconfig.metadatalib.announceHost "vserver")
                (myconfig.metadatalib.announceHost "nuc")
                (myconfig.metadatalib.announceHost "pi4")
                (myconfig.metadatalib.announceHost "pi3a")
                (myconfig.metadatalib.announceHost "pi0")
              ];
            })
            inputs.zephyrproject.nixosModule
          ] ++ moreModules) metadataOverride);
        host-workstation = moreModules: metadataOverride:
          (self.lib.evalConfiguration "x86_64-linux" "workstation" ([
            self.nixosModules.core
            inputs.license-compliance-toolbox.nixosModule
          ] ++ moreModules) metadataOverride);
        host-nas = moreModules: metadataOverride:
          (self.lib.evalConfiguration "x86_64-linux" "nas"
            ([ self.nixosModules.core ] ++ moreModules) metadataOverride);
        host-nuc = moreModules: metadataOverride:
          (self.lib.evalConfiguration "x86_64-linux" "nuc"
            ([ self.nixosModules.core
               inputs.zephyrproject.nixosModule
             ]
              ++ moreModules) metadataOverride);
      };

      ##########################################################################
      ## configurations ########################################################
      ##########################################################################

      nixosConfigurations = {
        x1extremeG2 = self.nixosConfigurationsGen.host-x1extremeG2 [ ] { };
        workstation = self.nixosConfigurationsGen.host-workstation [ ] { };
        nas = self.nixosConfigurationsGen.host-nas [ ] { };
        nuc = self.nixosConfigurationsGen.host-nuc [ ] { };

        container = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            self.nixosModules.activateHomeManager
            # self.nixosModules.core
            ({ pkgs, ... }: {
              boot.isContainer = true;

              # Let 'nixos-version --json' know about the Git revision
              # of this flake.
              system.configurationRevision =
                nixpkgs.lib.mkIf (self ? rev) self.rev;

              # Network configuration.
              networking.useDHCP = false;
              networking.firewall.allowedTCPPorts = [ 80 ];

              # Enable a web server.
              services.httpd = {
                enable = true;
                adminAddr = "morty@example.org";
              };
            })
          ];
        };
      };

    } (let

      eachDefaultSystem =
        inputs.flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" ];
      nixpkgsConfig = { allowUnfree = true; };

    in eachDefaultSystem (system: {
      legacyPackages = import inputs.nixpkgs {
        inherit system;
        config = nixpkgsConfig;
      };

      devShell = let
        pkgs = import inputs.nixpkgs {
          inherit system;
          config = nixpkgsConfig;
        };
      in pkgs.mkShell {
        nativeBuildInputs = with pkgs; [
          # nixos-rebuild
          git
          git-crypt
          git-secrets
          nixfmt
          age
        ];

        NIX_CONF_DIR = with pkgs;
          let
            nixConf = ''
              ${pkgs.lib.optionalString (builtins.pathExists /etc/nix/nix.conf)
              (builtins.readFile /etc/nix/nix.conf)}
              experimental-features = nix-command flakes ca-references
            '';
            # access-tokens = "github.com=${secrets.git.github.oauth-token}"
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
    }));
}
