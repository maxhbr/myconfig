{
  description = "myconfig";

  inputs = {
    private.url = "path:../myconfig-private";

    master.url = "github:nixos/nixpkgs/master";
    staged.url = "github:nixos/nixpkgs/staging";
    small.url = "github:nixos/nixpkgs/nixos-unstable-small";
    large.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    rel2009.url = "github:nixos/nixpkgs/nixos-20.09";
    rel2003.url = "github:nixos/nixpkgs/nixos-20.03";

    home.url = "github:nix-community/home-manager";
    home.inputs.nixpkgs.follows = "nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";

    # nix.url = "github:nixos/nix/flakes";
    # nix.inputs.nixpkgs.follows = "nixpkgs";

    nur.url = "github:nix-community/NUR";

    emacs.url = "github:nix-community/emacs-overlay";
    nix-doom-emacs.url = "github:vlaci/nix-doom-emacs";
    nix-doom-emacs.inputs.nixpkgs.follows = "nixpkgs";

    myemacs.url = "path:flakes/myemacs/";
    myemacs.inputs.nix-doom-emacs.follows = "nix-doom-emacs";

    myfish.url = "path:flakes/myfish/";

    myxmonad.url = "path:flakes/myxmonad/";
    myxmonad.inputs.nixpkgs.follows = "nixpkgs";
    myxmonad.inputs.flake-utils.follows = "flake-utils";

    license-compliance-toolbox.url = "path:flakes/license-compliance-toolbox/";
    license-compliance-toolbox.inputs.nixpkgs.follows = "nixpkgs";

    hardware.url = "github:nixos/nixos-hardware";
  };

  outputs = { self, ... }@inputs:
    let inherit (inputs.nixpkgs) lib;
    in lib.recursiveUpdate {
      lib = import ./outputs.lib inputs;

      ##########################################################################
      ## profiles and modules ##################################################
      ##########################################################################

      nixosModules.core = { ... }: {
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
                (mkSubPkgsOverlay "unstable" inputs.master)
                (mkSubPkgsOverlay "nixos-unstable" inputs.large)
                (mkSubPkgsOverlay "nixos-unstable-small" inputs.small)
                (mkSubPkgsOverlay "nixos-2003-small" inputs.rel2003)
                (mkSubPkgsOverlay "nixos-2009-small" inputs.rel2009)
              ];
            };
          })
          inputs.home.nixosModules.home-manager
          ({ ... }: {
            config = {
              home-manager = {
                useUserPackages = true;
                useGlobalPkgs = true;
              };
            };
          })

          inputs.myfish.nixosModule
          inputs.myemacs.nixosModule
        ] ++ (import ./nixosModules/_list.nix);
        config.nixpkgs.overlays = [ inputs.nur.overlay ];
      };
      hmModules.core = { ... }: {
        imports = [inputs.myemacs.hmModule inputs.myfish.hmModule ]
          ++ (import ./hmModules/_list.nix);
      };

      ##########################################################################
      ## configurations ########################################################
      ##########################################################################

      nixosConfigurations = {
        container = self.lib.evalConfiguration "x86_64-linux" "x1extremeG2" {
          config = { boot.isContainer = true; };
        };
        x1extremeG2 = self.lib.evalConfiguration "x86_64-linux" "x1extremeG2" {
          nixosModules = [
            { config = { hardware.enableRedistributableFirmware = true; }; }
            self.nixosModules.core
          ];
          hmModules = [ self.hmModules.core ];
        };
        workstation = self.lib.evalConfiguration "x86_64-linux" "workstation" {
          # imports = [ (myconfig.lib.fixIp "workstation" "enp39s0") ];
        };
      };

    } (let

      eachDefaultSystem = inputs.flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" ];
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
        nativeBuildInputs = with pkgs; [ git git-crypt git-secrets nixfmt age ];

        NIX_CONF_DIR = with pkgs;
          let
            nixConf = ''
              ${pkgs.lib.optionalString (builtins.pathExists /etc/nix/nix.conf)
              (builtins.readFile /etc/nix/nix.conf)}
              experimental-features = nix-command flakes ca-references
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
    }));
}
