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
    {
      lib = import ./outputs.lib inputs;

      ##########################################################################
      ## profiles and modules ##################################################
      ##########################################################################
      nixosModules.myemacs = inputs.myemacs.nixosModule;
      hmModules.myemacs = inputs.myemacs.hmModule;

      nixosModules.myfish = inputs.myfish.nixosModule;
      hmModules.myfish = inputs.myfish.hmModule;

      nixosModules.core = { ... }: {
        imports = [
          inputs.home.nixosModules.home-manager
          ({ config, lib, ... }: {
            config = {
              home-manager = {
                useUserPackages = true;
                useGlobalPkgs = true;
              };
            };
          })

          self.nixosModules.myfish
          self.nixosModules.myemacs
        ] ++ (import ./nixosModules/_list.nix);
        config.nixpkgs.overlays = [ inputs.nur.overlay ];
      };
      hmModules.core = { ... }: {
        imports = [ self.hmModules.myfish self.hmModules.myemacs ]
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

      ##########################################################################
      ##########################################################################
      ##########################################################################
    } // (let
      allSystems = [ "x86_64-linux" "aarch64-linux" ];
      eachDefaultSystem = inputs.flake-utils.lib.eachSystem allSystems;

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
        nativeBuildInputs = with pkgs; [ git git-crypt git-secrets nixfmt ];
        shellHook = ''
          mkdir -p secrets
        '';

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
