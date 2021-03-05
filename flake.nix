{
  description = "A very basic flake";

  inputs = {
    master.url = "github:nixos/nixpkgs/master";               #|.
    staged.url = "github:nixos/nixpkgs/staging";              #| |-- Nix
    small.url  = "github:nixos/nixpkgs/nixos-unstable-small"; #| |--   pkgs
    large.url  = "github:nixos/nixpkgs/nixos-unstable";       #|'

    home.url = "github:nix-community/home-manager"; #|- Home-manager
    home.inputs.nixpkgs.follows = "/master";        #|

    nix.url = "github:nixos/nix/flakes";
    nix.inputs.nixpkgs.follows = "master";

    emacs.url = "github:nix-community/emacs-overlay";
    utils.url = "github:numtide/flake-utils";

    flake-utils.url = "github:numtide/flake-utils";

    hardware = { url = "github:nixos/nixos-hardware"; flake = false; };
  };

  outputs = inputs: let
    allSystems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
    config = {
      allowUnfree = true;
      android_sdk.accept_license = true;
    };
    channels = with inputs; {
      pkgs = small;       # For packages
      modules = master;   # For nixos modules
      lib = master;       # For flake-wide lib
    }; inherit (channels.lib) lib; # this ^

    eachDefaultSystem = inputs.flake-utils.lib.eachSystem ["x86_64-linux"];

    # inherit (inputs.self.passthru) secrets;
  in {

################################################################################
################################################################################
################################################################################
    nixosConfigurations = let
      mkConfiguration = system: hostName:
        lib.nixosSystem {
          inherit system;
          modules = [
            (./host + ".${hostName}")
            {
              environment.etc."machine-id".text = builtins.hashString "md5" hostName;
              networking = { inherit hostName; };
            }
          ];
        };
    in {
      x1extremeG2 = mkConfiguration "x86_64-linux" "x1extremeG2";
    };

    nixosModules =
      {
        hosts = eachDefaultSystem (system: (let
          myconfig = {};

          # The flake-ier common basic stuff
          global = {
            nix.package = lib.mkDefault pkgs.nixFlakes;
            nix.registry = lib.mapAttrs (id: flake: {
              inherit flake;
              from = { inherit id; type = "indirect"; };
            }) (inputs // { nixpkgs = inputs.master; });
            nix.nixPath = lib.mapAttrsToList (k: v: "${k}=${toString v}") {
              nixpkgs = "${channels.pkgs}/";
              nixos = "${inputs.self}/";
              home-manager = "${inputs.home}/";
              self = "/run/current-system/flake/input/self/";
              flake = "/srv/git/github.com/bqv/nixrc";
            };
            system.configurationRevision = inputs.self.rev or "dirty";
          };

          pkgs = channels.modules.legacyPackages.${system};
          nixpkgs = { config, ... }: {
            config.nixpkgs = {
              inherit pkgs;
              system = config.platform;
            };
          };

          # Modules to propagate to containers
          extraModules = [
            global
          ];

          # Final modules set
          modules = [
            nixpkgs
          ];

          specialArgs = {
            inherit myconfig;
            flake = inputs.self;

            # inherit (secrets) hosts domains;

            modules = modules ++ [
              { _module.args = specialArgs; }
            ];
            inherit extraModules;
          };
        in  {
          inherit system specialArgs modules;
        }));
      };

################################################################################
################################################################################
################################################################################
    devShell = eachDefaultSystem (system:
      let
        pkgs = import channels.pkgs { inherit system; };
      in pkgs.mkShell {
        nativeBuildInputs = with pkgs; [git git-crypt git-secrets nixfmt];
        shellHook = ''
          mkdir -p secrets
        '';

        NIX_CONF_DIR = with pkgs; let
          nixConf = ''
            ${lib.optionalString (builtins.pathExists /etc/nix/nix.conf)
              (builtins.readFile /etc/nix/nix.conf)}
            experimental-features = nix-command flakes ca-references
            print-build-logs = true
          ''; # access-tokens = "github.com=${secrets.git.github.oauth-token}"
        in linkFarm "nix-conf-dir" ( [
          { name = "nix.conf"; path = writeText "flakes-nix.conf" nixConf; }
          { name = "registry.json"; path = /etc/nix/registry.json; }
          { name = "machines"; path = /etc/nix/machines; }
        ] );
      }
    );

    legacyPackages = eachDefaultSystem (system: import channels.pkgs { inherit system; });
    passthru = rec {
      inherit inputs channels;
    };
  };
}
