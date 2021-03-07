{
  description = "myconfig";

  inputs = {
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

    myxmonad.url = "path:flakes/myxmonad/";
    myxmonad.inputs.nixpkgs.follows = "nixpkgs";
    myxmonad.inputs.flake-utils.follows = "flake-utils";

    hardware.url = "github:nixos/nixos-hardware";
  };

  outputs = inputs:
    let
      inherit (inputs.nixpkgs) lib;

      allSystems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      eachDefaultSystem = inputs.flake-utils.lib.eachSystem allSystems;

    in (import ./flake.nixosConfigurations.nix {inherit inputs; }) // (eachDefaultSystem (system: {
      legacyPackages = import inputs.nixpkgs { inherit system; };

      devShell = let pkgs = import inputs.nixpkgs { inherit system; };
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
    }));
}
