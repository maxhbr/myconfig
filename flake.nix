{
  description = "myconfig";

  inputs = {
    master.url = "github:nixos/nixpkgs/master";
    staged.url = "github:nixos/nixpkgs/staging";
    small.url = "github:nixos/nixpkgs/nixos-unstable-small";
    large.url = "github:nixos/nixpkgs/nixos-unstable";
    rel2009.url = "github:nixos/nixpkgs/nixos-20.09";
    rel2003.url = "github:nixos/nixpkgs/nixos-20.03";

    home.url = "github:nix-community/home-manager";
    home.inputs.nixpkgs.follows = "/master";

    utils.url = "github:numtide/flake-utils";

    nix.url = "github:nixos/nix/flakes";
    nix.inputs.nixpkgs.follows = "master";

    nur.url = "github:nix-community/NUR";

    emacs.url = "github:nix-community/emacs-overlay";
    nix-doom-emacs.url = "github:vlaci/nix-doom-emacs";
    nix-doom-emacs.inputs.nixpkgs.follows = "master";

    myemacs.url = "path:flakes/myemacs/";

    flake-utils.url = "github:numtide/flake-utils";

    hardware.url = "github:nixos/nixos-hardware";
  };

  outputs = inputs:
    let
      channels = with inputs; {
        pkgs = small;
        modules = master;
        lib = master;
      };
      inherit (channels.lib) lib;

      allSystems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      eachDefaultSystem = inputs.flake-utils.lib.eachSystem allSystems;

    in (import ./flake.nixosConfigurations.nix {inherit inputs channels; }) // (eachDefaultSystem (system: {
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
    }));
}
