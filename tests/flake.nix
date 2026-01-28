{
  # This is a separate flake file from the one at the root because we don't
  # want to depend on nixpkgs
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
  outputs =
    { nixpkgs, ... }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
      lib = pkgs.lib;
      haskellPackages = pkgs.haskell.packages.ghc912;
      cabal2nix = haskellPackages.callCabal2nix "jail-nix-tests" (lib.fileset.toSource {
        root = ./.;
        fileset = ./jail-nix-tests.cabal;
      }) { };
      testDependencies = [
        (haskellPackages.ghc.withPackages (p: cabal2nix.buildInputs))
        pkgs.cabal-install
      ];
      mkTestPackage =
        name: runtimeInputs: script:
        pkgs.writeShellApplication {
          inherit name runtimeInputs;
          text = ''
            if ! [ -e jail-nix-tests.cabal ]; then
              if [ -e tests/jail-nix-tests.cabal ]; then
                cd tests
              else
                echo "${name} must be called in repo root or tests directory"
                exit 1
              fi
            fi
            ${script}
          '';
        };
    in
    rec {
      packages.x86_64-linux = rec {
        all-for-ci = pkgs.linkFarmFromDrvs "all-for-ci" [
          run-tests
          check-fmt
          fmt
          devShells.x86_64-linux.default
        ];

        run-tests = mkTestPackage "run-tests" testDependencies ''
          ${
            lib.pipe
              {
                jail-nix =
                  with lib.fileset;
                  toSource {
                    root = ../.;
                    fileset = difference ../. (unions [
                      ../.build.yml
                      ../LICENSE
                      ../README.md
                      ../tests
                      ../website
                    ]);
                  };
                nixpkgs = nixpkgs;
              }
              [
                (lib.mapAttrsToList (name: path: "${name}=${path}"))
                (lib.concatStringsSep " ")
                (lib.toShellVar "NIX_PATH")
              ]
          } cabal run spec -- "$@"
        '';

        check-fmt = mkTestPackage "check-fmt" [ pkgs.nixfmt-tree ] "treefmt --ci";

        fmt = mkTestPackage "fmt" [ pkgs.nixfmt-tree ] "treefmt";
      };

      devShells.x86_64-linux.default = pkgs.mkShell {
        buildInputs = testDependencies ++ [
          haskellPackages.haskell-language-server
          pkgs.ghcid
          pkgs.hpack
        ];
      };
    };
}
