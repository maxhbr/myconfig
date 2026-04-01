{
  description = "myconfig";

  inputs = {
    master.url = "github:nixos/nixpkgs?ref=master";
    nixos-unstable-small.url = "github:nixos/nixpkgs?ref=nixos-unstable-small";
    nixos-unstable.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";

    nix-index-database.url = "github:nix-community/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";

    git-hooks.url = "github:cachix/git-hooks.nix";

    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";
    nixpkgs-wayland.inputs.nixpkgs.follows = "nixpkgs";

    home.url = "github:nix-community/home-manager";
    home.inputs.nixpkgs.follows = "nixpkgs";

    impermanence.url = "github:nix-community/impermanence";
    impermanence.inputs.nixpkgs.follows = "nixpkgs";

    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    agenix.inputs.darwin.follows = ""; # optionally choose not to download darwin deps (saves some resources on Linux)

    flake-utils.url = "github:numtide/flake-utils";

    nur.url = "github:nix-community/NUR";
    nur.inputs.nixpkgs.follows = "nixpkgs";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    my-wallpapers.url = "github:maxhbr/wallpapers";
    my-wallpapers.inputs.nixpkgs.follows = "nixpkgs";

    zephyr-flake.url = "github:maxhbr/zephyr-flake";
    zephyr-flake.inputs.nixpkgs.follows = "nixpkgs";

    # octrc.url = "github:maxhbr/octrc";
    # octrc.inputs.nixpkgs.follows = "nixpkgs";

    clipboard-sync.url = "github:dnut/clipboard-sync";
    clipboard-sync.inputs.nixpkgs.follows = "nixpkgs";

    myphoto.url = "github:maxhbr/myphoto";
    myphoto.inputs.nixpkgs.follows = "nixpkgs";

    #############################################################
    # PRs
    # https://github.com/NixOS/nixpkgs/pull/502283
    pr502283.url = "github:Stebalien/nixpkgs/ff077fe943fab73adf58ad2f0f7650f49a1fda61";
  };

  outputs =
    { self, nixpkgs, ... }@inputs:
    let
      inherit (inputs.nixpkgs) lib;
      nixpkgsConfig = {
        allowUnfree = true;
        allowUnfreePredicate = (pkg: true);
        segger-jlink.acceptLicense = true;
        allowBroken = false;
      };
    in
    lib.recursiveUpdate
      {
        aggregatedInputs = inputs;
        lib = import ./flake.lib.nix inputs;

        ##########################################################################
        ## profiles and modules ##################################################
        ##########################################################################

        nixosModules = rec {
          readOnlyPkgs = {
            imports = [ nixpkgs.nixosModules.readOnlyPkgs ];
            nixpkgs.pkgs = nixpkgs.legacyPackages.x86_64-linux;
          };
          core =
            { ... }:
            {
              imports = [
                (
                  { pkgs, ... }:
                  {
                    config.nixpkgs = {
                      overlays =
                        let
                          mkSubPkgsOverlay =
                            targetName: input:
                            (self: super: {
                              "${targetName}" =
                                super."${targetName}" or { }
                                // import input {
                                  inherit (pkgs) system;
                                  config = pkgs.config // nixpkgsConfig;
                                };
                            });
                        in
                        [
                          (mkSubPkgsOverlay "master" inputs.master)
                          (mkSubPkgsOverlay "nixos-unstable" inputs.nixos-unstable)
                          (mkSubPkgsOverlay "nixos-unstable-small" inputs.nixos-unstable-small)
                          # (mkSubPkgsOverlay "nixos-2511" inputs.rel2511)
                          # (mkSubPkgsOverlay "nixos-2505" inputs.rel2505)
                          # (mkSubPkgsOverlay "nixos-2405" inputs.rel2405)
                          # (mkSubPkgsOverlay "nixos-2411" inputs.rel2411)
                          # (mkSubPkgsOverlay "stable" inputs.rel2411)
                        ];
                    };
                  }
                )
                (
                  { pkgs, config, ... }:
                  {
                    # To use a version from a PR, use the following:
                    ## 1. create an input with the following:
                    # pr275479.url =
                    #  "github:maxhbr/nixpkgs/freeplane-1_11_8"; # https://github.com/NixOS/nixpkgs/pull/275479
                    ## 2. add the input to the inputs list below
                    # { input = "pr275479"; pkg = "freeplane"; maxVersion = null; }
                    nixpkgs.overlays =
                      map
                        (
                          {
                            input,
                            pkg,
                            maxVersion ? null,
                          }:
                          let
                            prPkg = import inputs."${input}" {
                              inherit (pkgs) config system;
                            };
                            prVersion = prPkg."${pkg}";
                          in
                          (_: super: {
                            "${pkg}" =
                              if maxVersion == null then
                                prVersion
                              else if pkgs.lib.versionOlder (pkgs.lib.getVersion prVersion) maxVersion then
                                prVersion
                              else
                                super."${pkg}" or prVersion;
                          })
                        )
                        [
                          {
                            input = "pr502283";
                            pkg = "llama-swap";
                            # maxVersion = "199";
                          }
                        ];
                  }
                )
                (
                  { pkgs, ... }:
                  {
                    nixpkgs.overlays = [
                      (_: _: {
                        mybackup = pkgs.callPackage ../pkgs/mybackup { inherit pkgs; };
                        my-wallpapers = inputs.my-wallpapers.defaultPackage.x86_64-linux;
                      })
                    ];
                  }
                )
                inputs.my-wallpapers.nixosModule

                (
                  { pkgs, config, ... }:
                  {
                    config = {
                      nix.settings = {
                        trusted-public-keys = [
                          "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
                        ];
                        substituters = [ "https://cache.nixos.org" ];
                      };
                    };
                  }
                )
                {
                  nixpkgs.overlays = [ inputs.nur.overlays.default ];
                }
              ]
              ++ (map (n: ./modules + "/${n}") (builtins.attrNames (builtins.readDir ./modules)));
              config = {
                hardware.enableRedistributableFirmware = true;
              };
            };
        };

        nixosConfigurationsGen = {
          host-f13 =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "x86_64-linux" "f13" (
              [
                self.nixosModules.core
                (
                  { pkgs, myconfig, ... }:
                  {
                    imports = [ (myconfig.metadatalib.announceOtherHosts "f13") ];
                  }
                )
                {
                  myconfig.upg.enable = true;
                  myconfig.upg.otherHosts = [
                    "p14"
                    "workstation"
                    "r6c"
                    "nas"
                    "vserver"
                    "nuc"
                  ];
                  home-manager.sharedModules = [
                    {
                      programs.mr.settings = {
                        "myconfig/thing-priv/" = {
                          checkout = "git clone ssh://thing/home/mhuber/myconfig/priv/.git";
                          update = "git pull --rebase";
                        };
                        "myconfig/p14-priv/" = {
                          checkout = "git clone ssh://p14/home/mhuber/myconfig/priv/.git";
                          update = "git pull --rebase";
                        };
                      };
                    }
                  ];
                }
              ]
              ++ moreModules
            ) metadataOverride);
          host-p14 =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "x86_64-linux" "p14" (
              [
                self.nixosModules.core
                {
                  myconfig.upg.enable = true;
                }
              ]
              ++ moreModules
            ) metadataOverride);
          host-x1extremeG2 =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "x86_64-linux" "x1extremeG2" (
              [
                {
                  config = {
                    hardware.enableRedistributableFirmware = true;
                  };
                }
                self.nixosModules.core
              ]
              ++ moreModules
            ) metadataOverride);
          host-workstation =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "x86_64-linux" "workstation" (
              [ self.nixosModules.core ] ++ moreModules
            ) metadataOverride);
          host-vserver =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "x86_64-linux" "vserver" (
              [ self.nixosModules.core ] ++ moreModules
            ) metadataOverride);
          host-nas =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "x86_64-linux" "nas" (
              [ self.nixosModules.core ] ++ moreModules
            ) metadataOverride);
          host-nuc =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "x86_64-linux" "nuc" (
              [ self.nixosModules.core ] ++ moreModules
            ) metadataOverride);
          host-pi4 =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "aarch64-linux" "pi4" (
              [ self.nixosModules.core ] ++ moreModules
            ) metadataOverride);
          host-pi3a =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "aarch64-linux" "pi3a" (
              [ self.nixosModules.core ] ++ moreModules
            ) metadataOverride);
          host-r6c =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "aarch64-linux" "r6c" (
              [ self.nixosModules.core ] ++ moreModules
            ) metadataOverride);
          host-thing =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "x86_64-linux" "thing" (
              [
                self.nixosModules.core
                { myconfig.upg.enable = true; }
              ]
              ++ moreModules
            ) metadataOverride);
        };

        ##########################################################################
        ## configurations ########################################################
        ##########################################################################

        nixosConfigurations = {
          f13 = self.nixosConfigurationsGen.host-f13 [ ] { };
          p14 = self.nixosConfigurationsGen.host-p14 [ ] { };
          # x1extremeG2 = self.nixosConfigurationsGen.host-x1extremeG2 [ ] { };
          workstation = self.nixosConfigurationsGen.host-workstation [ ] { };
          vserver = self.nixosConfigurationsGen.host-vserver [ ] { };
          nas = self.nixosConfigurationsGen.host-nas [ ] { };
          nuc = self.nixosConfigurationsGen.host-nuc [ ] { };
          r6c = self.nixosConfigurationsGen.host-r6c [ ] { };
          pi4 = self.nixosConfigurationsGen.host-pi4 [ ] { };
          pi3a = self.nixosConfigurationsGen.host-pi3a [ ] { };
          thing = self.nixosConfigurationsGen.host-thing [ ] { };

          # container = nixpkgs.lib.nixosSystem {
          #   system = "x86_64-linux";
          #   modules = [
          #     self.nixosModules.activateHomeManager
          #     self.nixosModules.readOnlyPkgs
          #     # self.nixosModules.core
          #     ({ pkgs, ... }: {
          #       boot.isContainer = true;

          #       # Let 'nixos-version --json' know about the Git revision
          #       # of this flake.
          #       system.configurationRevision =
          #         nixpkgs.lib.mkIf (self ? rev) self.rev;

          #       # Network configuration.
          #       networking.useDHCP = false;
          #       networking.firewall.allowedTCPPorts = [ 80 ];

          #       # Enable a web server.
          #       services.httpd = {
          #         enable = true;
          #         adminAddr = "morty@example.org";
          #       };
          #     })
          #   ];
          # };
        };

      }
      (
        let
          eachDefaultSystem = inputs.flake-utils.lib.eachSystem [
            "x86_64-linux"
            "aarch64-linux"
          ];
        in
        eachDefaultSystem (system: {
          # might be overwritten in priv
          packages = {
            myconfig-iso = self.lib.mkISO {
              inherit system;
              hostName = "iso";
              nixosModules = [ self.nixosModules.core ];
              metadataOverride = { };
              bootstrappedConfig = null;
            };
          };

          formatter = nixpkgs.legacyPackages.${system}.nixfmt-tree;

          checks = {
            pre-commit-check = inputs.git-hooks.lib.${system}.run {
              src = ./.;
              hooks = {
                nixfmt-rfc-style.enable = true;
                # shfmt.enable = true;
                # shfmt.settings.simplify = true;
                # shellcheck.enable = true;
                # typos.enable = true;
              };
            };
            shell-fmt-check =
              let
                pkgs = inputs.nixpkgs.legacyPackages."${system}";
                files = pkgs.lib.concatStringsSep " " [
                  "switch.sh"
                ];
              in
              pkgs.stdenv.mkDerivation {
                name = "shell-fmt-check";
                src = ./.;
                doCheck = true;
                nativeBuildInputs = with pkgs; [
                  shellcheck
                  shfmt
                ];
                checkPhase = ''
                  shfmt -d -s -i 4 -ci ${files}
                  shellcheck -x ${files}
                '';
                installPhase = ''
                  mkdir "$out"
                '';
              };
          };

          devShells.default =
            let
              pkgs = import inputs.nixpkgs {
                inherit system;
                config = nixpkgsConfig;
              };
            in
            pkgs.mkShell {
              nativeBuildInputs = with pkgs; [
                git
                git-crypt
                git-secrets
                nixfmt
                age
              ];

              # TODO: building the CONF_DIR with content from /etc/... makes this require --impure flag
              NIX_CONF_DIR =
                with pkgs;
                let
                  nixConf = ''
                    ${pkgs.lib.optionalString (builtins.pathExists /etc/nix/nix.conf) (
                      builtins.readFile /etc/nix/nix.conf
                    )}
                    experimental-features = nix-command flakes ca-references
                  '';
                  # access-tokens = "github.com=${secrets.git.github.oauth-token}"
                in
                linkFarm "nix-conf-dir" (
                  [
                    {
                      name = "nix.conf";
                      path = writeText "flakes-nix.conf" nixConf;
                    }
                  ]
                  ++ (lib.optionals (builtins.pathExists /etc/nix/registry.json) [
                    {
                      name = "registry.json";
                      path = /etc/nix/registry.json;
                    }
                  ])
                  ++ (lib.optionals (builtins.pathExists /etc/nix/machines) [
                    {
                      name = "machines";
                      path = /etc/nix/machines;
                    }
                  ])
                );
            };
        })
      );
}
