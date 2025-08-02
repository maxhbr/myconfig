{
  description = "myconfig";

  inputs = {
    master.url = "github:nixos/nixpkgs/master";
    nixos-unstable-small.url = "github:nixos/nixpkgs/nixos-unstable-small";
    nixos-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    rel2405.url = "github:nixos/nixpkgs/release-24.05";
    rel2411.url = "github:nixos/nixpkgs/release-24.11";

    nixpkgs-wayland = {
      url = "github:nix-community/nixpkgs-wayland";
      # only needed if you use as a package set:
      inputs.nixpkgs.follows = "nixpkgs";
      # nixpkgs-wayland.inputs.master.follows = "master";
    };

    home.url = "github:nix-community/home-manager";
    home.inputs.nixpkgs.follows = "nixpkgs";

    impermanence.url = "github:nix-community/impermanence";

    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    agenix.inputs.darwin.follows = ""; # optionally choose not to download darwin deps (saves some resources on Linux)

    flake-utils.url = "github:numtide/flake-utils";

    nur.url = "github:nix-community/NUR";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    emacs.url = "github:nix-community/emacs-overlay";
    doomemacs.url = "github:hlissner/doom-emacs";
    doomemacs.flake = false;

    my-wallpapers.url = "github:maxhbr/wallpapers";
    my-wallpapers.inputs.nixpkgs.follows = "nixpkgs";

    zephyr-flake.url = "github:maxhbr/zephyr-flake";
    # zephyr-flake.inputs.nixpkgs.follows = "nixpkgs";

    # octrc.url = "github:maxhbr/octrc";
    # octrc.inputs.nixpkgs.follows = "nixpkgs";

    nixgl.url = "github:nix-community/nixGL";

    clipboard-sync.url = "github:dnut/clipboard-sync";

    myphoto.url = "github:maxhbr/myphoto";
    myphoto.inputs.nixpkgs.follows = "nixpkgs";
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
                          (mkSubPkgsOverlay "nixos-unstable-small" inputs.ninos-unstable-small)
                          (mkSubPkgsOverlay "nixos-2405" inputs.rel2405)
                          (mkSubPkgsOverlay "nixos-2411" inputs.rel2411)
                          (mkSubPkgsOverlay "stable" inputs.rel2411)
                        ];
                    };
                  }
                )
                (
                  { pkgs, ... }:
                  {
                    # To use a version from a PR, use the following:
                    ## 1. create an input with the following:
                    # pr275479.url =
                    #  "github:maxhbr/nixpkgs/freeplane-1_11_8"; # https://github.com/NixOS/nixpkgs/pull/275479
                    ## 2. add the input to the inputs list
                    # { input = "pr275479"; pkg = "freeplane"; }
                    nixpkgs.overlays = map (
                      { input, pkg }:
                      (_: _: {
                        "${pkg}" =
                          (import inputs."${input}" {
                            inherit (pkgs) config system;
                          })."${pkg}";
                      })
                    ) [ ];
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
                (
                  { pkgs, ... }:
                  {
                    nixpkgs.overlays = [
                      inputs.nixgl.overlay # https://github.com/nix-community/nixGL
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
              ] ++ (map (n: "${./modules}/${n}") (builtins.attrNames (builtins.readDir ./modules)));
              config = {
                hardware.enableRedistributableFirmware = true;
                nixpkgs.overlays = [ inputs.nur.overlays.default ];
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
              ]
              ++ moreModules
            ) metadataOverride);
          host-p14 =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "x86_64-linux" "p14" (
              [
                self.nixosModules.core
                (
                  { pkgs, myconfig, ... }:
                  {
                    imports = [ (myconfig.metadatalib.announceOtherHosts "p14") ];
                    config = {
                      home-manager.sharedModules = [
                        (
                          { config, ... }:
                          {
                            home.packages = [
                              (pkgs.writeShellScriptBin "myconfig" (
                                if config.programs.neovide.enable then
                                  "${config.programs.neovide.package}/bin/neovide ~/myconfig/myconfig &disown"
                                else
                                  "$EDITOR  ~/myconfig/myconfig"
                              ))
                            ];
                          }
                        )
                      ];
                    };
                  }
                )
              ]
              ++ moreModules
            ) metadataOverride);
          host-brain =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "x86_64-linux" "brain" (
              [ self.nixosModules.core ] ++ moreModules
            ) metadataOverride);
          host-spare =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "x86_64-linux" "spare" (
              [ self.nixosModules.core ] ++ moreModules
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
        };

        ##########################################################################
        ## configurations ########################################################
        ##########################################################################

        nixosConfigurations = {
          f13 = self.nixosConfigurationsGen.host-f13 [ ] { };
          p14 = self.nixosConfigurationsGen.host-p14 [ ] { };
          spare = self.nixosConfigurationsGen.host-spare [ ] { };
          brain = self.nixosConfigurationsGen.host-brain [ ] { };
          # x1extremeG2 = self.nixosConfigurationsGen.host-x1extremeG2 [ ] { };
          workstation = self.nixosConfigurationsGen.host-workstation [ ] { };
          vserver = self.nixosConfigurationsGen.host-vserver [ ] { };
          nas = self.nixosConfigurationsGen.host-nas [ ] { };
          nuc = self.nixosConfigurationsGen.host-nuc [ ] { };
          # pi4 = self.nixosConfigurationsGen.host-pi4 [ ] { };
          # pi3a = self.nixosConfigurationsGen.host-pi3a [ ] { };

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

            pi4-sd-image = inputs.self.nixosConfigurations.pi4.config.system.build.sdImage;
            pi3a-sd-image = inputs.self.nixosConfigurations.pi3a.config.system.build.sdImage;
          };

          devShell =
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
                linkFarm "nix-conf-dir" ([
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
        })
      );
}
