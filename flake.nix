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
    rel2111.url = "github:nixos/nixpkgs/release-21.11";
    rel2205.url = "github:nixos/nixpkgs/release-22.05";
    rel2211.url = "github:nixos/nixpkgs/release-22.11";
    rel2305.url = "github:nixos/nixpkgs/release-23.05";
    rel2311.url = "github:nixos/nixpkgs/release-23.11";
    rel2405.url = "github:nixos/nixpkgs/release-24.05";
    rel2411.url = "github:nixos/nixpkgs/release-24.11";

    zoom-old-screenshare.url = "github:NixOS/nixpkgs/06031e8a5d9d5293c725a50acf01242193635022";

    pr244937.url =
      "github:charles-dyfis-net/nixpkgs/freeplane-1_11_4"; # https://github.com/NixOS/nixpkgs/pull/244937
    pr275479.url =
      "github:maxhbr/nixpkgs/freeplane-1_11_8"; # https://github.com/NixOS/nixpkgs/pull/275479

    nixpkgs-wayland = {
      url = "github:nix-community/nixpkgs-wayland";
      # only needed if you use as a package set:
      inputs.nixpkgs.follows = "nixpkgs";
      # nixpkgs-wayland.inputs.master.follows = "master";
    };

    home.url = "github:nix-community/home-manager";
    home.inputs.nixpkgs.follows = "nixpkgs";

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

    # zephyrproject.url = "path:flakes/zephyrproject/";

    nixgl.url = "github:nix-community/nixGL";

    #wayland:hyprland
    hyprland = {
      url = "github:hyprwm/Hyprland/v0.41.2";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyprland-plugins = {
      url = "github:hyprwm/hyprland-plugins";
      inputs.hyprland.follows = "hyprland";
    };

    river-src.url = "git+https://github.com/riverwm/river?submodules=1";
    river-src.flake = false;
    rivercarro-src.url =
      "git+https://git.sr.ht/~novakane/rivercarro?submodules=1";
    rivercarro-src.flake = false;

    niri.url = "github:YaLTeR/niri";
    niri.inputs.nixpkgs.follows = "nixpkgs";

    mydwl.url = "github:maxhbr/mydwl";
    mydwl.inputs.nixpkgs.follows = "nixpkgs";

    myphoto.url = "github:maxhbr/myphoto";
    myphoto.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      inherit (inputs.nixpkgs) lib;
      nixpkgsConfig = {
        allowUnfree = true;
        allowUnfreePredicate = (pkg: true);
        segger-jlink.acceptLicense = true;
        allowBroken = false;
      };
    in lib.recursiveUpdate {
      aggregatedInputs = inputs;
      lib = import ./flake.lib.nix inputs;

      ##########################################################################
      ## profiles and modules ##################################################
      ##########################################################################

      nixosModules = rec {
        activateHomeManager = { config, lib, ... }: {
          imports = [
            # home manager:
            inputs.home.nixosModules.home-manager
          ];

          config = {
            home-manager = {
              useUserPackages = true;
              useGlobalPkgs = true;
              backupFileExtension = let
                  rev = toString (self.shortRev or self.dirtyShortRev or self.lastModified or "unknown");
                in "${rev}.homeManagerBackup";
              sharedModules = [
                ({ pkgs, ... }: {
                  home.stateVersion =
                    lib.mkDefault (config.system.stateVersion);
                  home.packages = [
                    pkgs.dconf
                  ]; # see: https://github.com/nix-community/home-manager/issues/3113
                })
              ];
            };
          };
        };
        readOnlyPkgs = {
          imports = [ nixpkgs.nixosModules.readOnlyPkgs ];
          nixpkgs.pkgs = nixpkgs.legacyPackages.x86_64-linux;
        };
        mydwl = import ./flake.nixosModules.mydwl.nix;
        core = { ... }: {
          imports = [
            ({ pkgs, ... }: {
              config.nixpkgs = {
                overlays = let
                  mkSubPkgsOverlay = targetName: input:
                    (self: super: {
                      "${targetName}" = super."${targetName}" or { }
                        // import input {
                          inherit (pkgs) system;
                          config = pkgs.config // nixpkgsConfig;
                        };
                    });
                in [
                  (mkSubPkgsOverlay "master" inputs.master)
                  (mkSubPkgsOverlay "nixos-unstable" inputs.nixos-unstable)
                  (mkSubPkgsOverlay "nixos-unstable-small"
                    inputs.ninos-unstable-small)
                  (mkSubPkgsOverlay "nixos-2003" inputs.rel2003)
                  (mkSubPkgsOverlay "nixos-2009" inputs.rel2009
                  )
                  (mkSubPkgsOverlay "nixos-2105" inputs.rel2105)
                  (mkSubPkgsOverlay "nixos-2111" inputs.rel2111)
                  (mkSubPkgsOverlay "nixos-2205" inputs.rel2205)
                  (mkSubPkgsOverlay "nixos-2111" inputs.rel2111)
                  (mkSubPkgsOverlay "nixos-2205" inputs.rel2205)
                  (mkSubPkgsOverlay "nixos-2211" inputs.rel2211)
                  (mkSubPkgsOverlay "nixos-2305" inputs.rel2305)
                  (mkSubPkgsOverlay "nixos-2311" inputs.rel2311)
                  (mkSubPkgsOverlay "nixos-2405" inputs.rel2405)
                  (mkSubPkgsOverlay "nixos-2411" inputs.rel2411)
                  (mkSubPkgsOverlay "stable" inputs.rel2411)
                ];
              };
            })
            ({ pkgs, ... }: {
              nixpkgs.overlays = map ({ input, pkg }:
                (_: _: {
                  "${pkg}" = (import inputs."${input}" {
                    inherit (pkgs) config system;
                  })."${pkg}";
                })) [
                  # { input = "pr275479"; pkg = "freeplane"; }
                ];
            })
            ({ pkgs, ... }: {
              nixpkgs.overlays = [
                (_: _: {
                  mybackup =
                    pkgs.callPackage ../pkgs/mybackup { inherit pkgs; };
                  my-wallpapers =
                    inputs.my-wallpapers.defaultPackage.x86_64-linux;
                })
              ];
            })
            ({ pkgs, ... }: {
              nixpkgs.overlays = [
                inputs.nixgl.overlay # https://github.com/nix-community/nixGL
              ];
            })
            inputs.my-wallpapers.nixosModule
            mydwl

            ({ pkgs, config, ... }: {
              config = {
                nix.settings = {
                  trusted-public-keys = [
                    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
                  ];
                  substituters = [
                    "https://cache.nixos.org"
                  ];
                };
              };
            })
          ] ++ (map (n: "${./modules}/${n}")
            (builtins.attrNames (builtins.readDir ./modules)));
          config = {
            hardware.enableRedistributableFirmware = true;
            nixpkgs.overlays = [ inputs.nur.overlays.default ];
          };
        };
      };

      nixosConfigurationsGen = {
        host-p14 = moreModules: metadataOverride:
          (self.lib.evalConfiguration "x86_64-linux" "p14" ([
            self.nixosModules.core
            ({ pkgs, myconfig, ... }: {
              imports = [
                (myconfig.metadatalib.announceHost "x1extremeG2")
                (myconfig.metadatalib.announceHost "spare")
                (myconfig.metadatalib.announceHost "workstation")
                (myconfig.metadatalib.announceHost "nas")
                (myconfig.metadatalib.announceHost "vserver")
                (myconfig.metadatalib.announceHost "nuc")
                (myconfig.metadatalib.announceHost "pi4")
                (myconfig.metadatalib.announceHost "pi3a")
                (myconfig.metadatalib.announceHost "pi0")
                (myconfig.metadatalib.announceHost "r6c")
              ];
              config = {
                home-manager.sharedModules = [({config, ...}: {
                  home.packages = [
                    (pkgs.writeShellScriptBin "myconfig" 
			(if config.programs.neovide.enable
				 then "${config.programs.neovide.package}/bin/neovide ~/myconfig/myconfig &disown"
				 else "$EDITOR  ~/myconfig/myconfig"))
                  ];
                })];
              };
            })
          ] ++ moreModules) metadataOverride);
        host-spare = moreModules: metadataOverride:
          (self.lib.evalConfiguration "x86_64-linux" "spare" ([
            self.nixosModules.core
          ] ++ moreModules) metadataOverride);
        host-x1extremeG2 = moreModules: metadataOverride:
          (self.lib.evalConfiguration "x86_64-linux" "x1extremeG2" ([
            { config = { hardware.enableRedistributableFirmware = true; }; }
            self.nixosModules.core
          ] ++ moreModules) metadataOverride);
        host-workstation = moreModules: metadataOverride:
          (self.lib.evalConfiguration "x86_64-linux" "workstation"
            ([ self.nixosModules.core ] ++ moreModules) metadataOverride);
        host-vserver = moreModules: metadataOverride:
          (self.lib.evalConfiguration "x86_64-linux" "vserver"
            ([ self.nixosModules.core ] ++ moreModules) metadataOverride);
        host-nas = moreModules: metadataOverride:
          (self.lib.evalConfiguration "x86_64-linux" "nas"
            ([ self.nixosModules.core ] ++ moreModules) metadataOverride);
        host-nuc = moreModules: metadataOverride:
          (self.lib.evalConfiguration "x86_64-linux" "nuc"
            ([ self.nixosModules.core ] ++ moreModules) metadataOverride);
        host-pi4 = moreModules: metadataOverride:
          (self.lib.evalConfiguration "aarch64-linux" "pi4"
            ([ self.nixosModules.core ] ++ moreModules) metadataOverride);
        host-pi3a = moreModules: metadataOverride:
          (self.lib.evalConfiguration "aarch64-linux" "pi3a"
            ([ self.nixosModules.core ] ++ moreModules) metadataOverride);
        host-r6c = moreModules: metadataOverride:
          (self.lib.evalConfiguration "aarch64-linux" "r6c"
            ([ self.nixosModules.core ] ++ moreModules) metadataOverride);
      };

      ##########################################################################
      ## configurations ########################################################
      ##########################################################################

      nixosConfigurations = {
        p14 = self.nixosConfigurationsGen.host-p14 [ ] { };
        spare = self.nixosConfigurationsGen.host-spare [ ] { };
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

    } (let
      eachDefaultSystem =
        inputs.flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" ];
    in eachDefaultSystem (system: {
      # legacyPackages = import inputs.nixpkgs {
      #   inherit system;
      #   config = nixpkgsConfig;
      # };

      # might be overwritten in priv
      packages = {
        myconfig-iso = self.lib.mkISO {
          inherit system;
          hostName = "iso";
          nixosModules = [ self.nixosModules.core ];
          metadataOverride = { };
          bootstrappedConfig = null;
        };

        pi4-sd-image =
          inputs.self.nixosConfigurations.pi4.config.system.build.sdImage;
        pi3a-sd-image =
          inputs.self.nixosConfigurations.pi3a.config.system.build.sdImage;
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
