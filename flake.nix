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

    nixpkgs-wayland = { url = "github:nix-community/nixpkgs-wayland"; };
    # only needed if you use as a package set:
    nixpkgs-wayland.inputs.nixpkgs.follows = "nixpkgs";
    # nixpkgs-wayland.inputs.master.follows = "master";

    home.url = "github:nix-community/home-manager";
    home.inputs.nixpkgs.follows = "nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";

    # nix.url = "github:nixos/nix/flakes";
    # nix.inputs.nixpkgs.follows = "nixpkgs";

    nur.url = "github:nix-community/NUR";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    # vulnerablecode.url = "github:nexB/vulnerablecode?dir=etc/nix";

    emacs.url = "github:nix-community/emacs-overlay";
    nix-doom-emacs.url = "github:nix-community/nix-doom-emacs";
    nix-doom-emacs.inputs.nixpkgs.follows = "nixpkgs";

    my-wallpapers.url = "github:maxhbr/wallpapers";
    my-wallpapers.inputs.nixpkgs.follows = "nixpkgs";

    # octrc.url = "github:maxhbr/octrc";
    # octrc.inputs.nixpkgs.follows = "nixpkgs";

    # zephyrproject.url = "path:flakes/zephyrproject/";

    # #wayland:sway
    # swaymonad = {
    #   url = "github:nicolasavru/swaymonad";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };

    # #wayland:river
    # river-grid = {
    #   url = "github:luc65r/grid";
    #   inputs.nixpkgs.follows = "nixpkgs";
    #   inputs.flake-utils.follows = "flake-utils";
    # };

    # #wayland:hyprland
    # hyprland = {
    #   url = "github:hyprwm/Hyprland";
    #   # build with your own instance of nixpkgs
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };

    # #wayland:newm
    # newmpkg = {
    #   url = "github:jbuchermn/newm";
    #   inputs.nixpkgs.follows = "nixpkgs";
    #   inputs.flake-utils.follows = "flake-utils";
    # };
    # pywm-fullscreenpkg = {
    #   url = "github:jbuchermn/pywm-fullscreen";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };

    # #wayland:vivarium
    # vivarium.url = "github:maxhbr/vivarium";

    mydwl.url = "github:maxhbr/mydwl";
    mydwl.inputs.nixpkgs.follows = "nixpkgs";

    ###########################################################################
    # begin fish
    fasd.url = "github:oh-my-fish/plugin-fasd";
    fasd.flake = false;
    foreign-env.url = "github:oh-my-fish/plugin-foreign-env";
    foreign-env.flake = false;
    tmux.url = "github:oh-my-fish/plugin-tmux";
    tmux.flake = false;
    z.url = "github:jethrokuan/z";
    z.flake = false;
    fzf.url = "github:jethrokuan/fzf";
    fzf.flake = false;
    done.url = "github:franciscolourenco/done";
    done.flake = false;
    fish-async-prompt.url = "github:acomagu/fish-async-prompt";
    fish-async-prompt.flake = false;
    fish-ssh-agent.url = "github:danhper/fish-ssh-agent";
    fish-ssh-agent.flake = false;
    # sashimi.url = "github:isacikgoz/sashimi";
    # sashimi.flake = false;
    agnoster.url = "github:hauleth/agnoster";
    agnoster.flake = false;
    bax.url = "github:jorgebucaran/bax.fish";
    bax.flake = false;
    # end fish
    ###########################################################################
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      inherit (inputs.nixpkgs) lib;
      nixpkgsConfig = {
        allowUnfree = true;
        allowUnfreePredicate = (pkg: true);
        segger-jlink.acceptLicense = true;
        allowBroken = true;
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
              backupFileExtension = "hmBackup";
              sharedModules = [{ home.stateVersion = lib.mkDefault "22.11"; }];
            };
          };
        };
        readOnlyPkgs = {
          imports = [ nixpkgs.nixosModules.readOnlyPkgs ];
          nixpkgs.pkgs = nixpkgs.legacyPackages.x86_64-linux;
        };
        fishPluginsModule = { pkgs, ... }: {
          config = {
            home-manager.sharedModules = [
              ({ config, lib, ... }: {
                config = lib.mkIf config.programs.fish.enable {
                  home.packages = with pkgs; [ fasd fzf ];
                  programs.fish = {
                    plugins = [
                      {
                        name = "fasd";
                        src = inputs.fasd;
                      }
                      {
                        name = "foreign-env";
                        src = inputs.foreign-env;
                      }
                      {
                        name = "tmux";
                        src = inputs.tmux;
                      }
                      {
                        name = "z";
                        src = inputs.z;
                      }
                      {
                        name = "fzf";
                        src = inputs.fzf;
                      }
                      {
                        name = "done";
                        src = inputs.done;
                      }
                      {
                        name = "fish-async-prompt";
                        src = inputs.fish-async-prompt;
                      }
                      {
                        name = "fish-ssh-agent";
                        src = inputs.fish-ssh-agent;
                      }
                    ];
                  };
                  home.file = {
                    ".config/fish/functions/fish_prompt.fish".source =
                      inputs.agnoster + "/fish_prompt.fish";
                    ".config/fish/functions/bax.fish".source = inputs.bax
                      + "/bax.fish";
                  };
                };
              })
            ];
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
                  (mkSubPkgsOverlay "nixos-2009" inputs.rel2009)
                  (mkSubPkgsOverlay "nixos-2105" inputs.rel2105)
                  (mkSubPkgsOverlay "nixos-2111" inputs.rel2111)
                  (mkSubPkgsOverlay "nixos-2205" inputs.rel2205)
                  (mkSubPkgsOverlay "nixos-2211" inputs.rel2111)
                  (mkSubPkgsOverlay "nixos-2305" inputs.rel2205)
                ];
              };
            })
            ({ pkgs, ... }: { nixpkgs.overlays = [ inputs.emacs.overlay ]; })
            ({ pkgs, ... }: {
              nixpkgs.overlays = [
                (_: _: {
                  # https://github.com/NixOS/nixpkgs/pull/145738
                  tree = (import inputs.master {
                    inherit (pkgs) config system;
                  }).tree;
                  # https://github.com/NixOS/nixpkgs/pull/159074
                  remarshal = (import inputs.master {
                    inherit (pkgs) config system;
                  }).remarshal;
                })
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
            # ({ pkgs, ... }: {
            #   nixpkgs.overlays = [
            #     (_: _: {
            #       newm = inputs.newmpkg.packages.${pkgs.system}.newm;
            #       pywm-fullscreen =
            #         inputs.pywm-fullscreenpkg.packages.${pkgs.system}.pywm-fullscreen;
            #     })
            #   ];
            # })
            # ({ pkgs, ... }: {
            #   nixpkgs.overlays = [
            #     (_: _: {
            #       swaymonad = inputs.swaymonad.defaultpackage.${pkgs.system};
            #     })
            #   ];
            # })
            # ({ pkgs, ... }: {
            #   nixpkgs.overlays = [
            #     (_: _: {
            #       river-grid = inputs.river-grid.defaultPackage.${pkgs.system};
            #     })
            #   ];
            # })
            # ({ pkgs, ... }: { nixpkgs.overlays = [ inputs.vivarium.overlay ]; })
            inputs.my-wallpapers.nixosModule
            fishPluginsModule

            ({ pkgs, ... }: {
              home-manager.sharedModules = [ inputs.nix-doom-emacs.hmModule ];
            })

            ({ pkgs, config, ... }: {
              config = {
                nix.settings = {
                  trusted-public-keys = [
                    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
                    "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
                  ];
                  substituters = [
                    "https://cache.nixos.org"
                    "https://nixpkgs-wayland.cachix.org"
                  ];
                };
                nixpkgs.overlays = [ inputs.nixpkgs-wayland.overlay ];
              };
            })
          ] ++ (map (n: "${./modules}/${n}")
            (builtins.attrNames (builtins.readDir ./modules)));
          config = {
            hardware.enableRedistributableFirmware = true;
            nixpkgs.overlays = [ inputs.nur.overlay ];
          };
        };
        mydwl = import ./flake.nixosModules.mydwl.nix inputs;
      };

      nixosConfigurationsGen = {
        host-p14 = moreModules: metadataOverride:
          (self.lib.evalConfiguration "x86_64-linux" "p14" ([
            self.nixosModules.core
            self.nixosModules.mydwl
            inputs.nixos-hardware.nixosModules.common-cpu-intel
            inputs.nixos-hardware.nixosModules.common-gpu-intel
            inputs.nixos-hardware.nixosModules.common-pc-laptop
            inputs.nixos-hardware.nixosModules.common-pc-laptop-acpi_call
            inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
            inputs.nixos-hardware.nixosModules.lenovo-thinkpad
            ({ myconfig, ... }: {
              imports = [
                (myconfig.metadatalib.announceHost "x1extremeG2")
                (myconfig.metadatalib.announceHost "workstation")
                (myconfig.metadatalib.announceHost "nas")
                (myconfig.metadatalib.announceHost "vserver")
                (myconfig.metadatalib.announceHost "nuc")
                (myconfig.metadatalib.announceHost "pi4")
                (myconfig.metadatalib.announceHost "pi3a")
                (myconfig.metadatalib.announceHost "pi0")
              ];
            })
          ] ++ moreModules) metadataOverride);
        host-x1extremeG2 = moreModules: metadataOverride:
          (self.lib.evalConfiguration "x86_64-linux" "x1extremeG2" ([
            { config = { hardware.enableRedistributableFirmware = true; }; }
            self.nixosModules.core
            # inputs.octrc.nixosModule
            inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-extreme-gen2
            # inputs.zephyrproject.nixosModule
          ] ++ moreModules) metadataOverride);
        host-workstation = moreModules: metadataOverride:
          (self.lib.evalConfiguration "x86_64-linux" "workstation" ([
            self.nixosModules.core
            # inputs.octrc.nixosModule
          ] ++ moreModules) metadataOverride);
        host-spare = moreModules: metadataOverride:
          (self.lib.evalConfiguration "x86_64-linux" "spare" ([
            self.nixosModules.core
            # inputs.octrc.nixosModule
          ] ++ moreModules) metadataOverride);
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
          (self.lib.evalConfiguration "aarch64-linux" "pi4" ([
            self.nixosModules.core
            inputs.nixos-hardware.nixosModules.raspberry-pi-4
          ] ++ moreModules) metadataOverride);
        host-pi3a = moreModules: metadataOverride:
          (self.lib.evalConfiguration "aarch64-linux" "pi3a"
            ([ self.nixosModules.core ] ++ moreModules) metadataOverride);
      };

      ##########################################################################
      ## configurations ########################################################
      ##########################################################################

      nixosConfigurations = {
        p14 = self.nixosConfigurationsGen.host-p14 [ ] { };
        x1extremeG2 = self.nixosConfigurationsGen.host-x1extremeG2 [ ] { };
        workstation = self.nixosConfigurationsGen.host-workstation [ ] { };
        spare = self.nixosConfigurationsGen.host-spare [ ] { };
        vserver = self.nixosConfigurationsGen.host-vserver [ ] { };
        nas = self.nixosConfigurationsGen.host-nas [ ] { };
        nuc = self.nixosConfigurationsGen.host-nuc [ ] { };
        pi4 = self.nixosConfigurationsGen.host-pi4 [ ] { };
        pi3a = self.nixosConfigurationsGen.host-pi3a [ ] { };

        container = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            self.nixosModules.activateHomeManager
            self.nixosModules.readOnlyPkgs
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
    in eachDefaultSystem (system: {
      legacyPackages = import inputs.nixpkgs {
        inherit system;
        config = nixpkgsConfig;
      };

      # might be overwritten in priv
      packages.myconfig-iso = self.lib.mkISO {
        inherit system;
        hostName = "iso";
        nixosModules = [ self.nixosModules.core ];
        metadataOverride = { };
        bootstrappedConfig = null;
      };

      packages.pi4-sd-image =
        inputs.self.nixosConfigurations.pi4.config.system.build.sdImage;
      packages.pi3a-sd-image =
        inputs.self.nixosConfigurations.pi3a.config.system.build.sdImage;

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
