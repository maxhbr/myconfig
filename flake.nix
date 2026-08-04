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

    hermes-agent.url = "github:NousResearch/hermes-agent/v2026.5.7";
    hermes-agent.inputs.nixpkgs.follows = "nixpkgs";
    microvm.url = "github:astro/microvm.nix";
    microvm.inputs.nixpkgs.follows = "nixpkgs";

    # Agent skills sources (consumed by vendor/agent-skills-nix)
    mattpocock-skills = {
      url = "github:mattpocock/skills";
      flake = false;
    };

    NanoKVM-USB.url = "github:maxhbr/NanoKVM-USB";
    NanoKVM-USB.inputs.nixpkgs.follows = "nixpkgs";

    niri.url = "github:niri-wm/niri";

    workmux.url = "github:raine/workmux";
    workmux.inputs.nixpkgs.follows = "nixpkgs";

    # PR overrides (see flake.pkgs_from_prs.nix)
    pr531581.url = "github:NixOS/nixpkgs/pull/531581/head"; # gimp: revert __structuredAttrs (crash fix)
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
        lib = {
          jail = import ./vendor/alexdavid-jail.nix/lib;
        }
        // import ./flake.lib.nix inputs
        // import ./flake.sandboxed-pi.nix inputs;

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
                (import ./flake.pkgs_from_prs.nix { inherit inputs; })
                (import ./flake.pkgs_overrides.nix { })
                (
                  { pkgs, ... }:
                  {
                    nixpkgs.overlays = [
                      (_: _: {
                        mybackup = pkgs.callPackage ../pkgs/mybackup { inherit pkgs; };
                        playwright-cli = pkgs.callPackage ./pkgs/playwright-cli { };
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
                        "myconfig/maxhbr-NanoKVM-USB" = {
                          checkout = "git clone https://github.com/maxhbr/NanoKVM-USB";
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
                (
                  { pkgs, myconfig, ... }:
                  {
                    imports = [ (myconfig.metadatalib.announceOtherHosts "p14") ];
                  }
                )
                {
                  myconfig.upg.enable = true;
                }
              ]
              ++ moreModules
            ) metadataOverride);
          host-workstation =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "x86_64-linux" "workstation" (
              [
                self.nixosModules.core
                (
                  { pkgs, myconfig, ... }:
                  {
                    imports = [ (myconfig.metadatalib.announceOtherHosts "workstation") ];
                  }
                )
              ]
              ++ moreModules
            ) metadataOverride);
          host-vserver =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "x86_64-linux" "vserver" (
              [
                self.nixosModules.core
                (
                  { pkgs, myconfig, ... }:
                  {
                    imports = [ (myconfig.metadatalib.announceOtherHosts "vserver") ];
                  }
                )
              ]
              ++ moreModules
            ) metadataOverride);
          host-nas =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "x86_64-linux" "nas" (
              [
                self.nixosModules.core
                (
                  { pkgs, myconfig, ... }:
                  {
                    imports = [ (myconfig.metadatalib.announceOtherHosts "nas") ];
                  }
                )
              ]
              ++ moreModules
            ) metadataOverride);
          host-nuc =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "x86_64-linux" "nuc" (
              [
                self.nixosModules.core
                (
                  { pkgs, myconfig, ... }:
                  {
                    imports = [ (myconfig.metadatalib.announceOtherHosts "nuc") ];
                  }
                )
              ]
              ++ moreModules
            ) metadataOverride);
          # host-pi4 =
          #   moreModules: metadataOverride:
          #   (self.lib.evalConfiguration "aarch64-linux" "pi4" (
          #     [
          #       self.nixosModules.core
          #       (
          #         { pkgs, myconfig, ... }:
          #         {
          #           imports = [ (myconfig.metadatalib.announceOtherHosts "pi4") ];
          #         }
          #       )
          #     ]
          #     ++ moreModules
          #   ) metadataOverride);
          # host-pi3a =
          #   moreModules: metadataOverride:
          #   (self.lib.evalConfiguration "aarch64-linux" "pi3a" (
          #     [
          #       self.nixosModules.core
          #       (
          #         { pkgs, myconfig, ... }:
          #         {
          #           imports = [ (myconfig.metadatalib.announceOtherHosts "pi3a") ];
          #         }
          #       )
          #     ]
          #     ++ moreModules
          #   ) metadataOverride);
          host-odroid =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "aarch64-linux" "odroid" (
              [
                self.nixosModules.core
                (
                  { pkgs, myconfig, ... }:
                  {
                    imports = [ (myconfig.metadatalib.announceOtherHosts "odroid") ];
                  }
                )
              ]
              ++ moreModules
            ) metadataOverride);
          host-r6c =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "aarch64-linux" "r6c" (
              [
                self.nixosModules.core
                (
                  { pkgs, myconfig, ... }:
                  {
                    imports = [ (myconfig.metadatalib.announceOtherHosts "r6c") ];
                  }
                )
              ]
              ++ moreModules
            ) metadataOverride);
          host-roc =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "aarch64-linux" "roc" (
              [
                self.nixosModules.core
                (
                  { pkgs, myconfig, ... }:
                  {
                    imports = [ (myconfig.metadatalib.announceOtherHosts "roc") ];
                  }
                )
              ]
              ++ moreModules
            ) metadataOverride);
          host-t6 =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "aarch64-linux" "t6" (
              [
                self.nixosModules.core
                (
                  { pkgs, myconfig, ... }:
                  {
                    imports = [ (myconfig.metadatalib.announceOtherHosts "t6") ];
                  }
                )
              ]
              ++ moreModules
            ) metadataOverride);
          host-futro =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "x86_64-linux" "futro" (
              [
                self.nixosModules.core
                (
                  { pkgs, myconfig, ... }:
                  {
                    imports = [ (myconfig.metadatalib.announceOtherHosts "futro") ];
                  }
                )
              ]
              ++ moreModules
            ) metadataOverride);
          host-thing =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "x86_64-linux" "thing" (
              [
                self.nixosModules.core
                (
                  { pkgs, myconfig, ... }:
                  {
                    imports = [ (myconfig.metadatalib.announceOtherHosts "thing") ];
                  }
                )
                { myconfig.upg.enable = true; }
              ]
              ++ moreModules
            ) metadataOverride);
        };

        # ISO builder that already includes the myconfig base modules
        # (`nixosModules.core`) and all installer logic from this repo.
        # Intended to be called from a priv flake, which only needs to pass
        # its own extra `moreModules`.
        mkMyconfigISO =
          {
            system ? "x86_64-linux",
            hostName ? "iso",
            metadataOverride ? { },
            bootstrappedConfig ? null,
          }:
          moreModules:
          self.lib.mkISO {
            inherit
              system
              hostName
              metadataOverride
              bootstrappedConfig
              ;
            nixosModules = [ self.nixosModules.core ] ++ moreModules;
          };

        ##########################################################################
        ## configurations ########################################################
        ##########################################################################

        nixosConfigurations = {
          test-f13 = self.nixosConfigurationsGen.host-f13 [
            { myconfig.secretsWarnOnMissingSource = false; }
          ] { };
          test-p14 = self.nixosConfigurationsGen.host-p14 [
            { myconfig.secretsWarnOnMissingSource = false; }
          ] { };
          test-workstation = self.nixosConfigurationsGen.host-workstation [
            { myconfig.secretsWarnOnMissingSource = false; }
          ] { };
          test-vserver = self.nixosConfigurationsGen.host-vserver [
            { myconfig.secretsWarnOnMissingSource = false; }
          ] { };
          test-nas = self.nixosConfigurationsGen.host-nas [
            { myconfig.secretsWarnOnMissingSource = false; }
          ] { };
          test-nuc = self.nixosConfigurationsGen.host-nuc [
            { myconfig.secretsWarnOnMissingSource = false; }
          ] { };
          test-r6c = self.nixosConfigurationsGen.host-r6c [
            { myconfig.secretsWarnOnMissingSource = false; }
          ] { };
          test-odroid = self.nixosConfigurationsGen.host-odroid [
            { myconfig.secretsWarnOnMissingSource = false; }
          ] { };
          test-roc = self.nixosConfigurationsGen.host-roc [
            { myconfig.secretsWarnOnMissingSource = false; }
          ] { };
          test-t6 = self.nixosConfigurationsGen.host-t6 [
            { myconfig.secretsWarnOnMissingSource = false; }
          ] { };
          # test-pi4 = self.nixosConfigurationsGen.host-pi4 [
          #   { myconfig.secretsWarnOnMissingSource = false; }
          # ] { };
          # test-pi3a = self.nixosConfigurationsGen.host-pi3a [
          #   { myconfig.secretsWarnOnMissingSource = false; }
          # ] { };
          test-thing = self.nixosConfigurationsGen.host-thing [
            { myconfig.secretsWarnOnMissingSource = false; }
          ] { };
          test-futro = self.nixosConfigurationsGen.host-futro [
            { myconfig.secretsWarnOnMissingSource = false; }
          ] { };

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
            myconfig-iso = self.mkMyconfigISO {
              inherit system;
              hostName = "iso";
            } [ ];
          }
          // lib.optionalAttrs (system == "x86_64-linux" || system == "aarch64-linux") {
            # Per-invocation microvm runner for `sandboxed-pi`. Built impurely
            # by the host-side `sandboxed-pi` wrapper, which sets the
            # SANDBOXED_PI_* environment variables (workspace path, forwarded
            # SSH port, throwaway authorized-keys file) before
            # `nix build --impure`. The workspace path therefore never appears
            # in any tracked file or flake output. See flake.sandboxed-pi.nix
            # and modules/myconfig.ai/programs.pi-coding-agent/default.nix.
            sandboxed-pi-runner =
              let
                getEnvOr =
                  name: fallback:
                  let
                    v = builtins.getEnv name;
                  in
                  if v == "" then fallback else v;
                workspace = builtins.getEnv "SANDBOXED_PI_WORKSPACE";
              in
              if workspace == "" then
                # Pure eval (e.g. `nix flake check`, `nix flake show`): the
                # env vars are empty, so emit a harmless placeholder derivation
                # instead of failing. The real runner is only ever built
                # impurely by the wrapper.
                nixpkgs.legacyPackages.${system}.writeShellScriptBin "sandboxed-pi-runner" ''
                  echo "sandboxed-pi-runner must be built via the sandboxed-pi wrapper (SANDBOXED_PI_WORKSPACE unset)" >&2
                  exit 1
                ''
              else
                self.lib.mkSandboxedPiRunner {
                  inherit system workspace;
                  sshPort = lib.toInt (getEnvOr "SANDBOXED_PI_SSH_PORT" "2222");
                  authorizedKeysFile = getEnvOr "SANDBOXED_PI_AUTHORIZED_KEYS" "/var/empty/authorized_keys";
                  piPackage = inputs.nixos-unstable.legacyPackages.${system}.pi-coding-agent;
                  allowNetwork = getEnvOr "SANDBOXED_PI_NETWORK" "1" != "0";
                };

            # Per-invocation microvm runner for the whole workmux/tmux session
            # (`alacritty-sandboxed-workmux-here`). Built impurely by that
            # wrapper, which sets the SANDBOXED_WORKMUX_* environment variables
            # (main repo, sibling worktrees dir, forwarded SSH port, throwaway
            # authorized-keys file, generated workmux config + tmux.conf store
            # paths). See flake.sandboxed-pi.nix and
            # modules/myconfig.ai/myconfig.ai.workmux/sandbox.nix.
            sandboxed-workmux-runner =
              let
                getEnvOr =
                  name: fallback:
                  let
                    v = builtins.getEnv name;
                  in
                  if v == "" then fallback else v;
                workspace = builtins.getEnv "SANDBOXED_WORKMUX_REPO";
              in
              if workspace == "" then
                nixpkgs.legacyPackages.${system}.writeShellScriptBin "sandboxed-workmux-runner" ''
                  echo "sandboxed-workmux-runner must be built via the alacritty-sandboxed-workmux-here wrapper (SANDBOXED_WORKMUX_REPO unset)" >&2
                  exit 1
                ''
              else
                self.lib.mkSandboxedWorkmuxRunner {
                  inherit system workspace;
                  worktrees = getEnvOr "SANDBOXED_WORKMUX_WORKTREES" workspace;
                  sshPort = lib.toInt (getEnvOr "SANDBOXED_WORKMUX_SSH_PORT" "2222");
                  authorizedKeysFile = getEnvOr "SANDBOXED_WORKMUX_AUTHORIZED_KEYS" "/var/empty/authorized_keys";
                  workmuxConfigFile = getEnvOr "SANDBOXED_WORKMUX_CONFIG" "/var/empty/config.yaml";
                  tmuxConf = getEnvOr "SANDBOXED_WORKMUX_TMUXCONF" "";
                  piPackage = inputs.nixos-unstable.legacyPackages.${system}.pi-coding-agent;
                  workmuxPackage = inputs.workmux.packages.${system}.default;
                  allowNetwork = getEnvOr "SANDBOXED_WORKMUX_NETWORK" "1" != "0";
                };
          };

          formatter = nixpkgs.legacyPackages.${system}.nixfmt-tree;

          checks = {
            pre-commit-check = inputs.git-hooks.lib.${system}.run {
              src = ./.;
              hooks = {
                nixfmt = {
                  enable = true;
                  # Vendored subtrees (managed via `git subtree`) keep
                  # their upstream formatting and may contain Nix fragments
                  # that nixfmt cannot parse.  Exclude them here, mirroring
                  # the `TREEFMT_EXCLUDES="vendor/**"` policy in
                  # nixfmtall.sh so `nix flake check` and `./nixfmtall.sh`
                  # agree on what gets formatted.
                  excludes = [ "vendor/.*" ];
                };
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
                  # The EXECUTED myconfig.ai.microvm batch-job harnesses and the
                  # real-KVM validation suite. They are plain scripts (not
                  # writeShellApplication), so nothing else gates them.
                  "tests/microvm-batch-result-integrity.sh"
                  "tests/microvm-batch-controller-smoke.sh"
                  "tests/microvm-batch-launcher-submit.sh"
                  "tests/microvm-launcher-recover.sh"
                  "modules/myconfig.ai/myconfig.ai.microvm/runtime-validation.sh"
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
          }
          # Automated EVAL / BUILD test suite for the myconfig.ai.microvm
          # Cloud Hypervisor agent-sandbox tier (plan §38, wired per §39).
          # Kept in a dedicated file to avoid bloating flake.nix. Only wired
          # in for x86_64-linux, the system the enabled reference host
          # `test-f13` is built for. These are eval/build checks ONLY — NOT
          # KVM/network runtime proof (see tests/microvm.nix header).
          // lib.optionalAttrs (system == "x86_64-linux") (
            import ./tests/microvm.nix { inherit self inputs system; }
          );

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
                shfmt
                shellcheck
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
