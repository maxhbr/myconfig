{
  description = "myconfig";

  inputs = {
    master.url = "github:nixos/nixpkgs?ref=master";
    nixos-unstable-small.url = "github:nixos/nixpkgs?ref=nixos-unstable-small";
    nixos-unstable.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    # sbomnix only (flake.pkgs_overrides.nix, doc/TODOs/drop-sbomnix-nixpkgs-pin.md)
    nixpkgs-sbomnix.url = "github:nixos/nixpkgs/fe13242281a67641c0102cdb1087f0ebe586c7df";

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

    flake-utils.url = "github:numtide/flake-utils";

    nur.url = "github:nix-community/NUR";
    nur.inputs.nixpkgs.follows = "nixpkgs";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    my-wallpapers.url = "github:maxhbr/wallpapers";
    my-wallpapers.inputs.nixpkgs.follows = "nixpkgs";

    zephyr-flake.url = "github:maxhbr/zephyr-flake";
    zephyr-flake.inputs.nixpkgs.follows = "nixpkgs";

    # The mysbx project as a path input: the tree is store-copied at
    # evaluation, so nothing under mysbx/ can reach back into this repo
    # by relative path — the flake outputs (lib, nixosModules, packages)
    # are the only interface.
    mysbx.url = "path:./mysbx";
    mysbx.inputs.nixpkgs.follows = "nixpkgs";

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

    # agent-of-empires builds its Rust workspace with crane against its own
    # pinned nixpkgs (nixos-unstable, plus a vendored npm frontend with a
    # pinned npmDepsHash). No `follows` here on purpose: overriding nixpkgs
    # would move the toolchain away from the revision upstream tests against
    # and can invalidate the pinned hashes.
    agent-of-empires.url = "github:agent-of-empires/agent-of-empires";

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
        // import ./flake.lib.nix inputs;

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
                          (mkSubPkgsOverlay "nixpkgs-sbomnix" inputs.nixpkgs-sbomnix)
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
                        thinkrail = pkgs.callPackage ./pkgs/thinkrail { };
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
          host-optiplex =
            moreModules: metadataOverride:
            (self.lib.evalConfiguration "x86_64-linux" "optiplex" (
              [
                self.nixosModules.core
                (
                  { pkgs, myconfig, ... }:
                  {
                    imports = [ (myconfig.metadatalib.announceOtherHosts "optiplex") ];
                  }
                )
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
          test-optiplex = self.nixosConfigurationsGen.host-optiplex [
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

          # Central list of directories that no formatter may touch.
          formatterExcludeDirs = [ "vendor" ];
          formatterExcludeGlobs = map (d: "${d}/**") formatterExcludeDirs; # treefmt
          formatterExcludeRegexes = map (d: "^${d}/") formatterExcludeDirs; # pre-commit
        in
        eachDefaultSystem (system: {
          # might be overwritten in priv
          packages.myconfig-iso = self.mkMyconfigISO {
            inherit system;
            hostName = "iso";
          } [ ];

          # Live kanban board over the bd (beads) issue DB.
          packages.beads-server = nixpkgs.legacyPackages.${system}.callPackage ./misc/beads-server { };
          apps.beads-server = inputs.flake-utils.lib.mkApp { drv = self.packages.${system}.beads-server; };

          # `nix fmt` — nixfmt for Nix, rustfmt for the
          # hand-written Rust crate in `mysbx/mysbx-rs`.
          # `nixfmt-tree`
          # is a treefmt wrapper that ships the nixfmt entry; the override
          # adds the formatter binary and its treefmt entry.
          #
          # `rustfmt` is invoked directly instead of `cargo fmt`: treefmt
          # hands a formatter the FILES it should format, and the crate is
          # not a member of a root workspace (it is its own Cargo project,
          # built by its own `nix/*.nix`), so there is no manifest
          # `cargo fmt` could be pointed at. `--edition` must then be given
          # explicitly — without a manifest rustfmt falls back to its own
          # default edition and misparses edition-specific syntax. The
          # crate is `edition = "2021"`; a crate on another edition needs
          # its own entry (or a `rustfmt.toml` next to it), not a change of
          # this one.
          formatter = nixpkgs.legacyPackages.${system}.nixfmt-tree.override {
            runtimeInputs = [ nixpkgs.legacyPackages.${system}.rustfmt ];
            settings = {
              excludes = formatterExcludeGlobs;
              formatter.rustfmt = {
                command = "rustfmt";
                options = [
                  "--edition"
                  "2021"
                ];
                includes = [ "*.rs" ];
              };
            };
          };

          checks = {
            pre-commit-check = inputs.git-hooks.lib.${system}.run {
              src = ./.;
              hooks = {
                nixfmt = {
                  enable = true;
                  excludes = formatterExcludeRegexes;
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
                  # Home-manager activation harness (runs a host's real
                  # activation against a throwaway HOME) plus the
                  # agent-skills integration check driven by it.
                  "tests/test_home-manager_activation.sh"
                  "tests/agent-skills-activation.sh"
                  # Qwen3.8-27B gfx1151 benchmark + tool-call fixtures (run by a
                  # human on the gfx1151 hardware).
                  "tests/qwen38-toolcall-fixture.sh"
                  "tests/qwen38-benchmark-comparison.sh"
                  # Standalone script embedded into a writeShellApplication
                  # wrapper (`git-branch-to-worktree`); shfmt is not covered
                  # by writeShellApplication's own shellcheck pass.
                  "modules/shell.git/bin/git-branch-to-worktree.sh"
                  # litellm smoke-test harness: standalone script embedded into a
                  # writeShellApplication wrapper (`litellm-smoke-test`); shfmt is
                  # not covered by writeShellApplication's own shellcheck pass.
                  "modules/myconfig.ai.llmops/bin/litellm-smoke-test.sh"
                  # The direct-libkrun spike runbook (bd myconfig-dak.1): a
                  # plain script executed by a human on a KVM host (f13).
                  "mysbx/nix/krun-direct-spike.sh"
                  # The krun live-validation suite (podman-krun + the
                  # direct backend, docs/krun-live-validation.md): same
                  # category — a plain script executed by a human on a
                  # KVM host.
                  "mysbx/nix/krun-live-validation.sh"
                  # The microvm-qemu live-validation suite (bd
                  # myconfig-6x9c/26z4, backends.md D9): same category —
                  # a plain script executed by a human on a KVM host.
                  "mysbx/nix/microvm-qemu-live-validation.sh"
                  # The microvm-ch spike runbook and live-validation suite
                  # (bd myconfig-9wkp, backends.md D11): same category.
                  "mysbx/nix/cloud-hypervisor-direct-spike.sh"
                  "mysbx/nix/microvm-ch-live-validation.sh"
                  # The phase-B egress broker runbook (bd myconfig-n8kv.1.6,
                  # docs/design/egress.md): same category — a plain script
                  # executed by a human on f13, with the host's LiteLLM up.
                  "mysbx/nix/egress-broker-live-validation.sh"
                  # The wedged-krun-sandbox triage collector (bd
                  # myconfig-dj4 §6.1 of docs/krun-live-validation.md):
                  # same category — a plain read-only /proc inspection
                  # script executed by a human on the host of a wedged
                  # guest.
                  "mysbx/nix/krun-inspect-and-collect.sh"
                  # Standalone script embedded into a writeShellApplication
                  # wrapper (`niri-run-or-raise`); shfmt is not covered by
                  # writeShellApplication's own shellcheck pass.
                  "modules/myconfig.desktop.wayland.niri/bin/niri-run-or-raise.sh"
                  # Standalone script embedded into a writeShellApplication
                  # wrapper (`compute-commit-attribution-trailer.sh`); shfmt is
                  # not covered by writeShellApplication's own shellcheck pass.
                  "modules/myconfig.ai.dev/skills/commit/compute-commit-attribution-trailer.sh"
                  # The beads-server bd shim: a plain script executed by the
                  # unittest suites (bd myconfig-y294).
                  "misc/beads-server/tests/fake-bd.sh"
                  # fail2ban textfile exporter: installed as a bare script and
                  # run by systemd, so neither writeShellApplication's shellcheck
                  # pass nor any build step covers it. Its regression test is
                  # wired as the `fail2ban-textfile-test` check below.
                  "modules/myconfig.observability/scripts/fail2ban-textfile.sh"
                  "tests/fail2ban-textfile.sh"
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

            # Regression test for the fail2ban textfile exporter
            # (modules/myconfig.observability/scripts/fail2ban-textfile.sh).
            # The exporter is a bare script run by systemd, so nothing in a
            # normal build exercises it; the suite drives it through a stubbed
            # fail2ban-client fixture (multi-jail, malformed, empty, and
            # status/jail-failure scenarios) and asserts both the rendered
            # Prometheus text and the `fail2ban_up 0` degradation path.
            fail2ban-textfile-test =
              let
                pkgs = inputs.nixpkgs.legacyPackages."${system}";
              in
              pkgs.stdenv.mkDerivation {
                name = "fail2ban-textfile-test";
                src = ./.;
                doCheck = true;
                nativeBuildInputs = with pkgs; [
                  bash
                  diffutils
                  gnugrep
                  gnused
                  coreutils
                  findutils
                ];
                checkPhase = ''
                  patchShebangs tests/fail2ban-textfile.sh
                  bash tests/fail2ban-textfile.sh
                '';
                installPhase = ''
                  mkdir "$out"
                '';
              };

            # beads-server unittest suites (bd myconfig-y294): the Python
            # stdlib suites run against tests/fake-bd.sh, so they need
            # neither Dolt nor network access.
            beads-server-tests =
              let
                pkgs = inputs.nixpkgs.legacyPackages."${system}";
              in
              pkgs.stdenv.mkDerivation {
                name = "beads-server-tests";
                src = ./misc/beads-server;
                doCheck = true;
                nativeBuildInputs = [
                  pkgs.python3
                  pkgs.jq.bin
                  pkgs.bash.out
                  pkgs.nodejs
                ];
                checkPhase = ''
                  patchShebangs tests/fake-bd.sh
                  python3 tests/test_bdcache.py
                  python3 tests/test_http.py
                  node --test tests/test_*.mjs
                '';
                installPhase = ''
                  mkdir "$out"
                '';
              };
          }
          // lib.optionalAttrs (system == "x86_64-linux") (
            # Rust mysbx cargo test suite — the checks file lives in the
            # mysbx tree, reached through the mysbx flake input (no
            # relative-path bypass). The scenarios inside evaluate
            # PARENT hosts (self.nixosConfigurations.test-f13/test-p14)
            # and import parent modules as fixtures, so they stay a
            # parent-flake concern.
            import (inputs.mysbx + "/nix/checks.nix") {
              inherit self inputs system;
              # The myconfig module fixtures the integration scenarios
              # inside evaluate — the mysbx tree cannot reach them by
              # relative path (store-copied flake input), so the parent
              # passes the paths in.
              fixtures = {
                piSandboxSubtrees = ./modules/myconfig.ai.dev/programs/pi-sandbox-subtrees.nix;
                piMcpServers = ./modules/myconfig.ai.dev/programs/pi-mcp-servers.nix;
                sandboxToolsModule = ./modules/myconfig.ai.dev/myconfig.ai.dev.sandboxTools.nix;
                litellmForwarderModule = ./modules/myconfig.ai.dev/myconfig.ai.dev.litellm-forwarder.nix;
                orcaModule = ./modules/myconfig.ai.dev/services.orca.nix;
              };
            }
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
                # The formatter of the Rust crates in `modules/` and
                # `mysbx/`, so `rustfmt`/`cargo fmt` in the dev shell matches what
                # `nix fmt` applies (see `formatter` above).
                rustfmt
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
