# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Provides a standalone Nix expression for the disposable QEMU agent runners.
# The wrappers evaluate this expression directly; these runners are not flake
# outputs because every invocation embeds transient workspace, port, and key
# paths.
{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  system = pkgs.stdenv.hostPlatform.system;
  # The workmux package as consumed everywhere else: the upstream flake
  # input's package with the check-phase tmux override (see
  # myconfig.ai.workmux/default.nix). Embeding the raw
  # `inputs.workmux.packages...default` here would reintroduce the unpatched
  # derivation into the closure (its `cargo test` run fails on remote
  # builders), so the runner must pin the same store path.
  workmuxPackage = inputs.workmux.packages.${system}.default.overrideAttrs (old: {
    nativeCheckInputs = (old.nativeCheckInputs or [ ]) ++ [ pkgs.tmux ];
  });
  runnerExpression = pkgs.writeText "qemu-agent-sandbox-runner.nix" ''
    let
      nixpkgsPath = ${inputs.nixpkgs};
      lib = import (nixpkgsPath + "/lib");
      nixpkgs = {
        inherit lib;
        legacyPackages = lib.genAttrs [ "${system}" "x86_64-linux" ] (
          targetSystem: import nixpkgsPath { system = targetSystem; }
        );
      };
      nixosSystem =
        args:
        import (nixpkgsPath + "/nixos/lib/eval-config.nix") {
          inherit (args) system modules;
        };
    in
    import ${./.}/runner.nix {
      inherit nixpkgs nixosSystem;
      system = "${system}";
      microvmModule = import ${inputs.microvm}/nixos-modules/microvm;
      seedAgentConfig = ${../../fns/seed-agent-config.nix};
      piPackage = ${inputs.nixos-unstable.legacyPackages.${system}.pi-coding-agent};
      herdrPackage = ${pkgs.herdr};
      workmuxPackage = ${workmuxPackage};
    }
  '';
in
{
  options.myconfig.ai.dev.qemu-agent-sandbox.runnerExpression = lib.mkOption {
    type = lib.types.path;
    readOnly = true;
    description = "Standalone impure Nix expression used to build disposable QEMU agent runners.";
  };

  config.myconfig.ai.dev.qemu-agent-sandbox.runnerExpression = runnerExpression;
}
