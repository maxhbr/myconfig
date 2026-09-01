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
      seedAgentConfig = ${../fns/seed-agent-config.nix};
      piPackage = ${inputs.nixos-unstable.legacyPackages.${system}.pi-coding-agent};
      herdrPackage = ${pkgs.herdr};
      workmuxPackage = ${inputs.workmux.packages.${system}.default};
    }
  '';
in
{
  options.myconfig.ai.qemu-agent-sandbox.runnerExpression = lib.mkOption {
    type = lib.types.path;
    readOnly = true;
    description = "Standalone impure Nix expression used to build disposable QEMU agent runners.";
  };

  config.myconfig.ai.qemu-agent-sandbox.runnerExpression = runnerExpression;
}
