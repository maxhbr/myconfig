# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The mysbx project's own flake. Consumed by the myconfig flake as a
# `path:` input (`mysbx.url = "path:./mysbx"`), so the project is
# store-copied at evaluation and nothing under this tree can reach
# back into myconfig by relative path — the flake outputs are the
# only interface:
#
#   lib.sandboxConfigLib  the `mkSandboxConfig` helper (nix/sandbox-config.nix)
#                         that tool modules use to mount their
#                         home-manager-deployed configuration into a
#                         sandbox.
#   nixosModules.mysbx    the NixOS module (default.nix, podman.nix, mcp.nix).
#                         It defines `myconfig.ai.dev.mysbx.*` and reads the
#                         umbrella options around it — a host that imports it
#                         is expected to carry the myconfig module set too.
#   packages.mysbx        the wrapped CLI (nix/mysbx.nix) on its own, for
#                         `nix build ./mysbx#mysbx` standalone iteration.
{
  description = "mysbx — the sandboxing CLI of myconfig";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      eachSystem = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      lib.sandboxConfigLib = import ./nix/sandbox-config.nix;

      nixosModules.mysbx = import ./default.nix;

      packages = eachSystem (pkgs: rec {
        mysbx = pkgs.callPackage ./nix/mysbx.nix { };
        default = mysbx;
      });

      devShells = eachSystem (pkgs: {
        default = pkgs.mkShell {
          packages = with pkgs; [
            cargo
            rustc
            rustfmt
            clippy
            rust-analyzer
          ];
          RUST_SRC_PATH = pkgs.rustPlatform.rustLibSrc;
        };
      });
    };
}
