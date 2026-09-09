# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.hermes — hermes-agent gateway integration.
#
# This file is just glue. The actual configuration lives in:
#
#   shared.nix           — shared `let` bindings (hermesServiceCfg, paths,
#                         URLs) consumed by both backends; a plain
#                         function, not a NixOS module.
#   service.nix          — host-level (native) hermes-agent service:
#                         declares the `myconfig.ai.hermes.*` options,
#                         imports the upstream `inputs.hermes-agent`
#                         NixOS module and wires up `services.hermes-agent`
#                         in native mode.
#   nixos-container.nix  — containerized and isolated hermes service via a
#                         NixOS declarative container.
#   microvm.nix          — hermes service in a microvm.nix MicroVM (real VM,
#                         separate kernel), managed as a host systemd service.
{ ... }:
{
  imports = [
    ./service.nix
    ./nixos-container.nix
    ./microvm.nix
  ];
}
