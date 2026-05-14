# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.llama-cpp — declaratively expose a set of GGUF models as
# per-(model, device) llama-server / llama-bench wrappers and (today)
# llama-swap entries.
#
# This file is just glue: it imports the option declarations and the
# llama-swap backend. The actual logic lives in:
#
#   options.nix          — option declarations (backend-agnostic)
#   lib/                 — pure helpers (devices, variants, script builders)
#   services.llama-cpp.nix — picks the right llama-cpp package for the host
#                            (GPU variant) and wires it into the nixpkgs
#                            `services.llama-cpp` module
#   llama-swap.nix       — llama-swap integration
#
# Long-term we want to support alternative backends to llama-swap; those
# would live next to ./llama-swap.nix and consume the same options.
{ ... }:
{
  imports = [
    ./options.nix
    ./services.llama-cpp.nix
    ./llama-swap.nix
  ];
}
