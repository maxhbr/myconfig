# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.llama-cpp — declaratively expose a set of GGUF models as
# per-(model, device) llama-server / llama-bench wrappers and as
# llama-swap entries and/or per-device llama-server router scripts.
#
# This file is just glue: it imports the option declarations and the
# available backends. The actual logic lives in:
#
#   options.nix          — option declarations (backend-agnostic)
#   lib/                 — pure helpers (devices, variants, script builders,
#                          router INI rendering)
#   services.llama-cpp.nix — picks the right llama-cpp package for the host
#                            (GPU variant) and wires it into the nixpkgs
#                            `services.llama-cpp` module
#   llama-swap.nix       — llama-swap integration
#   router.nix           — per-device llama-server router (one INI-driven
#                          llama-server_<Device> wrapper per device)
#   myconfig.ai.pull_models.nix
#                        — bridge that auto-collects each model's
#                          `pull-models = { target_directory; hf_spec; }`
#                          into `myconfig.ai.pull_models.models`
#
# The router and llama-swap backends consume the same `models` option
# and can be enabled independently.
{ ... }:
{
  imports = [
    ./options.nix
    ./services.llama-cpp.nix
    ./llama-swap.nix
    ./router.nix
    ./myconfig.ai.pull_models.nix
  ];
}
