# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# NInfer container variants for host.thing. Kept in its own directory
# (rather than under myconfig.ai.vllm/) since NInfer is a distinct engine
# from vLLM, even though both currently share the same GPU/port/llama-swap
# unit on this host.
#
# The llama-swap unit's Podman and NVIDIA CDI generator start dependencies
# are already declared by ../myconfig.ai.vllm/default.nix and
# ../myconfig.ai.vllm/docker.vllm.cuda.nix, which this host always imports
# alongside this module (both feature directories target the same NVIDIA
# RTX 5090 via the same llama-swap instance). They are intentionally *not*
# repeated here to avoid duplicate Wants=/After= entries on the generated
# systemd unit. If NInfer is ever used on a host without the vLLM CUDA
# module, add the equivalent `virtualisation.podman.enable` /
# `hardware.nvidia-container-toolkit.enable` / llama-swap
# wants/after wiring here.
{
  imports = [
    ./docker.ninfer.cuda.nix
  ];
}
