# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# NInfer container variants for host.thing. Kept in its own directory
# (rather than under ../myconfig.ai.vllm/) because NInfer is a distinct
# inference engine from vLLM, even though both currently share the same
# GPU, host port, and llama-swap unit on this host.
#
# The llama-swap unit's Podman and NVIDIA CDI generator start
# dependencies (Wants=/After=) are declared once in
# ../myconfig.ai.vllm/default.nix, which this host always imports
# alongside this module (both engine families target the same NVIDIA
# RTX 5090 via the same llama-swap instance). They are intentionally
# *not* repeated here so the generated systemd unit does not carry
# duplicate entries. If NInfer is ever used on a host without the vLLM
# module, declare the equivalent `systemd.services.llama-swap.{wants,
# after}` there.
{
  imports = [
    ./docker.ninfer.cuda.nix
  ];
}
