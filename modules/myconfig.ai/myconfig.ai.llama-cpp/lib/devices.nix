# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Device helpers shared across the llama-cpp module.
#
# A "device" here is a string accepted by llama-cpp's --device / -dev flag,
# e.g. "Vulkan0", "Vulkan1", "ROCm0", "CUDA0".
#
# `mkGuardDevice` returns a predicate that decides whether the current
# host actually has the hardware backing a given device string. It uses
# `config.myconfig.hardware.gpu.variant` when available; otherwise it
# treats every device as available (useful for containers / NixOS
# checks where the hardware module isn't imported).
{ lib, pkgs }:
let
  # CUDA-enabled variant of llama-cpp (plain pkgs.llama-cpp has no CUDA backend)
  llama-cpp-cuda = pkgs.llama-cpp.override { cudaSupport = true; };
in
{
  # `hasGpuVariant` is parameterised on a module's `config` + `options`
  # because the `myconfig.hardware.gpu.variant` option might not be
  # imported on every evaluation context (containers etc.). When the
  # option is absent we conservatively assume the variant is present.
  mkHasGpuVariant =
    { config, options }:
    variant:
    let
      hasMyconfigProperty = lib.hasAttrByPath [ "myconfig" "hardware" "gpu" "variant" ] options;
      isPresent = builtins.elem variant config.myconfig.hardware.gpu.variant;
    in
    if hasMyconfigProperty then isPresent else true;

  # Build the `guardDevice` predicate from a `hasGpuVariant` callable.
  mkGuardDevice =
    hasGpuVariant: device:
    if lib.hasPrefix "Vulkan" device then
      (hasGpuVariant "amd" || hasGpuVariant "amd-no-rocm")
    else if lib.hasPrefix "ROCm" device then
      (hasGpuVariant "amd")
    else if lib.hasPrefix "diffusionCUDA" device || lib.hasPrefix "CUDA" device then
      (hasGpuVariant "nvidia")
    else
      false;

  # Pick the right llama.cpp build for a device string.
  llamaServerFor =
    device:
    if lib.hasPrefix "Vulkan" device then
      lib.getExe' pkgs.llama-cpp-vulkan "llama-server"
    else if lib.hasPrefix "ROCm" device then
      lib.getExe' pkgs.llama-cpp-rocm "llama-server"
    else
      lib.getExe' llama-cpp-cuda "llama-server";

  llamaBenchFor =
    device:
    if lib.hasPrefix "Vulkan" device then
      lib.getExe' pkgs.llama-cpp-vulkan "llama-bench"
    else if lib.hasPrefix "ROCm" device then
      lib.getExe' pkgs.llama-cpp-rocm "llama-bench"
    else
      lib.getExe' llama-cpp-cuda "llama-bench";

  # Diffusion-gemma devices ("diffusionCUDA0", …) use the patched
  # diffusionllama-cpp binary that includes PR #24423.
  # `diffusionPkg` is the diffusionllama-cpp package; callers pass it
  # from `config.myconfig.ai.llama-cpp.diffusionLlamaCpp`.
  llamaServerForDiffusion = diffusionPkg: device: lib.getExe' diffusionPkg "llama-server";

  llamaBenchForDiffusion = diffusionPkg: device: lib.getExe' diffusionPkg "llama-bench";

  # Environment variables exported around llama-server / llama-bench runs to
  # pin them to a specific device.
  # `diffusionCUDA0` resolves to `CUDA0` for the actual env var — the
  # "diffusion" prefix is only used by the Nix-side device routing to
  # pick the patched diffusionllama-cpp binary.
  envForDevice =
    device:
    let
      envDevice =
        if lib.hasPrefix "diffusionCUDA" device then
          "CUDA${lib.removePrefix "diffusionCUDA" device}"
        else
          device;
    in
    [ "LLAMA_ARG_DEVICE=${envDevice}" ]
    ++ lib.optional (
      lib.hasPrefix "Vulkan" device || lib.hasPrefix "ROCm" device
    ) "CUDA_VISIBLE_DEVICES=";

  # Extract the numeric suffix from a device string (e.g. "Vulkan0" -> "0",
  # "ROCm1" -> "1"). Devices that share the same index map to the same
  # physical GPU and therefore must end up in the same llama-swap group.
  deviceIndex =
    device:
    let
      m = builtins.match ".*([0-9]+)$" device;
    in
    if m != null then builtins.head m else device;

  # Lowercase backend name for a device string. Used by the publishers
  # (router.nix, llama-swap.nix) to tag every model entry with the
  # llama.cpp backend it runs on, so tag-based routing / observability
  # can filter on "cuda" vs "rocm" vs "vulkan" vs "diffusion" without
  # inspecting the raw device string. Returns null for unrecognised
  # devices so callers can `lib.optional` it cleanly.
  backendForDevice =
    device:
    if lib.hasPrefix "Vulkan" device then
      "vulkan"
    else if lib.hasPrefix "ROCm" device then
      "rocm"
    else if lib.hasPrefix "diffusionCUDA" device then
      "diffusion"
    else if lib.hasPrefix "CUDA" device then
      "cuda"
    else
      null;
}
