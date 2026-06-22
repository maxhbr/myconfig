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

  # When a device string contains a comma it denotes multiple physical devices
  # (e.g. "Vulkan0,Vulkan1"). All helpers below operate on the *first* device
  # in the list for backend/guard/index decisions, while still preserving the
  # full string where it is passed through verbatim (env export, --device flag).
  firstDevice = device: builtins.head (lib.splitString "," device);

  # Returns true iff the device string spans multiple physical devices.
  isMultiDevice = device: builtins.length (lib.splitString "," device) > 1;
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
  # For multi-device strings (e.g. "Vulkan0,Vulkan1") the decision is made
  # based on the first device in the comma-separated list.
  mkGuardDevice =
    hasGpuVariant: device:
    let
      d = firstDevice device;
    in
    if lib.hasPrefix "Vulkan" d then
      (hasGpuVariant "amd" || hasGpuVariant "amd-no-rocm")
    else if lib.hasPrefix "ROCm" d then
      (hasGpuVariant "amd")
    else if lib.hasPrefix "CUDA" d then
      (hasGpuVariant "nvidia")
    else
      false;

  # Pick the right llama.cpp build for a device string.
  # Multi-device: select binary based on the first device in the list.
  llamaServerFor =
    device:
    let
      d = firstDevice device;
    in
    if lib.hasPrefix "Vulkan" d then
      lib.getExe' pkgs.llama-cpp-vulkan "llama-server"
    else if lib.hasPrefix "ROCm" d then
      lib.getExe' pkgs.llama-cpp-rocm "llama-server"
    else
      lib.getExe' llama-cpp-cuda "llama-server";

  llamaBenchFor =
    device:
    let
      d = firstDevice device;
    in
    if lib.hasPrefix "Vulkan" d then
      lib.getExe' pkgs.llama-cpp-vulkan "llama-bench"
    else if lib.hasPrefix "ROCm" d then
      lib.getExe' pkgs.llama-cpp-rocm "llama-bench"
    else
      lib.getExe' llama-cpp-cuda "llama-bench";

  # Environment variables exported around llama-server / llama-bench runs to
  # pin them to a specific device. For multi-device strings the full
  # comma-separated value is preserved in LLAMA_ARG_DEVICE; the Vulkan/ROCm
  # CUDA_VISIBLE_DEVICES suppression is based on the first device.
  envForDevice =
    device:
    let
      d = firstDevice device;
    in
    [ "LLAMA_ARG_DEVICE=${device}" ]
    ++ lib.optional (lib.hasPrefix "Vulkan" d || lib.hasPrefix "ROCm" d) "CUDA_VISIBLE_DEVICES=";

  # Extract the numeric suffix from a device string (e.g. "Vulkan0" -> "0",
  # "ROCm1" -> "1"). For multi-device strings the index of the first device
  # is used. Devices that share the same index map to the same physical GPU
  # and therefore must end up in the same llama-swap group.
  deviceIndex =
    device:
    let
      d = firstDevice device;
      m = builtins.match ".*([0-9]+)$" d;
    in
    if m != null then builtins.head m else d;

  # Lowercase backend name for a device string. Used by the publishers
  # (router.nix, llama-swap.nix) to tag every model entry with the
  # llama.cpp backend it runs on, so tag-based routing / observability
  # can filter on "cuda" vs "rocm" vs "vulkan" without inspecting the
  # raw device string. Returns null for unrecognised devices so
  # callers can `lib.optional` it cleanly. Multi-device: uses first device.
  backendForDevice =
    device:
    let
      d = firstDevice device;
    in
    if lib.hasPrefix "Vulkan" d then
      "vulkan"
    else if lib.hasPrefix "ROCm" d then
      "rocm"
    else if lib.hasPrefix "CUDA" d then
      "cuda"
    else
      null;

  # Whether the device string spans multiple physical devices.
  inherit isMultiDevice;
}
