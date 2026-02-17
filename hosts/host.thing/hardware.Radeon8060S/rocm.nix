# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  pkgs,
  myconfig,
  inputs,
  ...
}:
let
  llama-cpp-gfx1151 = pkgs.symlinkJoin {
    name = "llama-cpp-optimized";
    paths = [
      (pkgs.llama-cpp.override {
        rocmSupport = true;
        rocmGpuTargets = [ "gfx1151" ];
      })
    ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/llama-cli \
        --set HSA_OVERRIDE_GFX_VERSION 11.5.1 \
        --set GGML_HIP_VISIBLE_DEVICES 0\
        --set HSA_ENABLE_SDMA 0 \
        --set HIP_FORCE_DEV_KERNARG 1
    '';
  };

  ollama-rocm-gfx1151 = pkgs.ollama-rocm.override {
    rocmGpuTargets = [ "gfx1151" ];
  };
in
{
  config = {
    myconfig = {
      hardware.gpu.variant = "amd";
      ai.inference-cpp.ollama-cpp.package = llama-cpp-gfx1151;
    };
    services.ollama.package = lib.mkForce ollama-rocm-gfx1151;
  };
}
