# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:
let
  llama-cpp-gfx1151 = pkgs.symlinkJoin {
    name = "llama-cpp-optimized";
    paths = [ 
      (pkgs.llama-cpp.override {
        vulkanSupport = true;
        # Force the compiler to target the RDNA 3.5 ISA
        rocmGpuTargets = [ "gfx1151" ];
        # extraCmakeFlags = [ 
        #   "-DGGML_HIP_ROCWMMA=ON" 
        #   "-DGGML_HIP_ROCWMMA_FATTN=ON" # Critical for long context performance
        # ];
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
in
{
  config = {
    myconfig = {
      hardware.gpu.variant = "amd-no-rocm";
      ai.inference-cpp.ollama-cpp.package = llama-cpp-gfx1151;
    };
  };
}
