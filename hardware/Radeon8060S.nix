{
  config,
  lib,
  pkgs,
  ...
}:
let
  # Optimized llama-cpp for Strix Halo
  llama-cpp-max = pkgs.llama-cpp.override {
    rocmSupport = true;
    # Force the compiler to target the RDNA 3.5 ISA
    gpuTargets = [ "gfx1151" ]; 
  };
  llama-cpp-optimized = pkgs.symlinkJoin {
    name = "llama-cpp-optimized";
    paths = [ llama-cpp-max ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/llama-cli \
        --set HSA_OVERRIDE_GFX_VERSION 11.5.1 \
        --set GGML_HIP_VISIBLE_DEVICES 0
    '';
  };
in
{
  config = {
    myconfig = {
      hardware.gpu.variant = "amd";
      ai.inference-cpp.ollama-cpp.package = llama-cpp-optimized
    };
  };
}
