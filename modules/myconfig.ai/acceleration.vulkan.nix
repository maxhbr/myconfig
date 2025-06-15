{ config, lib, pkgs, ... }:
{
  options.myconfig = with lib; {
    ai.acceleration.vulkan.enable = mkEnableOption "myconfig.ai.acceleration.vulkan";
  };
  config = lib.mkIf config.myconfig.ai.acceleration.vulkan.enable {
    nixpkgs.overlays = [
      (final: prev: {
        # see: https://github.com/NixOS/nixpkgs/issues/409284#issuecomment-2952396401
        llama-cpp = (prev.llama-cpp.overrideAttrs (oldAttrs: {
          postPatch =
            (oldAttrs.postPatch or "")
            + ''
              echo "Applying patch to ggml/src/ggml-vulkan/CMakeLists.txt"
              sed -i '/DCMAKE_RUNTIME_OUTPUT_DIRECTORY/d' ggml/src/ggml-vulkan/CMakeLists.txt
            '';
        })).override {
          # cudaSupport = false;
          # rocmSupport = false;
          vulkanSupport = true;
        };
      })
    ];
    home-manager.sharedModules = [{
      home.packages = with pkgs; [
        vulkan-tools
      ];
    }];
  };
}