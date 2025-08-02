{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = {
    nixpkgs.overlays = [
      (final: prev: {
        # see: https://github.com/NixOS/nixpkgs/issues/409284#issuecomment-2952396401
        llama-cpp =
          (prev.llama-cpp.overrideAttrs (oldAttrs: {
            postPatch = (oldAttrs.postPatch or "") + ''
              echo "Applying patch to ggml/src/ggml-vulkan/CMakeLists.txt"
              sed -i '/DCMAKE_RUNTIME_OUTPUT_DIRECTORY/d' ggml/src/ggml-vulkan/CMakeLists.txt
            '';
          })).override
            {
              # cudaSupport = false;
              # rocmSupport = false;
              vulkanSupport = true;
            };
      })
    ];
    nixpkgs.config.cudaSupport = lib.mkDefault false;
    nixpkgs.config.rocmSupport = false; # gfx1150 has no rocm support, see https://rocm.docs.amd.com/en/latest/compatibility/compatibility-matrix.html
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          vulkan-tools
          nvtopPackages.amd
        ];
      }
    ];
  };
}
