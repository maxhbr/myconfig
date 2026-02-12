{
  config,
  lib,
  pkgs,
  myconfig,
  inputs,
  ...
}:
let
  user = myconfig.user;
  cfg = config.myconfig.hardware.gpu;
in
{
  options.myconfig.hardware.gpu.variant = lib.mkOption {
    type = lib.types.nullOr (
      lib.types.enum [
        "nvidia"
        "amd"
        "amd-no-rocm"
        "intel"
      ]
    );
    default = null;
  };
  config = lib.mkIf (cfg.variant != null) (
    lib.mkMerge [
      (lib.mkIf (cfg.variant != null) {
        hardware.graphics = {
          enable = true;
        };
        # nixpkgs.config.cudaSupport = lib.mkDefault (cfg.variant == "nvidia");
        # nixpkgs.config.rocmSupport = lib.mkDefault (cfg.variant == "amd");
      })
      (lib.mkIf (cfg.variant == "nvidia") {
        home-manager.sharedModules = [
          {
            home.packages =
              with pkgs;
              [
                nvtopPackages.nvidia
              ]
              ++ (with pkgs.cudaPackages; [
                cudatoolkit
              ]);
          }
        ];
        users.extraUsers."${myconfig.user}".extraGroups = [ "nvidia" ];
        services.xserver.videoDrivers = [ "nvidia" ];
        hardware.nvidia.open = true;
        hardware.nvidia-container-toolkit.enable = true;
        services.ollama.package = pkgs.ollama-cuda;
      })
      # (lib.mkIf (
      #   config.hardware.gpu.variant == "intel"
      # ) ({ imports = [ inputs.nixos-hardware.nixosModules.common-gpu-amd ]; }))
      (lib.mkIf (cfg.variant == "amd" || cfg.variant == "amd-no-rocm") {
        services.xserver.videoDrivers = [ "amdgpu" ];
        home-manager.sharedModules = [
          {
            home.packages = with pkgs; [
              vulkan-tools
              nvtopPackages.amd
            ];
          }
        ];
      })
      (lib.mkIf (cfg.variant == "amd") {
        home-manager.sharedModules = [
          {
            home.packages = with pkgs; [
              nvtopPackages.amd
              rocmPackages.rocminfo
              rocmPackages.rocm-smi
            ];
          }
        ];
        services.ollama.package = pkgs.ollama-rocm;
      })
      (lib.mkIf (cfg.variant == "amd-no-rocm") {
        nixpkgs.config.rocmSupport = false;
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
        services.ollama.package = pkgs.ollama-vulkan;
      })
      (lib.mkIf (cfg.variant == "intel") {
      })
    ]
  );
}
