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
in
{
  options.hardware.gpu.variant = lib.mkOption {
    type = lib.nullOr (lib.types.enum [ "nvidia" "amd" "amd-no-rocm" "intel" ]);
    default = "nvidia";
  };
  config = lib.mkIf config.hardware.gpu.variant != null (lib.mkMerge [
    (lib.mkIf (config.hardware.gpu.variant != null)
    {
      hardware.graphics = {
        enable = true;
      };
      nixpkgs.config.cudaSupport = lib.mkDefault (config.hardware.gpu.variant == "nvidia");
      nixpkgs.config.rocmSupport = lib.mkDefault (config.hardware.gpu.variant == "amd");
    })
    (lib.mkIf (config.hardware.gpu.variant == "nvidia") {
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
      hardware.graphics = {
        enable = true;
      };
      services.xserver.videoDrivers = [ "nvidia" ];
      hardware.nvidia-container-toolkit.enable = true;
    })
    (lib.mkIf (config.hardware.gpu.variant == "intel") inputs.nixos-hardware.nixosModules.common-gpu-amd)
    (lib.mkIf (config.hardware.gpu.variant == "amd" || config.hardware.gpu.variant == "amd-no-rocm") {
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
    (lib.mkIf (config.hardware.gpu.variant == "amd") {
      nixpkgs.config.rocmSupport = true;
      home-manager.sharedModules = [
        {
          home.packages = with pkgs; [
            nvtopPackages.amd
            rocmPackages.rocminfo
            rocmPackages.rocm-smi
          ];
        }
      ];
    })
    (lib.mkIf (config.hardware.gpu.variant == "amd-no-rocm") {
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
    })
    (lib.mkIf (config.hardware.gpu.variant == "intel") {
    })
  ]);
}