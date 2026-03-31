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
  hasVariant = v: builtins.elem v cfg.variant;
  hasAnyVariant = cfg.variant != [ ];
  hasMultipleGpuFamilies =
    (hasVariant "nvidia") && (hasVariant "amd" || hasVariant "amd-no-rocm" || hasVariant "intel")
    || (hasVariant "amd" || hasVariant "amd-no-rocm") && (hasVariant "intel");
  nvtopPackage =
    if hasMultipleGpuFamilies then
      pkgs.nvtopPackages.full
    else if hasVariant "nvidia" then
      pkgs.nvtopPackages.nvidia
    else if hasVariant "amd" || hasVariant "amd-no-rocm" then
      pkgs.nvtopPackages.amd
    else if hasVariant "intel" then
      pkgs.nvtopPackages.intel
    else
      pkgs.nvtopPackages.full;
in
{
  options.myconfig.hardware.gpu.variant = lib.mkOption {
    type = lib.types.listOf (
      lib.types.enum [
        "nvidia"
        "amd"
        "amd-no-rocm"
        "intel"
      ]
    );
    default = [ ];
    description = "List of GPU variants to enable. Multiple variants can be active simultaneously.";
  };
  config = lib.mkIf hasAnyVariant (
    lib.mkMerge [
      {
        hardware.graphics = {
          enable = true;
        };
        home-manager.sharedModules = [
          {
            home.packages = [ nvtopPackage ];
          }
        ];
        # nixpkgs.config.cudaSupport = lib.mkDefault (hasVariant "nvidia");
        # nixpkgs.config.rocmSupport = lib.mkDefault (hasVariant "amd");
      }
      (lib.mkIf (hasVariant "nvidia") {
        home-manager.sharedModules = [
          {
            home.packages = with pkgs.cudaPackages; [
              cudatoolkit
            ];
          }
        ];
        users.extraUsers."${myconfig.user}".extraGroups = [ "nvidia" ];
        services.xserver.videoDrivers = [ "nvidia" ];
        hardware.nvidia.open = true;
        hardware.nvidia-container-toolkit.enable = true;
      })
      # (lib.mkIf (hasVariant "intel") ({ imports = [ inputs.nixos-hardware.nixosModules.common-gpu-amd ]; }))
      (lib.mkIf (hasVariant "amd" || hasVariant "amd-no-rocm") {
        services.xserver.videoDrivers = [ "amdgpu" ];
        home-manager.sharedModules = [
          {
            home.packages = with pkgs; [
              vulkan-tools
            ];
          }
        ];
        hardware.graphics = {
          extraPackages = with pkgs; [
            rocmPackages.clr.icd
          ];
        };
      })
      (lib.mkIf (hasVariant "amd") {
        home-manager.sharedModules = [
          {
            home.packages =
              with pkgs;
              [
                rocmPackages.rocminfo
                rocmPackages.rocm-smi
                clinfo
                onnxruntime
              ]
              ++ (with pkgs.python3Packages; [
                onnx
                onnxruntime-tools
              ]);
          }
        ];
        systemd.tmpfiles.rules =
          let
            rocmEnv = pkgs.symlinkJoin {
              name = "rocm-combined";
              paths = with pkgs.rocmPackages; [
                rocblas
                hipblas
                clr
              ];
            };
          in
          [
            "L+    /opt/rocm   -    -    -     -    ${rocmEnv}"
          ];
        hardware.amdgpu.opencl.enable = true;
      })
      (lib.mkIf (hasVariant "amd-no-rocm") {
        nixpkgs.config.rocmSupport = false;
        home-manager.sharedModules = [
          {
            home.packages =
              with pkgs;
              [
                onnxruntime
              ]
              ++ (with pkgs.python3Packages; [
                onnx
                onnxruntime-tools
              ]);
          }
        ];
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
      (lib.mkIf (hasVariant "intel") {
      })
      {
        systemd.tmpfiles.rules = [
          "d /run/myconfig 0755 root root - -"
          "f /run/myconfig/gpu-variant 0644 root root - ${builtins.concatStringsSep "," cfg.variant}"
        ];
        hardware.graphics.extraPackages = [ intel-compute-runtime ];
      }
    ]
  );
}
