# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.comfyui;
in
{
  options.myconfig.ai = with lib; {
    comfyui = {
      enable = mkEnableOption "ComfyUI CUDA development environment";
      comfy_base = mkOption {
        type = types.str;
        default = "ComfyUI";
        description = "Base directory for ComfyUI";
      };
      src = mkOption {
        type = types.path;
        default = pkgs.fetchFromGitHub {
          owner = "comfyanonymous";
          repo = "ComfyUI";
          rev = "v0.17.2";
          hash = "sha256-jymf2noIR/QUk7pd1yA3Z+HQ1BZdAVM3Wax91G/d35I=";
        };
        description = "ComfyUI source from nix";
      };
      cuda_version = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "version of cuda to use (e.g., 'cu128')";
      };
      rocm_version = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "version of rocm to use (e.g., 'gfx1151')";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.sharedModules = [
      (
        {
          pkgs,
          lib,
          config,
          ...
        }:
        let
          abs_comfy_base = "${config.home.homeDirectory}/${cfg.comfy_base}";
          pythonEnv = pkgs.python313.withPackages (ps: [
            ps.pip
            ps.virtualenv
            ps.numpy
            ps.einops
            ps.transformers
            ps.tokenizers
            ps.sentencepiece
            ps.safetensors
            ps.aiohttp
            ps.yarl
            ps.pyyaml
            ps.pillow
            ps.scipy
            ps.tqdm
            ps.psutil
            ps.alembic
            ps.sqlalchemy
            ps.filelock
            ps.av
            ps.requests
            ps.simpleeval
            ps.blake3
            ps.kornia
            ps.pydantic
            ps.pydantic-settings
            ps.pyopengl
            ps.glfw
            ps.torch
            ps.torchvision
            ps.torchaudio
          ]);
          mkComfyui = {flavor, pytorch-index-url, libraries, exports, ...}: pkgs.writeShellApplication {
            name = "run-comfyui-${flavor}";

            runtimeInputs = [
              pkgs.cudatoolkit
              pkgs.linuxPackages.nvidia_x11
              pkgs.cudaPackages.cuda_cudart
              pkgs.cudaPackages.cudatoolkit
              pkgs.cudaPackages.cudnn
              pkgs.cudaPackages.nccl
              pkgs.git
              pkgs.gnumake
              pkgs.cmake
              pkgs.ninja
              pkgs.gcc
              pkgs.pkg-config
              pkgs.libGL
              pkgs.libGLU
              pkgs.libXi
              pkgs.libXext
              pkgs.zlib
              pkgs.ncurses5
              pythonEnv
              pkgs.curl
              pkgs.unzip
              (pkgs.writeShellScriptBin "run-comfyui" ''
                #!/usr/bin/env bash
                set -euo pipefail

                export COMFYUI_BASE="${abs_comfy_base}"

                mkdir -p "''${COMFYUI_BASE}"/{user,output,input,temp,custom_nodes}

                exec python3 "${cfg.src}/main.py" \
                  --base-directory "''${COMFYUI_BASE}" \
                  --user-directory "''${COMFYUI_BASE}/user" \
                  --output-directory "''${COMFYUI_BASE}/output" \
                  --input-directory "''${COMFYUI_BASE}/input" \
                  --temp-directory "''${COMFYUI_BASE}/temp" \
                  "$@"
              '')
            ];

            excludeShellChecks = [ "SC1091" ];

            text = ''
              export EXTRA_CCFLAGS="-I/usr/include"
              ${exports}

              export PATH=${
                pkgs.lib.makeBinPath [
                  pkgs.git
                  pkgs.curl
                  pkgs.gnumake
                  pkgs.util-linux
                  pkgs.m4
                  pkgs.gperf
                  pkgs.unzip
                  pkgs.cudatoolkit
                  pkgs.linuxPackages.nvidia_x11
                  pkgs.libGLU
                  pkgs.libGL
                  pkgs.libXi
                  pkgs.libXmu
                  pkgs.freeglut
                  pkgs.libXext
                  pkgs.libX11
                  pkgs.libXv
                  pkgs.libXrandr
                  pkgs.zlib
                  pkgs.ncurses5
                  pythonEnv
                  pkgs.pkg-config
                  pkgs.cmake
                  pkgs.ninja
                  pkgs.gcc
                ]
              }:$PATH

              export LD_LIBRARY_PATH=${
                pkgs.lib.makeLibraryPath ([
                  pkgs.stdenv.cc.cc.lib
                  pkgs.zlib
                  pkgs.ncurses5
                  pkgs.glibc
                ] ++ libraries)
              }:''${LD_LIBRARY_PATH:-}

              COMFYUI_ENV="${abs_comfy_base}/venv.${flavor}"

              echo "Virtual Environment: $COMFYUI_ENV"

              init_venv() (
                set -euo pipefail
                echo "Initializing virtual environment..."
                set -x
                python3 -m venv "$COMFYUI_ENV" --copies
                source "$COMFYUI_ENV/bin/activate"
                pip install --upgrade pip
                pip install -r ${cfg.src}/requirements.txt
                pip install --pre torch torchvision torchaudio --index-url ${pytorch-index-url}
              )

              if [[ ! -d "$COMFYUI_ENV" ]]; then
                init_venv
              fi
              echo "Using existing virtual environment."
              source "$COMFYUI_ENV/bin/activate"

              run-comfyui
            '';
          };
          cudaLibraries = [
            pkgs.linuxPackages.nvidia_x11
            pkgs.cudaPackages.cuda_cudart
          ];
          comfyuiCuda = mkComfyui {
            flavor = "cuda";
            pytorch-index-url = "https://download.pytorch.org/whl/nightly/${cfg.cuda_version}";
            libraries = cudaLibraries;
            exports = ''
            export CUDA_PATH=${pkgs.cudatoolkit}
            export EXTRA_LDFLAGS="-L/lib -L${pkgs.linuxPackages.nvidia_x11}/lib"
            '';
          };

          rocmLibraries = [
            pkgs.rocmPackages.clr
            pkgs.rocmPackages.rocblas
            pkgs.rocmPackages.hipblas
            pkgs.rocmPackages.miopen
          ];

          comfyuiRocmGFX1151 = mkComfyui {
            # for: Ryzen AI Max 395 (gfx1151 / Strix Halo)
            flavor = "rocm";
            pytorch-index-url = "https://rocm.nightlies.amd.com/v2/gfx1151/";
            libraries = rocmLibraries;
            exports = ''
            unset CUDA_VISIBLE_DEVICES
            export HIP_VISIBLE_DEVICES=0
            export HSA_OVERRIDE_GFX_VERSION=11.5.1
            export GPU_MAX_HEAP_SIZE=100
            export GPU_MAX_ALLOC_PERCENT=100
            export AMD_LOG_LEVEL=0
            export FLASH_ATTENTION_TRITON_AMD_ENABLE=1
            export HSA_ENABLE_SDMA=0
            '';
          };
        in
        {
          home.packages = 
            (lib.optional (cfg.cuda_version != null) comfyuiCuda) ++ (lib.optional (cfg.rocm_version == "gfx1151") comfyuiRocmGFX1151);
          myconfig.persistence.cache-directories = [ cfg.comfy_base ];
        }
      )
    ];
  };
}
