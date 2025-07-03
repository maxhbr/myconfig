{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
let
  run-comfyui = pkgs.writeShellScriptBin "run-comfyui" ''
    export CUDA_PATH=${pkgs.cudatoolkit}
    export EXTRA_LDFLAGS="-L/lib -L${pkgs.linuxPackages.nvidia_x11}/lib"
    export EXTRA_CCFLAGS="-I/usr/include"

    export PATH=${
      pkgs.lib.makeBinPath (
        with pkgs;
        [
          git
          gitRepo
          gnupg
          autoconf
          curl
          procps
          gnumake
          util-linux
          m4
          gperf
          unzip
          cudatoolkit
          linuxPackages.nvidia_x11
          libGLU
          libGL
          xorg.libXi
          xorg.libXmu
          freeglut
          xorg.libXext
          xorg.libX11
          xorg.libXv
          xorg.libXrandr
          zlib
          ncurses5
          stdenv.cc
          binutils

          python313
          python313Packages.pip
          #python331Packages.venv

          # Build dependencies that may be needed
          pkg-config
          cmake
          ninja
          gcc

          # System libraries needed for OpenCV and other dependencies
          zlib
          stdenv.cc.cc.lib # Provides libstdc++
          opencv

          # Add these to your packages list if you have an NVIDIA GPU
          cudaPackages.cuda_cudart
          # cudaPackages.cuda_runtime
          cudaPackages.cudatoolkit
        ]
      )
    }:$PATH

    export LD_LIBRARY_PATH=${
      pkgs.lib.makeLibraryPath [
        pkgs.stdenv.cc.cc.lib
        pkgs.zlib
        pkgs.opencv
        pkgs.linuxPackages.nvidia_x11
        pkgs.ncurses5
      ]
    }:$LD_LIBRARY_PATH

    COMFYUI_DIR=${config.users.users.${myconfig.user}.home}/run-comfyui
    COMFYUI_REPO='https://github.com/comfyanonymous/ComfyUI'
    COMFYUI_BRANCH='548457bac47bb6c0ce233a9f5abb3467582d710d'
    COMFYUI_ENV=$COMFYUI_DIR/comfyui_env

    set -euo pipefail

    if [[ ! -d "$COMFYUI_DIR" ]]; then
      git clone "$COMFYUI_REPO" "$COMFYUI_DIR"
    else
      echo "ComfyUI already cloned"
    fi
    cd "$COMFYUI_DIR"
    git fetch --all --prune
    git checkout "$COMFYUI_BRANCH"


    if [[ ! -d "$COMFYUI_ENV" ]]; then
      python3 -m venv "$COMFYUI_ENV"

      source "$COMFYUI_ENV/bin/activate"

      pip install -r requirements.txt
      pip install --pre torch torchvision torchaudio --index-url https://download.pytorch.org/whl/nightly/cu128
    else
      echo "ComfyUI venv already initialized"
      source "$COMFYUI_ENV/bin/activate"
    fi

    exec python main.py
  '';

in
{
  environment.systemPackages = [ run-comfyui ];
}
