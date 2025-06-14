# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, myconfig, inputs, ... }:
let
  ai-tmux-session = "ai";
  ai-tmux-session-script = pkgs.writeShellScriptBin "ai-tmux-session" ''
    # if session is not yet created, create it
    if ! tmux has-session -t ${ai-tmux-session}; then
      tmux new-session -d -s ${ai-tmux-session}
      tmux send-keys -t ${ai-tmux-session}:1 "btop" C-m
      tmux split-window -h -t ${ai-tmux-session}
      tmux send-keys -t ${ai-tmux-session}:1 "nvtop -i" C-m
      tmux split-window -v -t ${ai-tmux-session}
      tmux send-keys -t ${ai-tmux-session}:1 "journalctl -f" C-m
      tmux split-window -v -t ${ai-tmux-session}
    fi
    exec tmux attach-session -t ${ai-tmux-session}
  '';
in {
  imports = [{
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
          cudaSupport = false;
          rocmSupport = false;
          vulkanSupport = true;
        };
      })
    ];

    home-manager.sharedModules = [{
      home.packages = with pkgs;
        [
          nvtopPackages.amd
          rocmPackages.rocminfo
          rocmPackages.rocm-smi
          onnxruntime
        ] ++ (with pkgs.python3Packages; [
          onnx
          onnxruntime-tools
          huggingface-hub
        ]);
    }];
    users.extraUsers."${myconfig.user}".extraGroups = ["nvidia"];
    nixpkgs.config.rocmSupport = false; # gfx1150 has no rocm support, see https://rocm.docs.amd.com/en/latest/compatibility/compatibility-matrix.html
  }];
  config = {
    myconfig = {
      ai = {
        enable = true;
        coding.enable = true;
        inference-cpp = { enable = true; };
        lmstudio = { enable = false; };
      };
    };
    services.ollama = {
      enable = false;

      openFirewall = false;
      # acceleration = "rocm";
      host = "127.0.0.1";
    };

    home-manager.sharedModules =
      [{ home.packages = [ ai-tmux-session-script ]; }];
  };
}
