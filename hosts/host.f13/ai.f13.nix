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
    # users.extraUsers."${myconfig.user}".extraGroups = ["nvidia"];
    nixpkgs.config.rocmSupport = true;
  }];
  config = {
    # boot.kernelParams = [
    #   "pcie_aspm=off"
    # ];
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
      # home = "/home/ollama";

      openFirewall = false;
      acceleration = "rocm";
      host = "127.0.0.1";
      # environmentVariables = {
      #   OLLAMA_ORIGIN = "*";
      #   OLLAMA_KEEP_ALIVE = "5m";
      # };
      # loadModels = [
      #   "llama3.1:8b"
      #   "phi4"
      # ];
    };

    home-manager.sharedModules =
      [{ home.packages = [ ai-tmux-session-script ]; }];
  };
}
