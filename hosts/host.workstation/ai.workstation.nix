# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
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
in
{
  imports = [ ];

  config = {
    # boot.kernelParams = [
    #   "pcie_aspm=off"
    # ];
    myconfig = {
      ai = {
        enable = true;
        container = {
          nlm-ingestor = {
            enable = false;
          };
          open-webui = {
            enable = true;
          };
        };
      };
    };
    services.ollama = {
      enable = true;

      # host = myconfig.metadatalib.metadata.hosts."${config.networking.hostName}".ip4;
      # listenAddress = "0.0.0.0:11434";
      acceleration = "rocm";
      # environmentVariables = {
      #   OLLAMA_LLM_LIBRARY = "cpu";
      #   HIP_VISIBLE_DEVICES = "0,1";
      # };
      loadModels = [
        # "cogito:32b"
        # "deepseek-r1:32b"
        # "gemma3:27b"
        # "llama3.2:3b"
        # "llava:34b"
        # "mistral-small:24b"
        # "mistral:7b"
        # "openthinker:32b"
        # "phi4"
        # "qwq:32b"
        # "smollm2:1.7b"
      ];
    };
    services.tabby = {
      enable = false;
      acceleration = "cuda";
      model = "TabbyML/Qwen2.5-Coder-14B";
    };
    home-manager.sharedModules = [ { home.packages = [ ai-tmux-session-script ]; } ];
  };
}
