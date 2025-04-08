# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, myconfig, inputs, ... }: {
  imports = [ 
    ../../hardware/eGPU.nix
    ./run-comfyui.nix
  ];

  config = {
    myconfig = {
      services.pciDeviceMonitor = {
        enable = true;
        searchSubstring = "NVIDIA Corporation GB202";  # or your specific device
        retries = 3;
        checkInterval = 60;
        initialDelay = 60;
      };
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
      acceleration = "cuda";
      # environmentVariables = {
      #   OLLAMA_LLM_LIBRARY = "cpu";
      #   HIP_VISIBLE_DEVICES = "0,1";
      # };
      loadModels = [
        "deepseek-r1:32b"
        "gemma3:27b"
        "llama3.2:3b"
        "llava:34b"
        "mistral-small:24b"
        "mistral:7b"
        "openthinker:32b"
        "phi4"
        "qwq:32b"
        "smollm2:1.7b"
      ];
    };
    services.tabby = {
      enable = false;
      acceleration = "cuda";
      model = "TabbyML/Qwen2.5-Coder-14B";
    };
  };
}
