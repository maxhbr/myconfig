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
{
  imports = [
    ({
      config = lib.mkIf (builtins.elem "amd" config.myconfig.hardware.gpu.variant) {
        services.ollama = {
          environmentVariables = {
            HSA_OVERRIDE_GFX_VERSION = "11.5.1";
            HIP_VISIBLE_DEVICES = "1";
            HCC_AMDGPU_TARGET = "gfx1151";
            HSA_ENABLE_SDMA = "1";
          };
          rocmOverrideGfx = "11.5.1";
        };
      };
    })
  ];
  config = lib.mkIf config.services.ollama.enable {
    # TODO: setting home does not work and without it, it writes in tmpfs
    systemd.tmpfiles.rules = [ "d /persistent/cache/ollama 0700 ollama ollama -" ];
    services.ollama = {
      home = "/persistent/cache/ollama";
      environmentVariables = {
        # OLLAMA_FLASH_ATTENTION = lib.mkForce "0";
        OLLAMA_ORIGIN = "*";
      };

      openFirewall = false;
      # host = "0.0.0.0";

      # loadModels = [
      #   # "hf.co/unsloth/Qwen3.5-122B-A10B-GGUF:Q6_K"
      #   # "cogito:32b"
      #   # "deepseek-r1:32b"
      #   # "gemma3:27b"
      #   # "glm-4.7-flash:q8_0"
      #   # "glm-4.7-flash:bf16"
      #   # "gpt-oss:20b"
      #   # "gpt-oss:120b"
      #   # "granite4"
      #   # "llama4:16x17b"
      #   # "llama3.2:3b"
      #   # "llava:7b"
      #   # "llava:34b"
      #   # "magistral:24b"
      #   # "openthinker:32b"
      #   # "phi4"
      #   # "qwen3-coder-next:q8_0"
      #   # "qwen2.5vl:32b"
      #   # "qwen3-vl:32b"
      #   # "qwen3.5:122b"
      #   # "qwen3:30b"
      #   # "qwen3:32b"
      #   # "qwq:32b"
      #   # "smollm2:1.7b"
      # ];
    };
  };
}
