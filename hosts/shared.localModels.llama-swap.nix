{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
let
  models = [
    # "CUDA:Qwen3.5-27B-Q8_0"
    # "ROCm0:Qwen3.5-27B-Q8_0"
    "Vulkan1:qwen3.5-122B-A10B-Q5_K_M"
    "Vulkan0:Qwen3.5-27B-Q8_0"
                  devices = mkOption {
                type = types.listOf types.str;
                default = [ ];
                description = "Devices to provide scripts for the CARD";
              };"Vulkan0:Qwen3.5-27B-Q8_0:modded"
    "Vulkan1:Qwen3.5-27B-Q8_0"
    "Vulkan1:Qwen3.5-27B-BF16"
  ];
in
{
  config = {
    myconfig.ai.localModels = [
      {
        name = "llama-swap.thing";
        inherit models;
        port = 33656;
        host = myconfig.metadatalib.getIp "thing";
      }
      {
        name = "llama-swap.thing.wg0";
        inherit models;
        port = 33656;
        host = myconfig.metadatalib.getWgIp "thing";
      }
    ];
  };
}
