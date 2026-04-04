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
    "CUDA0:Qwen3.5-27B-Q8_0"
    "CUDA0:Qwen3.5-27B-Q8_0:mmproj"
    "CUDA0:gemma-4-26B-A4B-it-Q8_K_XL"
    "CUDA0:gemma-4-31B-it-Q6_K_XL"
    "Vulkan0:Qwen3.5-27B-Q8_0"
    "Vulkan0:Qwen3.5-27B-Q8_0:mmproj"
    "Vulkan0:Qwen3.5-27B-Q8_0:modded"
    "Vulkan0:gemma-4-26B-A4B-it-Q8_K_XL"
    "Vulkan0:gemma-4-31B-it-Q6_K_XL"
    "Vulkan1:Qwen3.5-27B-BF16"
    "Vulkan1:Qwen3.5-27B-BF16:modded"
    "Vulkan1:Qwen3.5-27B-Q8_0"
    "Vulkan1:Qwen3.5-27B-Q8_0:mmproj"
    "Vulkan1:gemma-4-26B-A4B-it-BF16"
    "Vulkan1:gemma-4-26B-A4B-it-Q8_K_XL"
    "Vulkan1:gemma-4-31B-it-BF16"
    "Vulkan1:gemma-4-31B-it-Q6_K_XL"
    "Vulkan1:qwen3.5-122B-A10B-Q5_K_M"
    "Vulkan1:qwen3.5-122B-A10B-Q5_K_M:mmproj"
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
