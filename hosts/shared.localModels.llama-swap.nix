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
    "CUDA0:Qwen3.5-9B-Q5_K_M"
    "CUDA0:Qwen3.6-27B-Q8_0"
    "CUDA0:Qwen3.6-27B-Q8_0-tweaked"
    "CUDA0:Qwen3.6-27B-UD-Q4_K_XL"
    "CUDA0:Qwen3.6-27B-UD-Q5_K_XL"
    "CUDA0:Qwen3.6-27B-UD-Q6_K_XL"
    "CUDA0:Qwen3.6-35B-A3B-UD-Q5_K_XL"
    "CUDA0:gemma-4-26B-A4B-it-Q8_K_XL"
    "CUDA0:gemma-4-31B-it-Q6_K_XL"
    "Qwen3.5-9B-Q5_K_M"
    "Qwen3.6-27B-Q8_0"
    "Qwen3.6-27B-Q8_0-tweaked"
    "Qwen3.6-27B-UD-Q4_K_XL"
    "Qwen3.6-27B-UD-Q5_K_XL"
    "Qwen3.6-27B-UD-Q6_K_XL"
    "Qwen3.6-35B-A3B-UD-Q5_K_XL"
    "gemma-4-26B-A4B-it-Q8_K_XL"
    "gemma-4-31B-it-Q6_K_XL"
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
