{ modelsPullDir }:
{
  amdModels = [
    {
      name = "Hy3-Q2_K_L";
      path = "/models/bartowski-Hy3-GGUF/Hy3-Q2_K_L/Hy3-Q2_K_L-00001-of-00003.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "bartowski/Hy3-GGUF/Hy3-Q2_K_L" ];
      };
      ctxSize = 131072;
      cacheType = "q4_0";
      ttl = 3600;
    }
  ];

  # Multi-GPU variant of Hy3-Q2_K_L spread across two Vulkan devices
  # (Vulkan0 + Vulkan1). The Q2_K_L quant is ~107 GB on disk, so the
  # layer split must heavily favour the larger GPU: the small GPU only
  # has ~32 GB VRAM, so we put ~1/5 (≈21 GB) on it and ~4/5 (≈86 GB)
  # on the large GPU via `tensorSplit = "1,4"`.
  multiGpuModels = [
    {
      name = "Hy3-Q2_K_L-split";
      path = "/models/bartowski-Hy3-GGUF/Hy3-Q2_K_L/Hy3-Q2_K_L-00001-of-00003.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "bartowski/Hy3-GGUF/Hy3-Q2_K_L" ];
      };
      devices = [ "Vulkan0,Vulkan1" ];
      tensorSplit = "1,4";
      params = [
        "--no-mmap"
        "--chat-template-kwargs"
        "{\"preserve_thinking\":true}"
      ];
      ctxSize = 131072;
      cacheType = "q8_0";
      parallel = 1;
      ttl = 3600;
    }
  ];
}
