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
      cacheType = "q8_0";
      ttl = 3600;
    }
  ];
}
