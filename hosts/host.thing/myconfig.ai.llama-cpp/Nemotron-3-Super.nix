let
  modelsPullDir = "/home/mhuber/models";
  # Context sizes from /props endpoint: 196608 = full, 131072 = 128k, 49152 = quarter
  ctxSize = 196608;
  ctxSize128k = 131072;
  ctxSizeQuarter = ctxSize / 4; # 49152
in
{
  amdModels = [
    {
      name = "NVIDIA-Nemotron-3-Super-120B-A12B-Q5_K_M";
      path = "/models/Nemotron-3-Super-120B-A12B-GGUF/NVIDIA-Nemotron-3-Super-120B-A12B-BF16-Q5_K_M-00001-of-00003.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "AesSedai/NVIDIA-Nemotron-3-Super-120B-A12B-GGUF/Q5_K_M" ];
      };
      params = [
        "--temp"
        "1.0"
        "--top-p"
        "0.95"
        "--top-k"
        "40"
        "--cache-type-k"
        "q4_0"
        "--cache-type-v"
        "q4_0"
      ];
      ttl = 1500;
      ctxSize = ctxSize128k;
      variants = {
        "196k" = {
          ctxSize = ctxSize;
        };
        # "128k" = { ctxSize = ctxSize128k; };
        "49k" = {
          ctxSize = ctxSizeQuarter;
        };
      };
    }
  ];
}
