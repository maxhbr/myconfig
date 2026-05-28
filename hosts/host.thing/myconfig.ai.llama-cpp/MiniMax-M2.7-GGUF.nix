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
      name = "MiniMax-M2.7-UD-Q3_K_S";
      path = "/models/unsloth-MiniMax-M2.7-GGUF/UD-Q3_K_S/MiniMax-M2.7-UD-Q3_K_S-00001-of-00003.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/MiniMax-M2.7-GGUF/UD-Q3_K_S" ];
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
    {
      name = "MiniMax-M2.7-UD-IQ4_NL";
      path = "/models/unsloth-MiniMax-M2.7-GGUF/UD-IQ4_NL/MiniMax-M2.7-UD-IQ4_NL-00001-of-00004.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/MiniMax-M2.7-GGUF/UD-IQ4_NL" ];
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
        "49k" = {
          ctxSize = ctxSizeQuarter;
        };
      };
    }
    {
      name = "MiniMax-M2.7-UD-IQ4_XS";
      path = "/models/unsloth-MiniMax-M2.7-GGUF/UD-IQ4_XS/MiniMax-M2.7-UD-IQ4_XS-00001-of-00004.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/MiniMax-M2.7-GGUF/UD-IQ4_XS" ];
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
