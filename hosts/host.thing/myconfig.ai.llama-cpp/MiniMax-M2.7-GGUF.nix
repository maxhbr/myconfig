let
  modelsPullDir = "/home/mhuber/models";
in
{
  amdModels = [
    {
      name = "MiniMax-M2.7-UD-Q3_K_S";
      path = "/models/MiniMax-M2.7-GGUF/UD-Q3_K_S/MiniMax-M2.7-UD-Q3_K_S-00001-of-00003.gguf";
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
      ttl = 300;
    }
    {
      name = "MiniMax-M2.7-UD-IQ4_XS";
      path = "/models/MiniMax-M2.7-GGUF/UD-IQ4_XS/MiniMax-M2.7-UD-IQ4_XS-00001-of-00004.gguf";
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
      ttl = 300;
    }
  ];
}
