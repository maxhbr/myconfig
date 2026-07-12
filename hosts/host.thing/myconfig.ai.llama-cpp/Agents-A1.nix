{ modelsPullDir }:
{
  rtxModels = [
    {
      name = "InternScience-Agents-A1-Q4_K_M";
      path = "/models/InternScience-Agents-A1-Q4_K_M-GGUF/Agents-A1-Q4_K_M.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [
          "InternScience/Agents-A1-Q4_K_M-GGUF/Agents-A1-Q4_K_M.gguf"
          "InternScience/Agents-A1-Q4_K_M-GGUF/Agents-A1-mmproj.gguf"
        ];
      };
      params = [
        "--temp"
        "0.85"
        "--top-p"
        "0.95"
        "--top-k"
        "20"
        "--min-p"
        "0.0"
        "--presence-penalty"
        "1.1"
        "--repeat-penalty"
        "1.0"
      ];
      variants = {
        mmproj = {
          mmproj = "/models/InternScience-Agents-A1-Q4_K_M-GGUF/Agents-A1-mmproj.gguf";
        };
      };
      ctxSize = 262144;
      ttl = 1500;
    }
  ];

  amdModels = [
    {
      name = "InternScience-Agents-A1-Q8_0";
      path = "/models/InternScience-Agents-A1-Q8_0-GGUF/Agents-A1-Q8_0.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "InternScience/Agents-A1-Q8_0-GGUF/Agents-A1-Q8_0.gguf" ];
      };
      ttl = 1500;
    }
  ];
}
