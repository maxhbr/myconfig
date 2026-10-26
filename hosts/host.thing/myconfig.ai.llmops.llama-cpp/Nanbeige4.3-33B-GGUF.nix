{ modelsPullDir }:
{
  rtxModels = [
    {
      name = "Nanbeige4.3-33B-AD-Q6_K-Q5_K";
      path = "/models/AtomicChat-Nanbeige4.3-33B-GGUF/Nanbeige4.3-33B-AD-Q6_K-Q5_K.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [
          "AtomicChat/Nanbeige4.3-33B-GGUF/Nanbeige4.3-33B-AD-Q6_K-Q5_K.gguf"
        ];
      };
      ttl = 1800;
    }
    {
      name = "Nanbeige4.3-33B-AD-Q5_K_M";
      path = "/models/AtomicChat-Nanbeige4.3-33B-GGUF/Nanbeige4.3-33B-AD-Q5_K_M.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [
          "AtomicChat/Nanbeige4.3-33B-GGUF/Nanbeige4.3-33B-AD-Q5_K_M.gguf"
        ];
      };
      ttl = 1800;
    }
  ];

  amdModels = [
    {
      name = "Nanbeige4.3-33B-AD-Q6_K-Q5_K";
      path = "/models/AtomicChat-Nanbeige4.3-33B-GGUF/Nanbeige4.3-33B-AD-Q6_K-Q5_K.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [
          "AtomicChat/Nanbeige4.3-33B-GGUF/Nanbeige4.3-33B-AD-Q6_K-Q5_K.gguf"
        ];
      };
      ttl = 1800;
    }
    {
      name = "Nanbeige4.3-33B-AD-Q5_K_M";
      path = "/models/AtomicChat-Nanbeige4.3-33B-GGUF/Nanbeige4.3-33B-AD-Q5_K_M.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [
          "AtomicChat/Nanbeige4.3-33B-GGUF/Nanbeige4.3-33B-AD-Q5_K_M.gguf"
        ];
      };
      ttl = 1800;
    }
  ];
}
