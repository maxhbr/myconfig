let
  modelsPullDir = "/home/mhuber/models";
  recommended_variants_Ornith1_0_35B = {
    general = {
      params = [
        "--temp"
        "0.7"
        "--top-p"
        "0.95"
        "--top-k"
        "40"
        "--min-p"
        "0.0"
      ];
    };
    creative = {
      params = [
        "--temp"
        "1.0"
        "--top-p"
        "0.95"
        "--top-k"
        "20"
        "--min-p"
        "0.0"
      ];
    };
    instruct = {
      params = [
        "--temp"
        "0.7"
        "--top-p"
        "0.8"
        "--top-k"
        "20"
        "--min-p"
        "0.0"
      ];
    };
  };
in
{
  rtxModels = [
    {
      name = "Ornith-1.0-35B-Q4_K_M";
      path = "/models/deepreinforce-ai-Ornith-1.0-35B-GGUF/ornith-1.0-35b-Q4_K_M.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "deepreinforce-ai/Ornith-1.0-35B-GGUF/ornith-1.0-35b-Q4_K_M.gguf" ];
      };
      variants = recommended_variants_Ornith1_0_35B;
      ttl = 900;
    }
    {
      name = "Ornith-1.0-35B-Q5_K_M";
      path = "/models/deepreinforce-ai-Ornith-1.0-35B-GGUF/ornith-1.0-35b-Q5_K_M.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "deepreinforce-ai/Ornith-1.0-35B-GGUF/ornith-1.0-35b-Q5_K_M.gguf" ];
      };
      variants = recommended_variants_Ornith1_0_35B;
      ttl = 900;
    }
  ];

  amdModels = [
    {
      name = "Ornith-1.0-35B-Q8_0";
      path = "/models/deepreinforce-ai-Ornith-1.0-35B-GGUF/ornith-1.0-35b-Q8_0.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "deepreinforce-ai/Ornith-1.0-35B-GGUF/ornith-1.0-35b-Q8_0.gguf" ];
      };
      variants = recommended_variants_Ornith1_0_35B;
      ttl = 3600;
    }
    {
      name = "Ornith-1.0-35B-BF16";
      path = "/models/deepreinforce-ai-Ornith-1.0-35B-GGUF/ornith-1.0-35b-bf16.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "deepreinforce-ai/Ornith-1.0-35B-GGUF/ornith-1.0-35b-bf16.gguf" ];
      };
      variants = recommended_variants_Ornith1_0_35B;
      ttl = 3600;
    }
  ];
}
