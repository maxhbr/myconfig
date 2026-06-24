let
  modelsPullDir = "/home/mhuber/models";
  recommended_variants_Qwen3_6-35B-A3B = {
    thinking-general = {
      params = [
        "--temp"
        "1.0"
        "--top-p"
        "0.95"
        "--top-k"
        "20"
        "--min-p"
        "0.0"
        "--presence-penalty"
        "1.5"
        # "--repetition-penalty"
        # "1.0"
      ];
    };
    thinking-coding = {
      params = [
        "--temp"
        "0.6"
        "--top-p"
        "0.95"
        "--top-k"
        "20"
        "--min-p"
        "0.0"
        "--presence-penalty"
        "0.0"
        # "--repetition-penalty"
        # "1.0"
      ];
    };
    instruct-general = {
      params = [
        "--temp"
        "0.7"
        "--top-p"
        "0.8"
        "--top-k"
        "20"
        "--min-p"
        "0.0"
        "--presence-penalty"
        "1.5"
        # "--repetition-penalty"
        # "1.0"
      ];
    };
    instruct-reasoning = {
      params = [
        "--temp"
        "1.0"
        "--top-p"
        "1.0"
        "--top-k"
        "40"
        "--min-p"
        "0.0"
        "--presence-penalty"
        "2.0"
        # "--repetition-penalty"
        # "1.0"
      ];
    };
  };
in
{
  rtxModels = [
    {
      name = "Qwen3.6-35B-A3B-UD-IQ1_M";
      path = "/models/unsloth-Qwen3.6-35B-A3B-GGUF/Qwen3.6-35B-A3B-UD-IQ1_M.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.6-35B-A3B-GGUF/Qwen3.6-35B-A3B-UD-IQ1_M.gguf" ];
      };
      ttl = 900;
    }
    {
      name = "Qwen3.6-35B-A3B-UD-Q2_K_XL";
      path = "/models/unsloth-Qwen3.6-35B-A3B-GGUF/Qwen3.6-35B-A3B-UD-Q2_K_XL.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.6-35B-A3B-GGUF/Qwen3.6-35B-A3B-UD-Q2_K_XL.gguf" ];
      };
      ttl = 900;
    }
    {
      name = "Qwen3.6-35B-A3B-UD-Q3_K_XL";
      path = "/models/unsloth-Qwen3.6-35B-A3B-GGUF/Qwen3.6-35B-A3B-UD-Q3_K_XL.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.6-35B-A3B-GGUF/Qwen3.6-35B-A3B-UD-Q3_K_XL.gguf" ];
      };
      ttl = 900;
    }
    {
      name = "Qwen3.6-35B-A3B-UD-Q5_K_XL";
      path = "/models/unsloth-Qwen3.6-35B-A3B-GGUF/Qwen3.6-35B-A3B-UD-Q5_K_XL.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.6-35B-A3B-GGUF/Qwen3.6-35B-A3B-UD-Q5_K_XL.gguf" ];
      };
      variants = recommended_variants_Qwen3_6-35B-A3B // {
        tweaked = {
          # https://github.com/nathanlgabriel/paper_code_mapping_assessment/blob/main/README.md
          cacheType = "q8_0";
          params = [
            "--split-mode"
            "layer"
            "--tensor-split"
            "1,1.12"
            "--parallel"
            "1"
          ];
          ctxSize = 262144;
        };
      };
      aliases = [
        "Qwen3.6-35B-A3B-UD-Q5_K"
        "Qwen3.6-35B-A3B-UD-Q5"
        "Qwen3.6-35B-A3B-UD"
        "Qwen3.6-35B-A3B"
        "Qwen3.6-35B"
        "hermes"
        "opencode-fast-fallback"
      ];
      ttl = 900;
    }
  ];

  amdModels = [
    {
      name = "Qwen3.6-35B-A3B-Q8_0";
      path = "/models/unsloth-Qwen3.6-35B-A3B-GGUF/Qwen3.6-35B-A3B-Q8_0.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.6-35B-A3B-GGUF/Qwen3.6-35B-A3B-Q8_0.gguf" ];
      };
      params = [
        "--chat-template-kwargs"
        "{\"preserve_thinking\":true}"
      ];
      ctxSize = 262144;
      parallel = 4;
      variants = recommended_variants_Qwen3_6-35B-A3B;
      ttl = 3600;
    }
    {
      name = "Qwen3.6-35B-A3B-BF16";
      path = "/models/unsloth-Qwen3.6-35B-A3B-GGUF/BF16/Qwen3.6-35B-A3B-BF16-00001-of-00002.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.6-35B-A3B-GGUF/BF16" ];
      };
      params = [
        "-ctk"
        "f16"
        "-ctv"
        "f16"
        "--chat-template-kwargs"
        "{\"preserve_thinking\":true}"
      ];
      ctxSize = 262144;
      parallel = 4;
      variants = recommended_variants_Qwen3_6-35B-A3B;
      ttl = 3600;
    }

    {
      name = "Qwen3.6-35B-A3B-MTP-BF16";
      path = "/models/ggml-org-Qwen3.6-35B-A3B-MTP-GGUF/Qwen3.6-35B-A3B-MTP-BF16.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "ggml-org/Qwen3.6-35B-A3B-MTP-GGUF/Qwen3.6-35B-A3B-MTP-BF16.gguf" ];
      };
      params = [
        "--chat-template-kwargs"
        "{\"preserve_thinking\":true}"
        "--spec-type"
        "draft-mtp"
        "--spec-draft-n-max"
        "3"
      ];
      ctxSize = 262144;
      parallel = 4;
      variants = recommended_variants_Qwen3_6-35B-A3B;
      ttl = 3600;
    }
  ];

  multiGpuModels = [
    {
      name = "Qwen3.6-35B-A3B-BF16-split";
      path = "/models/unsloth-Qwen3.6-35B-A3B-GGUF/BF16/Qwen3.6-35B-A3B-BF16-00001-of-00002.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.6-35B-A3B-GGUF/BF16" ];
      };
      devices = [ "Vulkan0,Vulkan1" ];
      tensorSplit = "2,3";
      params = [
        "--chat-template-kwargs"
        "{\"preserve_thinking\":true}"
      ];
      ctxSize = 262144;
      cacheType = "f16";
      parallel = 1;
      aliases = [
        "Qwen3.6-35B-A3B-BF16-multigpu"
      ];
      variants = recommended_variants_Qwen3_6-35B-A3B;
      ttl = 3600;
    }
    {
      name = "Qwen3.6-35B-A3B-BF16-MTP-split";
      path = "/models/ggml-org-Qwen3.6-35B-A3B-MTP-GGUF/Qwen3.6-35B-A3B-MTP-BF16.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "ggml-org/Qwen3.6-35B-A3B-MTP-GGUF/Qwen3.6-35B-A3B-MTP-BF16.gguf" ];
      };
      devices = [ "Vulkan0,Vulkan1" ];
      tensorSplit = "3,4";
      params = [
        "--chat-template-kwargs"
        "{\"preserve_thinking\":true}"
        # "--parallel" "1"
        "--spec-type" "draft-mtp" "--spec-draft-n-max" "2"
      ];
      ctxSize = 262144;
      cacheType = "f16";
      parallel = 1;
      aliases = [
        "Qwen3.6-35B-A3B-BF16-multigpu"
      ];
      variants = recommended_variants_Qwen3_6-35B-A3B;
      ttl = 3600;
    }
  ];
}
