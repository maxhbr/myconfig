let
  modelsPullDir = "/home/mhuber/models";
  recommended_variants_Qwen3_6-27B = {
    general-tasks = {
      params = [
        "--temp"
        "1.0"
        "--top-p"
        "0.95"
        "--top-k"
        "20"
        "--min-p"
        "0.00"
        "--presence-penalty"
        "1.5"
        "--chat-template-kwargs"
        "{\"enable_thinking\":true,\"preserve_thinking\":false}"
        # TODO: repetition_penalty=1.0
      ];
    };
    precise-coding-tasks = {
      params = [
        "--temp"
        "0.6"
        "--top-p"
        "0.95"
        "--top-k"
        "20"
        "--min-p"
        "0.00"
        "--presence-penalty"
        "0.0"
        "--chat-template-kwargs"
        "{\"enable_thinking\":true,\"preserve_thinking\":true}"
        # TODO: repetition_penalty=1.0
      ];
    };
    instruct-general-tasks = {
      params = [
        "--temp"
        "0.7"
        "--top-p"
        "0.8"
        "--top-k"
        "20"
        "--min-p"
        "0.00"
        "--presence-penalty"
        "1.5"
        "--chat-template-kwargs"
        "{\"enable_thinking\":false}"
        # TODO: repetition_penalty=1.0
      ];
    };
    instruct-reasoning-tasks = {
      params = [
        "--temp"
        "1.0"
        "--top-p"
        "0.95"
        "--top-k"
        "20"
        "--min-p"
        "0.00"
        "--presence-penalty"
        "1.5"
        "--chat-template-kwargs"
        "{\"enable_thinking\":false}"
        # TODO: repetition_penalty=1.0
      ];
    };
  };
in
{
  rtxModels = [
    {
      name = "Qwen3.6-27B-UD-Q4_K_XL";
      path = "/models/unsloth-Qwen3.6-27B-GGUF/Qwen3.6-27B-UD-Q4_K_XL.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.6-27B-GGUF/Qwen3.6-27B-UD-Q4_K_XL.gguf" ];
      };
      params = [
        "--chat-template-kwargs"
        "{\"preserve_thinking\":true}"
      ];
      aliases = [ ];
      ttl = 900;
    }
    {
      name = "Qwen3.6-27B-UD-Q5_K_XL";
      path = "/models/unsloth-Qwen3.6-27B-GGUF/Qwen3.6-27B-UD-Q5_K_XL.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.6-27B-GGUF/Qwen3.6-27B-UD-Q5_K_XL.gguf" ];
      };
      cacheType = "q8_0";
      parallel = 1;
      params = [
        "--batch-size"
        "2048"
        "--ubatch-size"
        "512"
      ];
      ctxSize = 262144;
      ttl = 900;
      variants = recommended_variants_Qwen3_6-27B // {
        general-tasks = {
          aliases = [ ];
        };
        precise-coding-tasks = {
          aliases = [
            "opencode-fast"
          ];
        };
      };
    }
    {
      name = "Qwen3.6-27B-UD-Q6_K_XL";
      path = "/models/unsloth-Qwen3.6-27B-GGUF/Qwen3.6-27B-UD-Q6_K_XL.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.6-27B-GGUF/Qwen3.6-27B-UD-Q6_K_XL.gguf" ];
      };
      cacheType = "q8_0";
      parallel = 1;
      params = [
        "--batch-size"
        "2048"
        "--ubatch-size"
        "512"
        "--chat-template-kwargs"
        "{\"preserve_thinking\":true}"
      ];
      ctxSize = 196608;
      aliases = [ ];
      ttl = 900;
      variants = recommended_variants_Qwen3_6-27B;
    }
    {
      name = "Qwen3.6-27B-Q6_K-MTP";
      path = "/models/unsloth-Qwen3.6-27B-MTP-GGUF/Qwen3.6-27B-Q6_K.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.6-27B-MTP-GGUF/Qwen3.6-27B-Q6_K.gguf" ];
      };
      params = [
        "--spec-type"
        "draft-mtp"
        "--spec-draft-n-max"
        "2"
      ];
      cacheType = "q8_0";
      ctxSize = 184320;
      aliases = [
        "opencode"
      ];
      parallel = 1;
      ttl = 900;
    }
  ];

  multiGpuModels = [
    {
      name = "Qwen3.6-27B-BF16-split";
      path = "/models/unsloth-Qwen3.6-27B-GGUF/BF16/Qwen3.6-27B-BF16-00001-of-00002.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.6-27B-GGUF/BF16" ];
      };
      devices = [ "Vulkan0,Vulkan1" ];
      tensorSplit = "1,2";
      params = [
        "--chat-template-kwargs"
        "{\"preserve_thinking\":true}"
        "--no-mmap"
      ];
      ctxSize = 262144;
      parallel = 1;
      cacheType = "f16";
      ttl = 3600;
    }
  ];

  amdModels = [
    {
      name = "Qwen3.6-27B-MTP-Q8_0";
      path = "/models/ggml-org-Qwen3.6-27B-MTP-GGUF/Qwen3.6-27B-MTP-Q8_0.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "ggml-org/Qwen3.6-27B-MTP-GGUF/Qwen3.6-27B-MTP-Q8_0.gguf" ];
      };
      params = [
        "--spec-type"
        "draft-mtp"
        "--spec-draft-n-max"
        "3"
      ];
      aliases = [ ];
      ttl = 900;
    }
    {
      name = "Qwen3.6-27B-Q8_0";
      path = "/models/unsloth-Qwen3.6-27B-GGUF/Qwen3.6-27B-Q8_0.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.6-27B-GGUF/Qwen3.6-27B-Q8_0.gguf" ];
      };
      params = [
        "--chat-template-kwargs"
        "{\"preserve_thinking\":true}"
      ];
      aliases = [ ];
      ttl = 900;
    }
    {
      name = "Qwen3.6-27B-BF16";
      path = "/models/unsloth-Qwen3.6-27B-GGUF/BF16/Qwen3.6-27B-BF16-00001-of-00002.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.6-27B-GGUF/BF16" ];
      };
      params = [
        "--chat-template-kwargs"
        "{\"preserve_thinking\":true}"
      ];
      cacheType = "f16";
      aliases = [
        "opencode-slow"
      ];
      ttl = 300;
    }
  ];
}
