let
  modelsPullDir = "/home/mhuber/models";
in
{
  rtxModels = [
    {
      name = "gemma-4-31B-it-UD-Q5_K_XL";
      path = "/models/gemma-4-31B-it-GGUF/gemma-4-31B-it-UD-Q5_K_XL.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/gemma-4-31B-it-GGUF/gemma-4-31B-it-UD-Q5_K_XL.gguf" ];
      };
      params = [
        "--batch-size"
        "2048"
        "--ubatch-size"
        "512"
        "--threads"
        "1"
        "--jinja"
        "--temp"
        "1.0"
        "--top-p"
        "0.95"
        "--top-k"
        "64"
      ];
      aliases = [
        "gemma-4-31B-Q5"
        "gemma-4-dense"
      ];
      ttl = 300;
    }
    {
      name = "gemma-4-31B-it-UD-Q4_K_XL";
      path = "/models/gemma-4-31B-it-GGUF/gemma-4-31B-it-UD-Q4_K_XL.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [
          "unsloth/gemma-4-31B-it-GGUF/gemma-4-31B-it-UD-Q4_K_XL.gguf"
          # mmproj sidecars used by `variants.mmproj.mmproj` below.
          "unsloth/gemma-4-31B-it-GGUF/mmproj-F16.gguf"
          "unsloth/gemma-4-31B-it-GGUF/mmproj-BF16.gguf"
          "unsloth/gemma-4-31B-it-GGUF/mmproj-F32.gguf"
        ];
      };
      ctxSize = 65536;
      params = [
        "--batch-size"
        "2048"
        "--ubatch-size"
        "512"
        "--threads"
        "1"
        "--jinja"
        "--temp"
        "1.0"
        "--top-p"
        "0.95"
        "--top-k"
        "64"
      ];
      variants = {
        mmproj = {
          mmproj = "/models/gemma-4-31B-it-GGUF/mmproj-F16.gguf";
        };
        nothink = {
          params = [
            "--chat-template-kwargs"
            "{\"enable_thinking\":false}"
          ];
        };
      };
      aliases = [
        "gemma-4-31B-Q4"
      ];
      ttl = 300;
    }
    {
      name = "gemma-4-26B-A4B-it-UD-Q6_K_XL";
      path = "/models/gemma-4-26B-A4B-it-GGUF/gemma-4-26B-A4B-it-UD-Q6_K_XL.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/gemma-4-26B-A4B-it-GGUF/gemma-4-26B-A4B-it-UD-Q6_K_XL.gguf" ];
      };
      params = [
        "--batch-size"
        "2048"
        "--ubatch-size"
        "512"
        "--threads"
        "1"
        "--jinja"
        "--temp"
        "1.0"
        "--top-p"
        "0.95"
        "--top-k"
        "64"
      ];
      ctxSize = 262144;
      aliases = [
        "gemma-4-26B-Q6"
        "gemma-4-MoE"
      ];
      ttl = 300;
    }
    {
      name = "gemma-4-26B-A4B-it-UD-Q8_K_XL";
      path = "/models/gemma-4-26B-A4B-it-GGUF/gemma-4-26B-A4B-it-UD-Q8_K_XL.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/gemma-4-26B-A4B-it-GGUF/gemma-4-26B-A4B-it-UD-Q8_K_XL.gguf" ];
      };
      params = [
        "--batch-size"
        "2048"
        "--ubatch-size"
        "512"
        "--threads"
        "1"
        "--jinja"
        "--temp"
        "1.0"
        "--top-p"
        "0.95"
        "--top-k"
        "64"
      ];
      aliases = [
        "gemma-4-26B-Q8"
      ];
      ttl = 300;
    }

  ];

  amdModels = [
    {
      name = "gemma-4-31B-it-BF16";
      path = "/models/gemma-4-31B-it-GGUF/BF16/gemma-4-31B-it-BF16-00001-of-00002.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/gemma-4-31B-it-GGUF/BF16" ];
      };
      params = [
        "-ctk"
        "f16"
        "-ctv"
        "f16"
      ];
      aliases = [
        "gemma-4-31B"
        "gemma-4-31B-BF16"
      ];
      ttl = 300;
    }
  ];
}
