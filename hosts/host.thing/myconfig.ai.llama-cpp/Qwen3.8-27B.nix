{ modelsPullDir }:
let
  # Best-practice sampling parameters from the unsloth Qwen3.8-27B GGUF
  # README (https://huggingface.co/unsloth/Qwen3.8-27B-GGUF).
  #
  # Thinking mode: temperature=1.0, top_p=0.95, top_k=20, min_p=0.0,
  #   presence_penalty=0.0, repetition_penalty=1.0
  # Instruct mode:  temperature=0.7, top_p=0.80, top_k=20, min_p=0.0,
  #   presence_penalty=1.5, repetition_penalty=1.0
  #
  # repetition_penalty=1.0 is the llama-cpp default, so it is omitted.
  recommended_variants_Qwen3_8-27B = {
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
        "0.0"
        "--chat-template-kwargs"
        "{\"enable_thinking\":true,\"preserve_thinking\":false}"
      ];
    };
    precise-coding-tasks = {
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
        "0.0"
        "--chat-template-kwargs"
        "{\"enable_thinking\":true,\"preserve_thinking\":true}"
      ];
    };
    instruct-general-tasks = {
      params = [
        "--temp"
        "0.7"
        "--top-p"
        "0.80"
        "--top-k"
        "20"
        "--min-p"
        "0.00"
        "--presence-penalty"
        "1.5"
        "--chat-template-kwargs"
        "{\"enable_thinking\":false}"
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
      ];
    };
  };

  # Shared base attributes for all UD quantisations of Qwen3.8-27B.
  baseModel = quant: {
    name = "Qwen3.8-27B-UD-${quant}";
    path = "/models/unsloth-Qwen3.8-27B-GGUF/Qwen3.8-27B-UD-${quant}.gguf";
    pull-models = {
      target_directory = modelsPullDir;
      hf_spec = [ "unsloth/Qwen3.8-27B-GGUF/Qwen3.8-27B-UD-${quant}.gguf" ];
    };
    ctxSize = 262144;
    ttl = 1800;
  };

  # Blackfrost abliterated build of Qwen3.8-27B (same qwen35
  # architecture, refusal surface reduced at weight level). Standard
  # K-quant ladder, no UD/IQ variants.
  # https://huggingface.co/Blackfrost-AI/Qwen3.8-27B-ABLITERATED-GGUF
  baseModelAbliterated = quant: {
    name = "Qwen3.8-27B-ABL-${quant}";
    path = "/models/Blackfrost-AI-Qwen3.8-27B-ABLITERATED-GGUF/Qwen3.8-27B-ABLITERATED-${quant}.gguf";
    pull-models = {
      target_directory = modelsPullDir;
      hf_spec = [ "Blackfrost-AI/Qwen3.8-27B-ABLITERATED-GGUF/Qwen3.8-27B-ABLITERATED-${quant}.gguf" ];
    };
    ctxSize = 262144;
    ttl = 1800;
  };
in
{
  # Q4/Q5/Q6 fit on the RTX 5090 (32 GB). These are served as rtxModels
  # and must NOT overlap with amdModels — a model name present in both
  # lists produces two `llama-server_Vulkan1_*` wrappers with different
  # store paths, which breaks `pkgs.buildEnv` in home-manager.
  rtxModels = [
    (
      baseModel "Q4_K_XL"
      // {
        cacheType = "q8_0";
        parallel = 1;
        params = [
          "--batch-size"
          "2048"
          "--ubatch-size"
          "512"
        ];
        variants = recommended_variants_Qwen3_8-27B;
      }
    )
    (
      baseModel "Q5_K_XL"
      // {
        cacheType = "q8_0";
        parallel = 1;
        params = [
          "--batch-size"
          "2048"
          "--ubatch-size"
          "512"
        ];
        variants = recommended_variants_Qwen3_8-27B;
      }
    )
    (
      baseModel "Q6_K_XL"
      // {
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
        ctxSize = 196608; # slightly reduced to fit in 32 GB VRAM
        variants = recommended_variants_Qwen3_8-27B;
      }
    )
    # Blackfrost abliterated Q5_K_M (19.5 GB) — same qwen35
    # architecture as the unsloth build, so it reuses the recommended
    # sampling variants. It carries an embedded Blackfrost
    # short-execution prompt in the chat template; `--jinja` is
    # required to apply it (set below).
    (
      baseModelAbliterated "Q5_K_M"
      // {
        cacheType = "q8_0";
        parallel = 1;
        params = [
          "--batch-size"
          "2048"
          "--ubatch-size"
          "512"
          "--jinja"
        ];
        variants = recommended_variants_Qwen3_8-27B;
      }
    )
  ];

  # Q8 (29.3 GB) is too large for the RTX 5090, so it is AMD-only.
  # On the host it appears only in scriptOnlyModels (Vulkan1), never in
  # rtxModels, so no buildEnv name collision can occur.
  amdModels = [
    (
      baseModel "Q8_K_XL"
      // {
        variants = recommended_variants_Qwen3_8-27B;
      }
    )
  ];
}
