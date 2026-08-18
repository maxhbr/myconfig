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
    # MTP (multi-token prediction) Q6_K — a single self-speculative
    # GGUF that bundles the base weights and the MTP draft head, so no
    # separate draft model is needed (`--spec-type draft-mtp` reads the
    # MTP tensors straight out of this file). unsloth/ggml-org have not
    # (yet) published an official Qwen3.8-27B MTP GGUF at Q6_K
    # granularity; this is the highest-trust community mirror that
    # does (17k+ downloads, matches upstream file-naming conventions).
    # https://huggingface.co/Jackrong/Qwen3.8-27B-MTP-GGUF/blob/main/Qwen3.8-27B-MTP-Q6_K.gguf
    # sha256 (lfs oid): d0fa94af270f3de426965f1f9f6f6140aae9f5aad9b88bb466452492748d526e
    {
      name = "Qwen3.8-27B-Q6_K-MTP";
      path = "/models/Jackrong-Qwen3.8-27B-MTP-GGUF/Qwen3.8-27B-MTP-Q6_K.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "Jackrong/Qwen3.8-27B-MTP-GGUF/Qwen3.8-27B-MTP-Q6_K.gguf" ];
      };
      mlock = false; # mlock fails with "Cannot allocate memory" on this model
      params = [
        "--spec-type"
        "draft-mtp"
        "--spec-draft-n-max"
        "2"
        "-b"
        "2048"
        "-ub"
        "2048"
      ];
      cacheType = "q8_0";
      ctxSize = 131072; # ~128k context — fits in RTX 5090 24 GB with Q6_K model (~17 GB)
      aliases = [
        "opencode"
      ];
      parallel = 1;
      ttl = 900;
      variants = {
        full-ctx = {
          ctxSize = 184320; # full context with quantized (q8_0) cache
          tags = [ "long-context" ];
        };
      };
    }
    # NOTE: the Qwen3.6-27B-NVFP4 quantisation previously served here
    # (tngtech/Qwen3.6-27B-NVFP4-GGUF) has no Qwen3.8-27B successor as
    # of this migration — `tngtech` has not published an NVFP4 GGUF for
    # Qwen3.8-27B (checked via
    # `curl https://huggingface.co/api/models?author=tngtech`, 2026-08).
    # Re-add once upstream publishes one; do not substitute a low-trust
    # community mirror for an NVIDIA-quantisation-format model.
  ];

  multiGpuModels = [
    {
      name = "Qwen3.8-27B-BF16-split";
      path = "/models/unsloth-Qwen3.8-27B-GGUF/BF16/Qwen3.8-27B-BF16-00001-of-00002.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.8-27B-GGUF/BF16" ];
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
    # MTP (multi-token prediction) Q8_0. ggml-org publishes the MTP
    # draft head bundled with the base weights directly in the main
    # GGUF repo (file prefixed `mtp-` rather than the unsloth/`ggml-org`
    # Qwen3.6 convention of a dedicated `*-MTP-GGUF` repo suffixed
    # `MTP-Q8_0`) — see the repo tree:
    # https://huggingface.co/ggml-org/Qwen3.8-27B-GGUF/blob/main/mtp-Qwen3.8-27B-Q8_0.gguf
    {
      name = "Qwen3.8-27B-MTP-Q8_0";
      path = "/models/ggml-org-Qwen3.8-27B-GGUF/mtp-Qwen3.8-27B-Q8_0.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "ggml-org/Qwen3.8-27B-GGUF/mtp-Qwen3.8-27B-Q8_0.gguf" ];
      };
      params = [
        "--spec-type"
        "draft-mtp"
        "--spec-draft-n-max"
        "3"
      ];
      variants = recommended_variants_Qwen3_8-27B // {
        precise-coding-tasks = {
          aliases = [
            "opencode"
          ];
        };
        q8_0 = {
          cacheType = "q8_0";
        };
      };
      ctxSize = 262144;
      ttl = 900;
    }
    {
      name = "Qwen3.8-27B-Q8_0";
      path = "/models/unsloth-Qwen3.8-27B-GGUF/Qwen3.8-27B-Q8_0.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.8-27B-GGUF/Qwen3.8-27B-Q8_0.gguf" ];
      };
      params = [
        "--chat-template-kwargs"
        "{\"preserve_thinking\":true}"
      ];
      variants = recommended_variants_Qwen3_8-27B // {
        precise-coding-tasks = {
          aliases = [
            "opencode-fallback"
          ];
        };
        q8_0 = {
          cacheType = "q8_0";
        };
      };
      ctxSize = 262144;
      ttl = 1800;
    }
  ];
}
