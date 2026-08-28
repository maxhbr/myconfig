{
  modelsPullDir,
  sharpTemplate,
  forkPkg,
}:
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
  # Candidate profiles for the gfx1151 Vulkan DFlash2 and ROCm MTP/ngram
  # experiments (task items 4 & 5). These are opt-in: unique explicit names,
  # no production aliases, single-backend `devices`, and per-model options
  # (serverPackage / extraEnv / noMmap) that do NOT affect existing models.
  # They carry their own `devices` so the `gfx-llama-cpp-config.models` map
  # (which forces ["Vulkan0" "ROCm0"] on legacy models) must NOT override
  # them — they are appended AFTER that map in default.nix.
  #
  # Model SHA-256 values are the HuggingFace LFS oids (verified via the
  # /api/.../tree/main?blobs=true endpoint, 2026-08-27). They are logged in
  # the startup banner for provenance but NOT verified at runtime.
  candidateModels = [
    # --- Vulkan DFlash2 candidate (task item 4) ---------------------------
    # Approximates PieBru's balanced/coding configuration: Vulkan-only,
    # Nathanw1014 strix-halo-vulkan fork, Q6_K_XL target + DFlash2 Q8_0
    # draft, draft-dflash speculation (n-max 6), f16 KV, 131k context,
    # 4096/4096 batch, -t 16 -tb 32, mmap+mlock (noMmap=false, tested
    # against the repo's current --no-mmap), sharp.jinja tool template.
    {
      name = "Qwen3.8-27B-DFlash2-Q6_K_XL";
      path = "/models/unsloth-Qwen3.8-27B-GGUF/Qwen3.8-27B-UD-Q6_K_XL.gguf";
      devices = [ "Vulkan0" ];
      serverPackage = forkPkg;
      noMmap = false; # mmap+mlock (test against current --no-mmap)
      cacheType = "f16";
      ctxSize = 131072;
      parallel = 1;
      group = "Qwen3.8-27B";
      ttl = 1800;
      tags = [
        "candidate"
        "vulkan"
        "dflash2"
        "Q6_K_XL"
        "f16"
        "ctx131072"
        "fork-strix-halo"
      ];
      sha256 = "701d8fa9ed214ab21bfc130cd2a7df19ca89bbef7713e2dfb19f3c63696aa917";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [
          "unsloth/Qwen3.8-27B-GGUF/Qwen3.8-27B-UD-Q6_K_XL.gguf"
          "z-lab/Qwen3.8-27B-DFlash2-GGUF/Qwen3.8-27B-DFlash2-Q8_0.gguf"
        ];
      };
      params = [
        "--spec-type"
        "draft-dflash"
        "--spec-draft-model"
        "/models/z-lab-Qwen3.8-27B-DFlash2-GGUF/Qwen3.8-27B-DFlash2-Q8_0.gguf"
        "--spec-draft-n-max"
        "6"
        # Workaround for a fork bug: creating the DFlash2 draft context
        # aborts with `pre-allocated tensor (output.weight) in a buffer
        # (Vulkan*) that cannot run the operation (NONE)` (draft lm_head
        # is pre-allocated on the GPU but unused by the draft-dflash
        # graph). Keep the draft's output.weight off the Vulkan weights
        # buffer AND disable the op-offload / fused-op resolution path
        # that trips over it. See
        # doc/TODOs/fix-dflash2-fork-abort-draft-output-weight.md.
        # TODO(prune): once confirmed on the gfx1151 hardware, reduce to
        # the minimal working subset of these two flags.
        "--override-tensor-draft"
        "output.weight=CPU"
        "--no-op-offload"
        "--batch-size"
        "4096"
        "--ubatch-size"
        "4096"
        "-t"
        "16"
        "-tb"
        "32"
        "--jinja"
        "--chat-template"
        "${sharpTemplate}"
      ];
    }
    # --- ROCm MTP/ngram candidate (task item 5) ----------------------------
    # Based on KyaniteLabs' final profile: ROCm-only, upstream b10549,
    # Q4_K_XL target + mtp-Q8_0 draft, draft-mtp+ngram-mod speculation
    # (n-max 12, ngram n-min 24 / n-max 12), q4_0 KV, 1 slot, 262k context,
    # -t 16, flash-attn+jinja. Candidate-only HSA_ENABLE_SDMA=0 + HSA_XNACK=1
    # (NOT applied globally until the candidate passes stability tests).
    {
      name = "Qwen3.8-27B-MTP-ngram-Q4_K_XL";
      path = "/models/unsloth-Qwen3.8-27B-GGUF/Qwen3.8-27B-UD-Q4_K_XL.gguf";
      devices = [ "ROCm0" ];
      extraEnv = {
        HSA_ENABLE_SDMA = "0";
        HSA_XNACK = "1";
      };
      cacheType = "q4_0";
      ctxSize = 262144;
      parallel = 1;
      group = "Qwen3.8-27B";
      ttl = 1800;
      tags = [
        "candidate"
        "rocm"
        "mtp-ngram"
        "Q4_K_XL"
        "q4_0"
        "ctx262144"
      ];
      sha256 = "3f227079003add2511437e5b1e94812e363385225bf6a9b47b0054a72bc8b01e";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [
          "unsloth/Qwen3.8-27B-GGUF/Qwen3.8-27B-UD-Q4_K_XL.gguf"
          "ggml-org/Qwen3.8-27B-GGUF/mtp-Qwen3.8-27B-Q8_0.gguf"
        ];
      };
      params = [
        "--spec-type"
        "draft-mtp,ngram-mod"
        "--spec-draft-model"
        "/models/ggml-org-Qwen3.8-27B-GGUF/mtp-Qwen3.8-27B-Q8_0.gguf"
        "--spec-draft-n-max"
        "12"
        "--spec-ngram-mod-n-min"
        "24"
        "--spec-ngram-mod-n-max"
        "12"
        "--parallel"
        "1"
        "-t"
        "16"
        "--jinja"
      ];
    }
  ];

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
    # MTP (multi-token prediction) Q4_0. Same standalone-draft-head
    # situation as the Q8_0 MTP entry below (amdModels) — ggml-org
    # publishes only the MTP head, not the base weights:
    # https://huggingface.co/ggml-org/Qwen3.8-27B-GGUF/blob/main/mtp-Qwen3.8-27B-Q4_0.gguf
    # At Q4_0 the head is ~1.6 GB, so pairing it with the Q4_K_XL base
    # (~16.7 GB) fits comfortably in the RTX 5090's 32 GB budget,
    # unlike the Q8_0 head (~2.9 GB) which only fits paired with the
    # 29.3 GB Q8_0 base on the AMD host (see amdModels below).
    (
      baseModel "Q4_K_XL"
      // {
        name = "Qwen3.8-27B-MTP-Q4_0";
        cacheType = "q8_0";
        parallel = 1;
        # The MTP draft context allocates its own pp compute buffers,
        # dominated by the logits buffer (n_vocab ~151936 * f32 *
        # n_batch): 2048 * 151936 * 4 B ~= 1360 MiB — exactly the
        # `cudaMalloc failed` size observed. On top of the base weights
        # (~16.7 GB), the q8_0 KV cache at the inherited 262144 ctx and
        # the ~1.6 GB MTP head that overflows the RTX 5090's 32 GB and
        # llama-server aborts with `graph_reserve: failed to allocate
        # compute buffers` / `common_speculative_init_result: failed to
        # create MTP context`. So: halve the context (131072, mirroring
        # the working Qwen3.8-27B-Q6_K-MTP entry below) and drop the
        # batch size to the ubatch size (512), which shrinks the MTP
        # compute buffer to ~340 MiB.
        ctxSize = 131072;
        # mlock of the 1.44 GB MTP head fails with "Cannot allocate
        # memory" (same as the Q6_K MTP entry below).
        mlock = false;
        pull-models = {
          target_directory = modelsPullDir;
          hf_spec = [
            "unsloth/Qwen3.8-27B-GGUF/Qwen3.8-27B-UD-Q4_K_XL.gguf"
            "ggml-org/Qwen3.8-27B-GGUF/mtp-Qwen3.8-27B-Q4_0.gguf"
          ];
        };
        params = [
          "--batch-size"
          "512"
          "--ubatch-size"
          "512"
          "--spec-type"
          "draft-mtp"
          "--spec-draft-model"
          "/models/ggml-org-Qwen3.8-27B-GGUF/mtp-Qwen3.8-27B-Q4_0.gguf"
          "--spec-draft-n-max"
          "3"
        ];
        variants = recommended_variants_Qwen3_8-27B;
        ttl = 900;
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
    # Q8_0 (29.3 GB, single file — the same weights as the AMD-only
    # `Qwen3.8-27B-Q8_0` entry in amdModels) split across both GPUs.
    # Alone it does not fit the RTX 5090's 32 GB at full context (see
    # amdModels). Unlike the BF16-split entry above ("1,2"), the Q8_0
    # entry deliberately inverts the ratio ("2,1"): the model is small
    # enough that the larger 2/3 share still fits on the RTX 5090 side —
    # ~19.5 GB of weights plus the q8_0 KV cache at 262144 ctx on
    # Vulkan0, ~9.8 GB on Vulkan1 (gfx1151, the KV cache is split in
    # the same ratio). The base GGUF is also declared by the
    # `Qwen3.8-27B-Q8_0` entry in amdModels; the merged pull list in
    # `default.nix` is de-duplicated, so this does not cause a double
    # download.
    # No --no-mmap:
    # unlike the BF16-split entry this is a single-file GGUF (the
    # single-file multi-GPU precedent, Qwen3.6-35B-A3B-BF16-MTP-split,
    # omits it as well).
    {
      name = "Qwen3.8-27B-Q8_0-split";
      path = "/models/unsloth-Qwen3.8-27B-GGUF/Qwen3.8-27B-Q8_0.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.8-27B-GGUF/Qwen3.8-27B-Q8_0.gguf" ];
      };
      devices = [ "Vulkan0,Vulkan1" ];
      tensorSplit = "2,1";
      params = [
        "--chat-template-kwargs"
        "{\"preserve_thinking\":true}"
      ];
      ctxSize = 262144;
      parallel = 1;
      cacheType = "q8_0";
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
    # MTP (multi-token prediction) Q8_0. Unlike the unsloth Qwen3.6
    # `*-MTP-GGUF` repos (a single self-speculative GGUF bundling base
    # weights + MTP head, so `-m` alone suffices), ggml-org publishes
    # the MTP head for Qwen3.8-27B as a STANDALONE file — `mtp-` prefix,
    # ~18 tensors (`eh_proj`, `enorm`, `hnorm`, `shared_head_norm`, MTP
    # block), base weights NOT included:
    # https://huggingface.co/ggml-org/Qwen3.8-27B-GGUF/blob/main/mtp-Qwen3.8-27B-Q8_0.gguf
    # It must be loaded as the *draft* model (`--spec-draft-model`,
    # a.k.a. `-md`/`--model-draft`) alongside the ordinary Q8_0 base
    # weights loaded via `-m`; llama.cpp's `--spec-type draft-mtp` then
    # reads the MTP tensors from the draft file. This mirrors upstream
    # llama.cpp's own `--mtp` auto-download fallback (see `arg.cpp`,
    # which sets `params.speculative.draft.mparams.path` to the
    # resolved MTP head file).
    {
      name = "Qwen3.8-27B-MTP-Q8_0";
      path = "/models/unsloth-Qwen3.8-27B-GGUF/Qwen3.8-27B-Q8_0.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [
          "unsloth/Qwen3.8-27B-GGUF/Qwen3.8-27B-Q8_0.gguf"
          "ggml-org/Qwen3.8-27B-GGUF/mtp-Qwen3.8-27B-Q8_0.gguf"
        ];
      };
      params = [
        "--spec-type"
        "draft-mtp"
        "--spec-draft-model"
        "/models/ggml-org-Qwen3.8-27B-GGUF/mtp-Qwen3.8-27B-Q8_0.gguf"
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
