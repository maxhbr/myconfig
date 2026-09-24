{ modelsPullDir }:
{
  amdModels = [
    {
      # Sparse MoE, 309B total / 15B activated, hybrid SWA, 1M-token
      # training context. Loads on the stock llama.cpp build
      # (mimo2 architecture).
      name = "MiMo-V2.6-Flash-RL-IQ2_S";
      path = "/models/AesSedai-MiMo-V2.6-Flash-GGUF/IQ2_S/MiMo-V2.6-Flash-RL-IQ2_S-00001-of-00004.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "AesSedai/MiMo-V2.6-Flash-GGUF/IQ2_S" ];
      };
      params = [
        # Xiaomi-recommended sampling for MiMo-V2.6-Flash-RL.
        "--temp"
        "1.0"
        "--top-p"
        "0.95"
      ];
      ctxSize = 262144;
      cacheType = "q8_0";
      ttl = 1500;
      variants = {
        # DRY sampler on top of the Xiaomi-recommended sampling: same
        # temp/top-p, flat top-k/min-p/repeat-penalty, with DRY
        # repetition breaking. ctxSize 262144 and the q8_0 KV/
        # spec-draft cache types are inherited from the base entry
        # (cacheType expands to --cache-type-k/v and
        # --spec-draft-type-k/v).
        dry = {
          params = [
            "--top-k"
            "0"
            "--min-p"
            "0.0"
            "--repeat-penalty"
            "1.0"
            "--dry-multiplier"
            "0.5"
            "--dry-base"
            "1.75"
            "--dry-allowed-length"
            "4"
            "--dry-penalty-last-n"
            "4096"
            "--dry-sequence-breaker"
            "none"
          ];
        };
      };
    }
  ];
}
