# Profile selection, deep-context limits, and rollback

## Current production profile (unchanged, rollback target)

The existing Q8_0 / MTP profile on the gfx1151 container remains the
production route. Its aliases are untouched:

| Alias | Model | Backend | Notes |
|-------|-------|---------|-------|
| `opencode` | Qwen3.8-27B-MTP-Q8_0 (precise-coding-tasks variant) | Vulkan0 + ROCm0 | Q8_0 target, mtp-Q8_0 draft, draft-mtp n-max 3, q8_0 KV, 262k ctx |
| `opencode-fallback` | Qwen3.8-27B-Q8_0 (precise-coding-tasks variant) | Vulkan0 + ROCm0 | Q8_0 target, no speculation, 262k ctx |

These models serve on **both** Vulkan0 and ROCm0 (the `gfx-llama-cpp-config`
map forces `devices = ["Vulkan0" "ROCm0"]` on legacy models). Vulkan0 is
listed first, so unprefixed names select Vulkan.

### Rollback

The production profile is retained verbatim. To roll back, no code
change is needed — the candidates are opt-in (unique names, no aliases)
and the version bump is a forward-only overlay change. If a candidate
causes problems, stop it via llama-swap (it swaps out automatically) and
the production profile continues serving.

To revert the llama.cpp version bump (if b10549 proves unstable on the
hardware), change `version` and `hash` in
`hosts/host.thing/nixpkgs.overlays.llama-cpp.nix` back to b10056 and
rebuild. The version-drift assertion will still pass (all backends
move together).

## Candidate profiles

Both candidates are **opt-in**: unique explicit names, no production
aliases, single-backend `devices`, and per-model options scoped to them
only. They are appended to `gfx-llama-cpp-config.models` AFTER the
`[Vulkan0 ROCm0]`-forcing map, so they keep their own single-backend
device lists.

### Vulkan DFlash2 candidate

| Field | Value |
|-------|-------|
| Name | `Qwen3.8-27B-DFlash2-Q6_K_XL` |
| Backend | Vulkan0 only |
| Engine | Nathanw1014 strix-halo-vulkan fork (`llama-cpp-strix-halo`) |
| Target | `Qwen3.8-27B-UD-Q6_K_XL.gguf` (25.3 GB) |
| Draft | `z-lab/Qwen3.8-27B-DFlash2-GGUF/Qwen3.8-27B-DFlash2-Q8_0.gguf` (2.0 GB) |
| Speculation | `draft-dflash`, `spec-draft-n-max = 6` |
| Workaround | `--override-tensor-draft output.weight=CPU` + `--no-op-offload` — the fork aborts creating the DFlash2 draft context (`pre-allocated tensor (output.weight) ... cannot run the operation (NONE)`); see `doc/TODOs/fix-dflash2-fork-abort-draft-output-weight.md` |
| KV cache | f16 (target + draft) |
| Context | 131,072 tokens |
| Batch / microbatch | 4096 / 4096 |
| Threads | `-t 16 -tb 32` |
| GPU offload | all layers |
| Flash attention | on (script default) |
| Template | `--jinja --chat-template <sharp.jinja store path>` |
| mmap | mmap+mlock (`noMmap = false`) — tested against the repo's current `--no-mmap` |
| Penalties | neutral (none added; benchmark profile) |
| Tags | candidate, vulkan, dflash2, Q6_K_XL, f16, ctx131072, fork-strix-halo |

### ROCm MTP/ngram candidate

| Field | Value |
|-------|-------|
| Name | `Qwen3.8-27B-MTP-ngram-Q4_K_XL` |
| Backend | ROCm0 only |
| Engine | upstream b10549 (`serverPackage = null` → device-default) |
| Target | `Qwen3.8-27B-UD-Q4_K_XL.gguf` (17.6 GB) |
| Draft | `ggml-org/Qwen3.8-27B-GGUF/mtp-Qwen3.8-27B-Q8_0.gguf` (3.1 GB) |
| Speculation | `draft-mtp,ngram-mod`, `spec-draft-n-max = 12`, `spec-ngram-mod-n-min = 24`, `spec-ngram-mod-n-max = 12` |
| KV cache | q4_0 (K + V) |
| Parallel slots | 1 (`--parallel 1`, explicit) |
| Context | 262,144 tokens (subject to full-depth test) |
| Threads | `-t 16` |
| Flash attention | on (script default) |
| Jinja | `--jinja` (embedded template, not sharp.jinja) |
| Env (candidate-only) | `HSA_ENABLE_SDMA=0`, `HSA_XNACK=1` (via `extraEnv`, NOT global) |
| Tags | candidate, rocm, mtp-ngram, Q4_K_XL, q4_0, ctx262144 |

**Warning (from KyaniteLabs):** the very high warm/replay rates reported by
KyaniteLabs are repetition-assisted ngram results, not representative chat
throughput. The benchmark harness must report cold, warm/replay, novel
prose, code, and end-to-end task latency separately. Tokens/second alone is
insufficient — use end-to-end wall time per correct completed task as the
primary decision metric.

## Deep-context limits (task item 6)

| Backend | Default cap | Reason | Override |
|---------|-------------|--------|----------|
| Vulkan | 131,072 | The kernel's GPU lockup watchdog can falsely trigger during long Vulkan fills on gfx1151, resetting the GPU. | `amdgpu.lockup_timeout=-1` via the opt-in `amdgpu-no-lockup-timeout` NixOS specialisation (NOT default; requires reboot) |
| ROCm | 262,144 | ROCm is not affected by the false lockup trigger. Keep 262k on the default kernel config. | None needed |

The `amdgpu.lockup_timeout=-1` specialisation:
- Is **opt-in** — the default boot config keeps the standard lockup watchdog.
- Is **documented** with a GPU canary (`rocm-smi` + `vulkaninfo --summary`)
  and a recovery/reboot procedure (hard reset + select default entry).
- Only adds a kernel param — no root-filesystem change, so no store-path
  rollback is needed to revert.
- See `hosts/host.thing/hardware.Radeon8060S.nix` for the inline documentation.

## Per-model options (task item 3)

Four options added to the model submodule in
`modules/.../options.nix` so backend-specific policy can differ per
model without duplicating metadata:

| Option | Type | Default | Used by |
|--------|------|---------|---------|
| `serverPackage` | nullOr package | null | DFlash2 (fork) |
| `extraEnv` | attrsOf str | {} | ROCm (HSA vars) |
| `noMmap` | nullOr bool | null | DFlash2 (false = mmap+mlock) |
| `sha256` | nullOr str | null | Both (pinned LFS oid, logged in banner) |

These default to null/empty, so existing models are unaffected
(snapshot-verified: all 35 gfx + 17 rtx models byte-identical).

## Runtime version verification (task item 7)

Each generated `llama-server_*` wrapper prints a startup banner to
stderr at launch:

```
[llama-cpp] === startup banner ===
[llama-cpp]   model:     Qwen3.8-27B-DFlash2-Q6_K_XL
[llama-cpp]   backend:   vulkan
[llama-cpp]   device:    Vulkan0
[llama-cpp]   version:   0eb5280-strix-halo
[llama-cpp]   srcRev:    0eb528051a56f34567312ce63ab4e14a3fc71d89
[llama-cpp]   modelPath: /models/unsloth-Qwen3.8-27B-GGUF/Qwen3.8-27B-UD-Q6_K_XL.gguf
[llama-cpp]   sha256:    701d8fa9ed214ab21bfc130cd2a7df19ca89bbef7713e2dfb19f3c63696aa917
[llama-cpp]   cacheType: f16
[llama-cpp]   ctxSize:   131072
[llama-cpp]   parallel:  1
[llama-cpp]   noMmap:    false(mmap+mlock)
[llama-cpp]   serverPkg: fork:llama-cpp-0eb5280-strix-halo
[llama-cpp] === effective flags (see set -x below) ===
```

The full effective command line is printed by `set -x` immediately after.
This lets an operator verify which engine, revision, model hash, and
flags a running llama-server actually started with — check the
llama-swap journal (`journalctl -u llama-swap`) for the banner.
