# Qwen3.8-27B serving on gfx1151 (Radeon 8060S / Strix Halo)

This directory documents the upgrade of the Qwen3.8-27B deployment on the
gfx1151 host: llama.cpp version pinning, tool-call templating, candidate
profiles, deep-context safety, rollback, and runtime version verification.

## Files

| File | Contents |
|------|----------|
| `research-notes.md` | Version selection rationale, hash computation, upstream/fork verification, PR #25494 |
| `profiles.md` | Production + candidate profile specs, deep-context limits, per-model options, rollback |
| `recommendation.md` | Final recommendation (pending hardware benchmarking) |

## Summary of changes

1. **llama.cpp b10056 → b10549** — newest stable tag including PR #25494
   (Vulkan q8_0 KV dequant) and Qwen3.8 / MTP / ngram-mod / draft-dflash.
   Version-drift assertion added so base/vulkan/rocm stay in sync.
2. **sharp.jinja vendored** — PieBru's tool-call template
   (`qwen3.8-froggeric-v22.3`, MIT) pinned with provenance + SHA-256.
3. **Per-model options** — `serverPackage`, `extraEnv`, `noMmap`, `sha256`
   added to the model submodule; startup banner logs revision/backend/
   device/model-hash/cache-type/context/flags.
4. **Nathanw1014 fork** — `llama-cpp-strix-halo` packaged separately so
   the DFlash2 candidate uses the Vulkan fork without moving other
   backends off upstream b10549.
5. **DFlash2 candidate** — Vulkan0, Q6_K_XL, DFlash2 Q8_0 draft,
   draft-dflash n-max 6, f16 KV, 131k, mmap+mlock, sharp.jinja.
6. **ROCm MTP/ngram candidate** — ROCm0, Q4_K_XL, mtp-Q8_0 draft,
   draft-mtp+ngram-mod n-max 12, q4_0 KV, 262k, candidate-only HSA vars.
7. **amdgpu.lockup_timeout=-1** — opt-in NixOS specialisation for
   deep-context Vulkan, NOT the default, with canary + recovery docs.

## Production aliases (unchanged)

`opencode` and `opencode-fallback` remain on the existing Q8_0/MTP
profile. No alias is repointed until benchmarking clears the acceptance
gates in the task document and the operator explicitly approves.

## Validation

```bash
# Nix eval + build (per AGENTS.md, prefer host-scoped over full flake check)
nix eval --raw .#nixosConfigurations.thing.config.system.build.toplevel.drvPath
nix build --dry-run .#nixosConfigurations.thing.config.system.build.toplevel

# Version-drift assertion (should show base=vulkan=rocm=10549)
nix eval .#nixosConfigurations.thing.config.myconfig.ai.llama-cpp.backendVersions

# Check the fork package
nix eval .#nixosConfigurations.thing.pkgs.llama-cpp-strix-halo.version

# Check candidate models in the gfx container
nix eval .#nixosConfigurations.thing.config.containers.llama-cpp-33657.config.myconfig.ai.llama-cpp.models
```
