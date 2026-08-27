# Final recommendation

**Status: PENDING hardware benchmarking.**

This document cannot issue a final recommendation because the acceptance
gates in the task require on-hardware measurements that have not yet been
run. What follows is the decision framework and a preliminary assessment.

## Decision framework (from the task's promotion rule)

A candidate may receive a production alias ONLY if ALL of:

1. **No correctness or tool-use regression** — the tool-call fixtures
   (`tests/qwen38-toolcall-fixture.sh`) pass, and a fixed coding/task
   battery produces equivalent answers under deterministic sampling.
2. **No stability regression** — repeated load/unload cycles through
   llama-swap, a multi-hour soak, and clean recovery after a stopped
   server all pass. No silent CPU fallback (verify with device
   enumeration + utilisation evidence).
3. **Meaningful improvement** — at least a meaningful improvement in
   median wall time per correct task, OR a clearly justified
   quality/context benefit (e.g. 262k context on ROCm). Tokens/second
   alone is insufficient.
4. **Acceptable memory headroom** — peak/resident RAM and GTT leave room
   for concurrent host services.
5. **Explicit operator decision** — the production alias move is
   authorized after benchmark review.

## What has been done (not pending)

- llama.cpp upgraded from b10056 to b10549 (stable, includes PR #25494).
- Version-drift assertion added (base=vulkan=rocm detectable).
- sharp.jinja vendored with provenance + SHA-256.
- Per-model options (serverPackage/extraEnv/noMmap/sha256) + startup banner.
- Nathanw1014 fork packaged separately (only DFlash2 uses it).
- DFlash2 + ROCm candidates added as opt-in profiles (no aliases moved).
- amdgpu.lockup_timeout=-1 specialisation added (opt-in, documented).
- All Nix evaluation + toplevel build verified clean.
- Snapshot-verified: existing 35 gfx + 17 rtx models unchanged; only +2
  candidate models added.

## What is pending (requires the gfx1151 hardware)

1. **Build the host** — `nix build .#nixosConfigurations.thing.config.system.build.toplevel`
   (the overlay + fork must actually build on the target architecture).
2. **Tool-call fixtures** — run `tests/qwen38-toolcall-fixture.sh` against
   each candidate (DFlash2 with sharp.jinja, ROCm with embedded template)
   and the production profile.
3. **Benchmark comparison** — run `tests/qwen38-benchmark-comparison.sh`
   (candidates vs production, back-to-back, randomized order).
4. **Deep-context gates** — Vulkan: fill + retrieve near 128k without GPU
   reset. ROCm: fill to 254k + exact retrieval at several depths.
5. **Stability soak** — multi-hour run with tool calls, prompt reuse, mixed
   short/long requests, repeated load/unload cycles.

## Preliminary assessment

### DFlash2 candidate (Vulkan, fork)

- **Pros:** Uses the PieBru-validated DFlash2 draft + sharp.jinja template.
  f16 KV is higher quality than the production q8_0. The fork may include
  Strix-Halo-specific Vulkan improvements not yet in upstream b10549.
- **Cons:** Uses a **fork** (Nathanw1014, not upstream) — a maintenance
  and trust liability. The fork is pinned to a specific commit; future
  Vulkan fixes would require re-pinning. 131k context is half the
  production's 262k. mmap+mlock (vs production --no-mmap) is untested
  on this hardware.
- **Key risk:** the fork may diverge from upstream in ways that break
  future upgrades. Only promote if the DFlash2 speed/quality gain is
  substantial and sustained.

### ROCm MTP/ngram candidate (upstream b10549, no fork)

- **Pros:** Uses upstream b10549 (no fork — lower maintenance risk).
  q4_0 KV + ngram-mod may improve throughput. 262k context matches
  production. The HSA env vars are candidate-only (no global impact).
- **Cons:** q4_0 KV is lower quality than the production's q8_0 KV.
  KyaniteLabs' high warm rates are repetition-assisted ngrams, not chat
  throughput — must not be mistaken for real-world speed. The HSA vars
  (`HSA_ENABLE_SDMA=0`, `HSA_XNACK=1`) are a stability tradeoff that
  needs validation against other ROCm workloads.
- **Key risk:** ngram-mod warm rates are misleading. The decision must
  use cold/novel-prose/code wall time, not replay tokens/second.

### If neither candidate clears the gate

Per the task: "keep the upgraded upstream engine and template fixes while
retaining Q8/MTP as the production route." The b10549 bump, version-drift
assertion, sharp.jinja vendoring, per-model options, and startup banner
are improvements independent of the candidate profiles and should be
kept regardless of the candidate outcome.

## Operator action items (after hardware benchmarking)

1. Run the fixtures + benchmarks (scripts in `tests/`).
2. Fill in the measurement table in `tests/qwen38-benchmark-comparison.sh` output.
3. Review against the decision framework above.
4. If a candidate clears: move the production alias in
   `hosts/host.thing/myconfig.ai.llama-cpp/Qwen3.8-27B.nix` (change the
   `aliases` on the relevant variant from the production model to the
   candidate — a one-line change, reviewed and authorized).
5. If neither clears: keep the production aliases as-is; the infrastructure
   improvements (version bump, template, options, banner) stand.
