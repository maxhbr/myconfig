# Fix DFlash2 candidate: draft-context abort on op-NONE `output.weight`

Introduced by commit `023c756634` ("Qwen3.8-27B: add Vulkan DFlash2 + ROCm
MTP/ngram candidate profiles"); candidate lives in
`hosts/host.thing/myconfig.ai.llama-cpp/Qwen3.8-27B.nix`
(`candidateModels`, entry `Qwen3.8-27B-DFlash2-Q6_K_XL`), engine
`pkgs.llama-cpp-strix-halo` (Nathanw1014 strix-halo-vulkan fork,
pinned `0eb5280`) from `hosts/host.thing/nixpkgs.overlays.llama-cpp.nix`.

## Symptom

`llama-server` aborts with SIGABRT while creating the DFlash2 *draft*
context (after the target model loaded fine):

```
ggml-backend.cpp:930: pre-allocated tensor (output.weight) in a buffer
(Vulkan1) that cannot run the operation (NONE)
```

Stack: `common_speculative_init_result` → `llama_context` ctor →
`sched_reserve` → `resolve_fused_ops` → `graph_reserve` →
`ggml_backend_sched_split_graph`. The draft's `output.weight` is
pre-allocated in the Vulkan weights buffer but the draft-dflash draft
graph never runs an op on it, and the scheduler validation rejects the
op-NONE leaf. The earlier `dflash requires ctx_other to be set`
memory-fitting warning is benign and unrelated.

## Root cause (identified 2026-09-03, upstream — NOT fork-specific)

`ggml_backend_sched_backend_from_buffer()` calls `supports_op()` for
every pre-allocated tensor. The Vulkan backend's `supports_op` has no
`GGML_OP_NONE` case and falls through to `default: return false` (the
CPU backend returns true for `GGML_OP_NONE`), so no backend is found
for the unused draft lm_head leaf → `GGML_ABORT` in
`ggml_backend_sched_backend_id_from_cur()`.

The identical abort site is present in **every build this host pins or
could pin today** (verified by source inspection):

- upstream master (as of 2026-09-03, `d30500b`),
- upstream `b10549` (the host's pinned release tag),
- upstream `v0.3.0` (the pinned nixpkgs `llama-cpp`),
- fork `0eb5280` (the pinned `llama-cpp-strix-halo`),
- fork tip `df1671a` (2026-08-31, +74 commits past `0eb5280`) — so
  bumping the fork pin does NOT fix this.

Upstream PR **#27906** ("ggml : skip backend supports_op check for
GGML_OP_NONE", head `2eeed91e67bbb64c9065e6e13b9039ad587b7eee`) is the
exact one-line fix: skip the `supports_op` check when `op->op ==
GGML_OP_NONE` in `ggml_backend_sched_backend_from_buffer()`
(`ggml/src/ggml-backend.cpp`). It reproduces our abort with the same
DFlash2 draft setup (their log names `token_embd.weight` instead of
`output.weight` — same op-NONE leaf class). It was **closed unmerged
by its author on 2026-08-31**; no fix has landed in master as of
2026-09-03. Related upstream issues (open): #26636, #27833.

Because the abort can name *any* unused pre-allocated draft tensor, the
`output.weight=CPU` override only patches one instance of the class —
do not rely on "minimal working flag subset" pruning (see checklist).

## Applied mitigation

Both workaround flags are in the candidate's `params` in
`Qwen3.8-27B.nix` (applied in commit `523136e430`, following
`023c756634`):

- `--override-tensor-draft output.weight=CPU` — keeps the unused draft
  lm_head out of the Vulkan weights buffer (targets the aborting
  tensor directly).
- `--no-op-offload` — disables the op-offload / fused-op resolution
  path (`resolve_fused_ops`) shown in the abort trace.

Both flags exist in the pinned fork source (`common/arg.cpp`: 2903 /
4049). They were applied blind (no gfx1151 hardware in the authoring
environment); the per-flag mechanism remains unconfirmed. Note the
inline comment in `Qwen3.8-27B.nix` and the `profiles.md` row still
call this a "fork bug" — per the root-cause section above, that
attribution is wrong; the abort is an upstream scheduler bug.

## Checklist

- [ ] **Preferred fix: carry PR #27906 locally.** Add its one-line
      patch (`skip_op_none` in `ggml_backend_sched_backend_from_buffer`)
      to the `llama-cpp-strix-halo` override in
      `hosts/host.thing/nixpkgs.overlays.llama-cpp.nix`
      (`patches = [ ./pr27906-skip-op-none.patch ];` — composes with
      nixpkgs `package.nix`, which passes `inherit (finalAttrs) src
      patches` to `fetchNpmDeps`). Then drop BOTH workaround flags
      from the candidate's `params` in `Qwen3.8-27B.nix`, fix the
      stale "fork bug" comment there and the Workaround row in
      `doc/qwen38-gfx1151/profiles.md`, re-run
      `nix eval --raw
      .#nixosConfigurations.test-thing.config.system.build.toplevel.drvPath`
      (the flake exposes hosts as `test-<name>`; the plain
      `nixosConfigurations.thing` path used elsewhere in
      `doc/qwen38-gfx1151/` does not exist),
      and delete this file.
- [ ] **Validate on gfx1151 hardware** (either with the patch, or with
      the flags until then): swap in
      `Qwen3.8-27B-DFlash2-Q6_K_XL` via llama-swap (gfx container,
      `Vulkan0`) or run the wrapper manually on the host
      (`Vulkan1`); confirm the server starts and speculation works.
- [ ] **Upstream tracking**: watch for a merged fix for the op-NONE
      pre-allocated-tensor abort (PR #27906 was closed unmerged;
      issues #26636 / #27833 are open). Once a merged fix is in a
      release tag, bump the pin in
      `hosts/host.thing/nixpkgs.overlays.llama-cpp.nix` and drop the
      local patch instead.
- [ ] ~~Bisect fork vs upstream~~ — **obsolete**: both abort
      identically (see Root cause). If reporting upstream, reference
      PR #27906 / issue #26636 with our `output.weight` log; a fork
      report to Nathanw1014 is not warranted.
- [ ] **If nothing works on hardware**: remove or comment out the
      DFlash2 candidate from `candidateModels` so llama-swap stops
      attempting swap-ins that load 25 GB before crashing, and delete
      `llama-cpp-strix-halo` from the overlay if nothing else uses it
      (check `nixpkgs.overlays.llama-cpp.nix` consumers first).

## Triage 2026-09-03

- Workaround flags still present (`Qwen3.8-27B.nix`, commit
  `523136e430`); note not stale.
- Root cause identified upstream (see above); "fork bug" premise
  corrected. Fork tip `df1671a` checked: no fix there either.
- No gfx1151 hardware available → no code change made; flags kept.
- Stale command corrected: `nixosConfigurations.thing` does not exist
  as a flake output; `nixosConfigurations.test-thing` evaluates fine
  (verified 2026-09-03). `doc/qwen38-gfx1151/README.md` and
  `recommendation.md` still carry the broken `nixosConfigurations.thing`
  path — fix them when that doc set is next touched.
- Method: source inspection of pinned revs via raw.githubusercontent /
  GitHub API / git clone; PR #27906 state confirmed via HTML + API +
  `git merge-base --is-ancestor` against upstream master.
