# Fix DFlash2 candidate: fork aborts on draft `output.weight` (op NONE)

Introduced by commit `023c756634` ("Qwen3.8-27B: add Vulkan DFlash2 + ROCm
MTP/ngram candidate profiles"); candidate lives in
`hosts/host.thing/myconfig.ai.llama-cpp/Qwen3.8-27B.nix`
(`candidateModels`, entry `Qwen3.8-27B-DFlash2-Q6_K_XL`), engine
`pkgs.llama-cpp-strix-halo` (Nathanw1014 strix-halo-vulkan fork,
`0eb5280`) from `hosts/host.thing/nixpkgs.overlays.llama-cpp.nix`.

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
pre-allocated in the Vulkan weights buffer but the `draft-dflash` draft
graph never runs an op on it, and the scheduler validation rejects the
op-NONE leaf. Earlier in the log the benign memory-fitting warning
`dflash requires ctx_other to be set` appears — that one is expected and
not the cause.

## Applied mitigation

Both workaround flags are now in the candidate's `params` in
`Qwen3.8-27B.nix` (commit that applied them: see the "fix DFlash2
candidate" commit following `023c756634`):

- `--override-tensor-draft output.weight=CPU` — keeps the unused draft
  lm_head out of the Vulkan weights buffer (targets the aborting
  tensor directly).
- `--no-op-offload` — disables the op-offload / fused-op resolution
  path (`resolve_fused_ops`) shown in the abort trace.

They were applied blind (no gfx1151 hardware in the authoring
environment); the exact mechanism of the fork bug is unconfirmed.

## Checklist

- [ ] Rebuild `host.thing` and swap in `Qwen3.8-27B-DFlash2-Q6_K_XL`
      via llama-swap (gfx container, `Vulkan0` there) or run the
      wrapper manually on the host (`Vulkan1`) to confirm the server
      now starts and speculation works.
- [ ] If it still aborts: run the wrapper with each flag removed in
      turn, and additionally try `--gpu-layers-draft 0` (diagnostic:
      draft fully on CPU) to isolate whether the abort is about tensor
      placement or the fused-op path.
- [ ] Bisect fork vs upstream b10549: repeat the identical command with
      the device-default Vulkan build (binary behind the
      `Qwen3.8-27B-Q8_0` gfx wrapper). If upstream aborts too, report to
      ggml-org/llama.cpp; otherwise report a fork regression to
      Nathanw1014/llama.cpp (`strix-halo-vulkan` branch).
- [ ] Once the minimal working flag subset is known: prune the other
      flag(s) from the `params` list in `Qwen3.8-27B.nix`, update
      `doc/qwen38-gfx1151/profiles.md`, re-run
      `nix eval --raw
      .#nixosConfigurations.thing.config.system.build.toplevel.drvPath`,
      and delete this file.
- [ ] If no workaround works: remove or comment out the DFlash2
      candidate from `candidateModels` so llama-swap stops attempting
      swap-ins that load 25 GB before crashing, and delete
      `llama-cpp-strix-halo` from the overlay if nothing else uses it
      (check `nixpkgs.overlays.llama-cpp.nix` consumers first).
