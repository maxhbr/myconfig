# Identify the boot-time litellm `/health` (model-burst) caller on thing

Tracking bead: **myconfig-eg7** (priority 1, bug). This file is the
self-contained task description for work performed on host `thing`
(which has no access to the beads DB). Close the bead when done.

## Background

`thing` OOM'd at boot because many llama-swap models started at once in
the `llama-cpp-33657` container. Root cause chain:

1. A litellm `GET /health` call sends a **real chat completion**
   ("test from litellm", max_tokens=16) to *every* deployment in
   `model_list` (241 deployments), with unbounded parallelism
   (`asyncio.gather` over the whole list).
2. `thing`'s `model_list` points at on-demand backends: the gfx1151
   llama-swap instance (container `llama-cpp-33657`) and the rtx5090
   llama-server router.
3. The gfx1151 llama-swap instance has non-exclusive groups
   (MoE/dense/Qwen3.8-27B, all `exclusive=false`, intentional
   coexistence per commit 018436d333), so a burst starts up to 4
   concurrent multi-GB models (`--no-mmap`, 128k–256k ctx KV) on the
   Strix Halo unified memory → OOM at boot.

**Repo-side fix already merged** (commit 277c2312c9 "litellm: never
health-probe on-demand local model backends", on `master`, branch
`thing-litellm-fix` merged it earlier):

- `modules/myconfig.ai/services.litellm.nix` +
  `modules/myconfig.ai/litellm.proxy.nix`: every local/forwarded
  deployment now carries
  `model_info.disable_background_health_check = true`, and
  `general_settings` sets
  `health_check_skip_disabled_background_models = true` +
  `health_check_concurrency = 4`.

This neutralizes `/health` **regardless of caller**. What is still
unknown is *who* calls `GET /health` (or otherwise issues the burst) at
boot time — so a recurrence via a different path remains possible.

## Work items (all on thing)

1. **Find the caller.** Grep litellm access logs / container logs around
   a boot for `GET /health` (or evidence of a model burst, e.g. many
   near-simultaneous completions against the gfx1151 llama-swap paths):

   ```bash
   journalctl -b -g 'GET /health' --no-pager
   # and/or the litellm container logs for the boot in question
   ```

   Candidates to check:

   - n8n workflows (recurring /health or model-list pings)
   - Grafana / vserver tooling (uptime or model-status probes)
   - tng.nix-side scripts (anything that curls the litellm proxy at
     boot, e.g. a wait-for-service check)
   - a human

2. **If the caller is tng.nix-side:** file/fix it there too, in the
   tng.nix repo. (That repo can fix itself; this repo cannot.)

3. **Verify llama-swap in the container recovered after the deploy.**
   At the time the bead was filed, all `gfx1151.thing` paths returned
   502. Re-check a model request against the gfx1151 llama-swap
   instance after the fixed litellm config is deployed.

## Validation hints

- litellm exposes `GET /health/log` on port 4000 (thing, wg0): it shows
  per-deployment check status **without** triggering checks — safe to
  inspect.
- **Do NOT curl `GET /health` as a probe** — it fires real completions
  against every deployment. (Post-fix it should no longer hit the
  local/forwarded ones, but do not rely on that for probing.)

## Done when

- [ ] The boot-time `/health` (or burst) caller is identified, with
      journal evidence (log line + source IP / unit).
- [ ] If tng.nix-side: fixed or filed there, with a reference.
- [ ] gfx1151 llama-swap paths serve requests again (no 502).
- [ ] Bead myconfig-eg7 closed with the findings in the reason/comments.
