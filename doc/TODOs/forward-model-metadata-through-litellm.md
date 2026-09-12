# Forward model metadata (context windows): deploy + verify (tng.nix change applied)

## Status

The tng.nix-side change is **applied and eval-verified** (uncommitted on
`../tng.nix` `main`):

- `doc/TODOs/tng-litellm-forward-model-metadata.patch` was applied to
  `../tng.nix` (equivalent re-implementation, same semantics):
  - `TNG-tng.nix/modules/trustedtokens/default.nix`:
    `mkTrustedTokensEntry` adds `model_info.max_input_tokens = m.context_length`.
  - `TNG-tng.nix/modules/skainet/default.nix`: `mkSkainetEntry` /
    `mkSkainetExternalEntry` add `model_info.max_input_tokens = m.max_total_tokens`.
  - `modules/litellm.nix`: pool entries propagate
    `model_info.max_input_tokens = min across member deployments`
    (`ctxOf` + `foldl' lib.min`; none emitted if no member declares one).
- Verified with `nix eval` on a minimal host importing the tng modules:
  `trustedtokens/zai-org/GLM-5.3`, `skainet/zai-org/GLM-5.3(-Flash)` and the
  pools `GLM-5.3`/`GLM-5.3-Flash` all report `max_input_tokens = 1048576`.
  nixfmt-rfc-style clean.
- The myconfig-side companion work (forward entries in
  `hosts/shared.localModels.litellm.models.nix`, first-occurrence-wins lookups
  in `programs.pi-coding-agent`) was already done — see bd issue `myconfig-0rs`.

## What remains

1. **Commit** the three modified files in `../tng.nix` (currently unstaged on
   `main`). Use the commit message embedded in
   `doc/TODOs/tng-litellm-forward-model-metadata.patch` (subject: "litellm:
   forward model metadata (context window) through model_list entries"),
   then delete the patch file from this repo.
2. **Re-deploy thing** with the updated tng.nix and verify:
   `curl -s localhost:4000/model/info | jq '.data[] | select(.model_name=="GLM-5.3") | .model_info.max_input_tokens'`
   reports the pool minimum (1048576, not null).
3. **Re-run `./hosts/shared.localModels.update.sh`** (needs thing's LiteLLM
   reachable — it was down, connection refused on
   `thing.wg0.maxhbr.local:4000`). It must reproduce the hand-added
   GLM-5.3/skainet entries of `hosts/shared.localModels.litellm.models.nix`
   from the now-enriched upstream `/model/info` (regeneration is the source
   of truth; the hand edits match the same schema).
4. **Switch f13/p14**, then in pi `/model` shows
   `f13-litellm/GLM-5.3` with context 1048576 (was 131072) and maxTokens
   65536 (was 32768). The auto-compaction/turn-prefix-summarization errors
   disappear; footer shows `%/1048k`.

Once steps 1–4 pass, this file (and the patch file, if still present) can be
deleted and bd issue `myconfig-0rs` closed.

## Related

- bd issue: `myconfig-0rs`
- `modules/myconfig.ai/litellm.proxy.nix` (local-proxy `model_info`
  plumbing, `mkForwardEntry`), already supports `contextWindow`.
