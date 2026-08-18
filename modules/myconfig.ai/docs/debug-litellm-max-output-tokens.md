<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# Debugging: "Model stopped because it reached the maximum output token limit"

Investigation of the frequent abort of AI coding agents on `f13`.

## Symptom

pi-coding-agent sessions on `f13` regularly end a turn with:

```
Error: Model stopped because it reached the maximum output token limit.
The response may be incomplete.
```

This string is rendered by pi itself when the API response carries
`finish_reason: "length"`:

```
$ grep -n 'maximum output token limit' \
    <pi-store-path>/dist/modes/interactive/components/assistant-message.js
121:        if (message.stopReason === "length") {
123: ... "Error: Model stopped because it reached the maximum output token limit. ..."
```

So *something* caps the completion. The question is which hop.

## The chain as actually configured

```
pi-coding-agent (f13)
  └─ http://localhost:4000/v1                LOCAL LiteLLM (f13)
       └─ http://litellm.thing.wg0.maxhbr.local:80/v1
            └─ (Caddy on thing) ─> localhost:4000  REMOTE LiteLLM (thing)
                 └─ http://localhost:33657/v1      llama-server (llama-swap, gfx1151)
```

### Hop 1 — local LiteLLM on f13

* `hosts/shared.litellm.proxy.nix` enables `myconfig.ai.litellm.proxy`
  with `upstreamApiBase = "http://litellm.thing.wg0.maxhbr.local:80/v1"`
  and `models = import ./shared.localModels.litellm.models.nix`.
* `modules/myconfig.ai/litellm.proxy.nix` turns each entry into a
  pass-through `openai/<name>` entry. Entries that are **bare strings**
  — which every `gfx1151:*` entry in
  `hosts/shared.localModels.litellm.models.nix` is — get **no**
  `model_info` block at all.

Observed (`/model/info` on `localhost:4000`, trimmed):

```json
{"model_name":"gfx1151:Qwen3.6-27B-UD-Q5_K_XL",
 "litellm_params":{"api_base":"http://litellm.thing.wg0.maxhbr.local:80/v1",
                   "model":"openai/gfx1151:Qwen3.6-27B-UD-Q5_K_XL"},
 "model_info":{"max_tokens":null,"max_input_tokens":null,"max_output_tokens":null}}
```

⇒ **the local hop imposes no cap** — and it also passes no budget
information down to its clients.

### Hop 2 — remote LiteLLM on thing

`modules/myconfig.ai/services.litellm.nix` auto-generates the model list
from `myconfig.ai.localModels` and adds, per model with a known context
window:

```nix
// lib.optionalAttrs (modelContextWindow != null) {
  max_input_tokens = modelContextWindow;
  max_tokens = lib.min (modelContextWindow / 4) 65536;
};
```

Observed (`/model/info` on `thing.wg0.maxhbr.local:4000`, trimmed):

```json
{"model_name":"gfx1151:Qwen3.6-27B-UD-Q5_K_XL",
 "litellm_params":{"api_base":"http://localhost:33657/v1",
                   "model":"openai/Qwen3.6-27B-UD-Q5_K_XL",
                   "tags":["base","gfx1151","vulkan"]}}
```

For this model the remote publishes **no** budget at all. For models
whose entry does carry a context window it publishes a *default*, e.g.

```json
{"model_name":"gfx1151:Qwen3.8-27B-UD-Q8_K_XL-general-tasks",
 "litellm_params":{"max_input_tokens":262144,"max_tokens":65536, ...}}
```

⇒ the remote hop supplies at most a **default** of 65536 output tokens,
and (experiments E4/E6) a client-supplied `max_tokens` *wins* over it —
LiteLLM does not clamp the request to the config value.

### Hop 3 — llama.cpp backend

`hosts/host.thing/myconfig.ai.llama-cpp/Qwen3.6-27B.nix:90`:

```nix
{
  name = "Qwen3.6-27B-UD-Q5_K_XL";
  ...
  cacheType = "q8_0";
  parallel = 1;
  ctxSize = 262144;
  ttl = 900;
}
```

`router.nix:199` emits `ctx-size = ctxSize * parallel` ⇒ the single slot
gets the full **262144** tokens. No `--n-predict` is set anywhere, so
llama-server's default (unlimited, bounded by the remaining context)
applies.

⇒ **the backend imposes no meaningful cap either** (experiments E3, E5).

### Hop 0 — the client (pi), i.e. the actual culprit

`modules/myconfig.ai/programs.pi-coding-agent/default.nix` generates
`~/.pi/agent/extensions/myconfig-providers.ts` and registered **every**
model with a hard-coded:

```nix
maxTokens = 4096;
```

Deployed evidence:

```
$ grep -o '"id":"gfx1151:Qwen3.6-27B-UD-Q5_K_XL"[^}]*' \
    ~/.pi/agent/extensions/myconfig-providers.ts
"id":"gfx1151:Qwen3.6-27B-UD-Q5_K_XL","input":["text"],"maxTokens":4096,...,"reasoning":false
```

pi turns that field into the request's `max_tokens`
(`@earendil-works/pi-ai`):

```ts
// src/api/simple-options.ts
maxTokens: clampMaxTokensToContext(model, context, options?.maxTokens ?? model.maxTokens),
// src/api/openai-completions.ts:707
if (options?.maxTokens) { (params as any).max_tokens = options.maxTokens; }
```

pi's own default when a provider omits the field is `16384`
(`dist/core/provider-composer.js:70`) — the hard-coded `4096` was **four
times lower than pi's own default**, and 16× lower than what the chain
allows. For a *thinking* model such as Qwen3.6/3.8 the reasoning block
alone routinely exceeds 4096 tokens, so the turn dies before the answer
even starts.

## Experiments

Test model for all experiments below:
**`gfx1151:Qwen3.6-27B-UD-Q5_K_XL`** (~10 tok/s on the gfx1151 backend;
the heavier `Qwen3.8-27B-UD-Q8_K_XL-general-tasks` behaves identically
but generates at 3–6 tok/s, which makes long-output experiments take
tens of minutes).

No API key is required — neither proxy configures a `master_key`
(documented in `modules/myconfig.ai/litellm.proxy.nix`), so nothing had
to be redacted.

### E1 — small prompt, explicit `max_tokens: 4096`, local proxy

```bash
curl -s http://localhost:4000/v1/chat/completions \
  -H 'Content-Type: application/json' \
  -d '{"model":"gfx1151:Qwen3.6-27B-UD-Q5_K_XL",
       "messages":[{"role":"user","content":"Count from 1 to 500, one number per line."}],
       "max_tokens":4096}' | jq '.usage, .choices[0].finish_reason'
```

```json
{"prompt_tokens":25,"completion_tokens":2621,"total_tokens":2646}
"stop"
```

### E2 — same, **without** `max_tokens` (server-side default)

| hop | prompt_tokens | completion_tokens | finish_reason |
|---|---|---|---|
| `localhost:4000` | 25 | 2381 | `stop` |
| `thing.wg0.maxhbr.local:4000` | 25 | 2592 | `stop` |

Short answers never hit a cap — which is why the bug only shows up in
real agent sessions.

### E3 — 20 000-word filler prompt, no `max_tokens`, local proxy

```json
{"prompt_tokens":20033,"completion_tokens":1853,"total_tokens":21886}
"stop"
```

⇒ disproves the `n_ctx - prompt_tokens` hypothesis: a 20k prompt still
leaves plenty of output budget.

### E4 — reproduce `finish_reason: "length"` with pi's exact cap

```bash
curl -s http://localhost:4000/v1/chat/completions \
  -H 'Content-Type: application/json' \
  -d '{"model":"gfx1151:Qwen3.6-27B-UD-Q5_K_XL",
       "messages":[{"role":"user","content":"Write a thorough technical design document of about 1800 words for a distributed key-value store, covering consistency, replication, failure modes, and the client API."}],
       "max_tokens":4096}' | jq '.usage, .choices[0].finish_reason'
```

```json
{"prompt_tokens":44,"completion_tokens":4096,"total_tokens":4140}
"length"
```

**Exactly 4096 completion tokens ⇒ the client value is what binds**, and
it is *not* rewritten by either LiteLLM hop (the remote's
`litellm_params.max_tokens` default did not raise it, the local hop did
not lower it). The same experiment against
`gfx1151:Qwen3.8-27B-UD-Q8_K_XL-general-tasks` — whose remote entry
*does* declare `max_tokens: 65536` — likewise stopped at exactly 4096.

### E5 — large prompt **and** large budget: does the backend accept it?

40 000-word filler prompt + `max_tokens: 32768`, asking for a one-word
reply:

```json
{"prompt_tokens":40021,"completion_tokens":329,"total_tokens":40350,
 "prompt_tokens_details":{"cached_tokens":19517}}
"stop"
```

⇒ a 40k prompt with a 32k output budget is accepted without error; the
262144-token context is nowhere near exhausted.

### E6 — post-fix budget: the truncating request now completes

The **identical** request as E4, with the budget the fixed generator
emits for this model on `f13` (`32768`):

```bash
curl -s http://localhost:4000/v1/chat/completions \
  -H 'Content-Type: application/json' \
  -d '{"model":"gfx1151:Qwen3.6-27B-UD-Q5_K_XL",
       "messages":[{"role":"user","content":"Write a thorough technical design document of about 1800 words for a distributed key-value store, covering consistency, replication, failure modes, and the client API."}],
       "max_tokens":32768}' | jq '.usage, .choices[0].finish_reason'
```

```json
{"prompt_tokens":44,"completion_tokens":4716,"total_tokens":4760}
"stop"
```

**Same prompt, same chain, only the client-side budget changed:
`length` @4096 → `stop` @4716.** The reported error is gone for exactly
the request that produced it.

## Root cause

**The cap is client-side, not proxy-side.**
`modules/myconfig.ai/programs.pi-coding-agent/default.nix` registered
every model of every generated provider with a hard-coded
`maxTokens = 4096`. pi sends that verbatim as the OpenAI `max_tokens`
request field, so every completion longer than 4096 tokens — trivially
reached by a reasoning model whose `<think>` block counts towards the
output budget — comes back with `finish_reason: "length"`, which pi
surfaces as the reported error.

Contributing factors:

* The models are also registered with `reasoning = false`, so pi does not
  account for thinking tokens when budgeting.
* On `f13` the local proxy's model entries are bare strings, so neither
  `max_input_tokens` nor `max_output_tokens` reaches pi from LiteLLM;
  pi falls back to the module's `defaultContextWindow = 131072` even
  though the backend really offers 262144.
* The same hard-coded `4096` existed in the microVM guest generator
  (`modules/myconfig.ai/myconfig.ai.microvm/guest-model-config.nix`).

## Fix

Implemented in this branch:

1. `modules/myconfig.ai/programs.pi-coding-agent/default.nix`
   * new `maxOutputTokensLookup`, built from the LiteLLM `model_list`
     (`litellm_params.max_tokens`, falling back to
     `model_info.max_output_tokens`) — the mirror image of the existing
     `contextWindowLookup`;
   * new `deriveMaxOutputTokens = cw: lib.min (cw / 4) 65536`, the same
     formula `modules/myconfig.ai/services.litellm.nix` already uses for
     its generated `litellm_params.max_tokens`;
   * `maxTokens = maxOutputTokensLookup.${modelId} or (deriveMaxOutputTokens contextWindow)`
     instead of the constant `4096`.

2. `modules/myconfig.ai/myconfig.ai.microvm/guest-model-config.nix`
   * `guestModelConfig.maxTokens = 4096` → `guestModelConfig.maxTokensCap = 65536`;
   * the generated pi extension now reports
     `min(contextWindow / 4, maxTokensCap)` per model.

Effect on the generated `~/.pi/agent/extensions/myconfig-providers.ts`
(evaluated for `f13`, 238 model entries):

| | `maxTokens` distribution |
|---|---|
| before | `4096` × 238 |
| after | `32768` × 185, `65536` × 31, `49152` × 10, `46080` × 6, `16384` × 6 |

For the test model: `4096` → `32768` on `f13`. Models where the remote
LiteLLM publishes a real `litellm_params.max_tokens` (e.g. the
Qwen3.8 variants, evaluated on `thing`) get that value verbatim —
`65536`.

**A `nixos-rebuild switch` (or at least a home-manager activation) is
required on `f13` for the regenerated extension to take effect.**

## Part 2 — why the budget information never reached the agents

The fix above makes pi *derive* a budget, but it derives it from a
fallback `contextWindow` of 131072 because nothing in the chain told it
the truth. Three separate defects caused that.

### 2a. `--ctx-size` is not the per-request context

llama-server's `--ctx-size` sizes the WHOLE KV cache and is divided by
the slot count unless the cache is unified
(`src/llama-context.cpp:286`):

```cpp
if (cparams.kv_unified) {
    cparams.n_ctx_seq = cparams.n_ctx;
} else {
    cparams.n_ctx_seq = cparams.n_ctx / cparams.n_seq_max;
}
```

and unified KV is enabled implicitly ONLY in auto mode, i.e. when
`--parallel` is not passed at all (`tools/server/server.cpp:146`):

```cpp
if (params.n_parallel < 0) { n_parallel = 4; kv_unified = true; }
```

So the module's `parallel` option had a counter-intuitive cost model:

| config | flags | slots | ctx per request | KV memory |
|---|---|---|---|---|
| `parallel = 1` | `--ctx-size C` | 4 (auto) | **C** (shared pool) | 1×C |
| `parallel = 4` | `--ctx-size 4C --parallel 4` | 4 | C (static slice) | **4×C** |
| `parallel = 4` + `kvUnified` | `--ctx-size C --parallel 4 --kv-unified` | 4 | **C** (shared pool) | 1×C |

A new `kvUnified` option (`options.nix`) selects the third row;
`lib/scripts.nix` and `router.nix` only multiply `--ctx-size` by
`parallel` when it is off. thing's `Qwen3.6-35B-A3B` entries — the
models behind the `hermes` and `opencode-fast` aliases — were the only
`parallel > 1` models and now use it:

```
-  --ctx-size 1048576 --parallel 4 --cont-batching
+  --ctx-size 262144  --parallel 4 --cont-batching --kv-unified
```

### 2b. The advertised context window was `parallel`× too large

`effectiveContextWindow = m: m.ctxSize * m.parallel` (`llama-swap.nix`,
`router.nix`) is published through `myconfig.ai.localModels` into
LiteLLM's `max_input_tokens` and from there to the agents. For the
`parallel = 4` models it advertised **1048576** tokens while each slot
could really hold 262144 — pi would happily fill a prompt four times
larger than the slot. It is now `m.ctxSize`, which equals llama.cpp's
`n_ctx_seq` under both layouts (a no-op for every `parallel = 1` model).

### 2c. `ctxSize` was dropped when re-serving RTX models on gfx1151

`hosts/host.thing/myconfig.ai.llama-cpp/default.nix`'s `fromRtxModels`
rebuilds each model from an explicit allowlist
(`name`/`path`/`params`/`group`/`aliases`) — so `ctxSize`, `cacheType`,
`parallel` and `kvUnified` were silently discarded. Consequence:
llama-server started without `--ctx-size` (falling back to the GGUF's
`n_ctx_train`) and the model published `contextWindow = null`, so
LiteLLM emitted no `max_input_tokens`/`max_tokens` at all.

That is exactly why the test model looked like this on thing:

```json
{"model_name":"gfx1151:Qwen3.6-27B-UD-Q5_K_XL",
 "litellm_params":{"model":"openai/Qwen3.6-27B-UD-Q5_K_XL"}}   // no budgets
```

despite `hosts/host.thing/myconfig.ai.llama-cpp/Qwen3.6-27B.nix:104`
declaring `ctxSize = 262144`. Carrying the four attributes through
raises the gfx1151 models that publish a context window from
**101/154 to 131/154**, and the test model now evaluates to:

```json
{"model_name":"gfx1151:Qwen3.6-27B-UD-Q5_K_XL",
 "litellm_params":{"max_input_tokens":262144,"max_tokens":65536, ...}}
```

### 2d. The model-list scrape only saw *loaded* models

`hosts/shared.localModels.update.sh` regenerates
`hosts/shared.localModels.litellm.models.nix`, which is what f13's local
proxy forwards. It scraped `--ctx-size` out of `status.args` in the
backends' `/v1/models` — but llama-swap only publishes the command line
for models it currently has **loaded**. With `ttl` between 300 and
1800s essentially everything is unloaded at scrape time; a run during
this investigation recovered **55** entries that way, out of 179
available. Hence the bare strings.

`build_budget_map` now reads the upstream LiteLLM's `/model/info`
instead, which reports what the Nix config declares and is therefore
independent of load state (**179** context windows and **171** output
budgets in the same run). It also emits `maxOutputTokens`, populating
the `myconfig.ai.litellm.proxy.models[*].maxOutputTokens` option that
existed but had never been fed.

## Deployment order

1. Rebuild **thing** — 2a/2b/2c only take effect there, and the models
   must be reloaded (or their `ttl` expire) for the new llama-server
   command line to apply.
2. Re-run `./hosts/shared.localModels.update.sh litellm` and commit the
   regenerated `hosts/shared.localModels.litellm.models.nix`. Doing it
   *before* step 1 would bake in the pre-fix `ctxSize * parallel`
   context windows.
3. Rebuild **f13** / **p14** so the local proxy forwards the new
   `model_info` and pi regenerates `myconfig-providers.ts`.

## Follow-ups (not done here)

* The generators register all models with `reasoning = false`; marking
  the thinking variants as reasoning models would let pi budget the
  `<think>` block explicitly.
* `modules/myconfig.ai/services.litellm.nix` still emits no
  `max_input_tokens`/`max_tokens` for models whose `ctxSize` is unset
  (23 of 154 gfx1151 entries after the fixes above).
