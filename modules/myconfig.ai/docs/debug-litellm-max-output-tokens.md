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

## Follow-ups (not done here)

* `hosts/shared.localModels.update.sh` could emit
  `{ name; contextWindow; maxOutputTokens; }` attrsets for the local
  proxy's model list, so `f13` learns the *real* 262144 context window
  and 65536 output budget instead of falling back to
  `131072` / `32768`. The plumbing already exists
  (`myconfig.ai.litellm.proxy.models[*].maxOutputTokens`).
* The generators register all models with `reasoning = false`; marking
  the thinking variants as reasoning models would let pi budget the
  `<think>` block explicitly.
* `modules/myconfig.ai/services.litellm.nix` only emits
  `max_input_tokens`/`max_tokens` for models whose `contextWindow` is
  known; several `gfx1151:*` entries (including the test model) still
  publish nothing.
