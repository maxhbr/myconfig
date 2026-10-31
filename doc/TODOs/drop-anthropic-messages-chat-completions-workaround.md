# Drop the /v1/messages → chat/completions workaround once llama.cpp accepts litellm's Responses payload

## What is worked around

`modules/myconfig.ai.llmops/services.litellm.nix` sets
`settings.litellm_settings.use_chat_completions_url_for_anthropic_messages = true`.

Without it, litellm routes Anthropic `/v1/messages` requests whose
deployment is `openai/*` through its Responses-API bridge, which posts to
the backend's `/v1/responses` (and `/v1/responses/input_tokens` for
`/v1/messages/count_tokens`). llama.cpp's Responses→chat-completions
converter (`server_chat_convert_responses_to_chatcmpl` in
`tools/server/server-chat.cpp`) rejects item shapes the bridge used to
emit:

- ~~an assistant turn becomes a bare `{"role": "assistant", "content": …}`
  item with no `type` field → `400 Cannot determine type of 'item'`~~
  (fixed in the deployed litellm 1.106.x: assistant turns now carry
  `type: "message"` + `output_text` parts);
- a replayed `thinking` block becomes a `{"type": "reasoning", "summary": …}`
  with no `content` array → `400 item['content'] is not an array`
  (still unfixed on both sides, see Triage 2026-10-31).

The remaining failure fires for any request whose history carries a
reasoning/thinking block — i.e. every multi-turn conversation with the
reasoning models (Qwen3 etc.) these backends serve.

## Code touched

- `modules/myconfig.ai.llmops/services.litellm.nix` — the
  `use_chat_completions_url_for_anthropic_messages` setting and its
  comment.

Introduced in commit `4a3ab98eb763776c2e57850a3e8d17c6614337a6`.

## What to do

1. Drop the `use_chat_completions_url_for_anthropic_messages = true` line
   (and its comment) from `services.litellm.nix`.
2. Verify against a live deployment that a two-turn Anthropic tool loop
   works with the Responses bridge in place. Any model on `thing` works;
   the `rtx5090` and `gfx1151` backends are the ones that failed:

   ```bash
   # turn 1: expect a tool_use block
   curl -s -X POST http://thing.wg0.maxhbr.local:4000/v1/messages \
     -H 'content-type: application/json' \
     -d '{"model":"gfx1151:Qwen3.8-27B-UD-Q4_K_XL","max_tokens":300,
          "tools":[{"name":"Bash","description":"d",
                    "input_schema":{"type":"object","properties":{"command":{"type":"string"}},"required":["command"]}}],
          "messages":[{"role":"user","content":"Use Bash to list /tmp"}]}'

   # turn 2: replay the assistant content plus a tool_result — this is the
   # request that used to 500.
   ```

   The working alternative, if the bridge is still broken but the
   passthrough is fine, is `model_info.supported_endpoints = [ "/v1/messages" ]`
   on each deployment, which forwards `/v1/messages` untranslated to the
   backend's native Anthropic endpoint. Keeping the `openai/` provider
   prefix is preferable either way: it is what these llama.cpp backends
   are (OpenAI-compatible, not vLLM), and it keeps `litellm_params.model`
   describing the real upstream.

## Follow-up worth filing upstream

- ~~litellm: the Responses bridge should emit `type: "message"` on assistant
  items~~ (done in the deployed litellm 1.106.x, see Triage 2026-10-31).
  Remaining ask: include a `content` array on `reasoning` items
  (`_reasoning_item_from_block_group` in
  `litellm/litellm_core_utils/prompt_templates/common_utils.py` emits only
  `type` + `summary` [+ `encrypted_content`]).
- llama.cpp: `server_chat_convert_responses_to_chatcmpl` could accept a
  summary-only `reasoning` item instead of throwing
  `item['content'] is not an array`. (The older ask — treat a bare
  `{role, content}` item as a message — is obsolete now that litellm emits
  the `type` field.)

## Triage 2026-10-31

Checked upstream sources for the versions this repo deploys (no overlay
pins llama.cpp or litellm; both come from the locked nixpkgs rev
`1c89083e451e891152365b799595cf7790a43325`):

- litellm pinned at **1.106.4**. Its bridge
  (`litellm/llms/anthropic/pass_through/responses_adapters/transformation.py`,
  `translate_messages_to_responses_input`) now emits assistant text turns
  as `{"type": "message", "role": "assistant", "content": [{"type": "output_text", …}]}`
  — the bare-item 400 is gone. But replayed thinking blocks still become
  `{"type": "reasoning", "summary": […]}` with no `content` array
  (`_reasoning_item_from_block_group`, common_utils.py).
- llama.cpp pinned at **v0.7.0**; latest release is **v0.8.0**, and both —
  plus current master — still require `item['content']` to be a non-empty
  array on `reasoning` items (`tools/server/server-chat.cpp`) and still
  throw `item['content'] is not an array` otherwise.

Result: condition NOT met. Any `/v1/messages` request whose history carries
a thinking block would still 400 through the bridge, and the note's own
verification (live two-turn Anthropic tool loop against a llama.cpp backend
on thing) is not reachable from the review sandbox anyway
(thing.wg0.maxhbr.local:4000 unreachable; live backends needed). Flag kept.

Next check: after a litellm bump that adds `content` to reasoning items,
or a llama.cpp bump that accepts summary-only reasoning items — then run
the live two-turn tool-loop test on `thing` (turn 1 must produce a
`tool_use` block, turn 2 replays assistant content + `tool_result`,
ideally with a reasoning model so the thinking-block path is exercised
too).
