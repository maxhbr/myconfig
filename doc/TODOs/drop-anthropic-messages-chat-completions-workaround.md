# Drop the /v1/messages → chat/completions workaround once llama.cpp accepts litellm's Responses payload

## What is worked around

`modules/myconfig.ai.llmops/services.litellm.nix` sets
`settings.litellm_settings.use_chat_completions_url_for_anthropic_messages = true`.

Without it, litellm routes Anthropic `/v1/messages` requests whose
deployment is `openai/*` through its Responses-API bridge, which posts to
the backend's `/v1/responses` (and `/v1/responses/input_tokens` for
`/v1/messages/count_tokens`). The bridge emits two item shapes that
llama.cpp's Responses→chat-completions converter (`server_chat_convert_responses_to_chatcmpl`
in `tools/server/server-chat.cpp`) rejects:

- an assistant turn becomes a bare `{"role": "assistant", "content": …}`
  item with no `type` field → `400 Cannot determine type of 'item'`;
- a replayed `thinking` block becomes `{"type": "reasoning", "summary": …}`
  with no `content` array → `400 item['content'] is not an array`.

The first only fires for requests that replay an assistant turn, i.e. the
second and every later turn of a tool loop; the second fires for any
request whose history carries a reasoning block.

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

Both fixes below were verified against the `rtx5090` backend: each item shape
is accepted by llama-server today.

- litellm: the Responses bridge should emit `type: "message"` on assistant
  items and include a `content` array on `reasoning` items. Either change
  alone makes the payload acceptable.
- llama.cpp: `server_chat_convert_responses_to_chatcmpl` could treat a bare
  `{role, content}` item as a message instead of throwing
  `Cannot determine type of 'item'`.
