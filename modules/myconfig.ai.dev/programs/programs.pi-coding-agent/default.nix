# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# pi coding agent, the generated pi extensions (providers, jail marker,
# subagent/handoff examples) and the mysbx tier integration: pi's config is
# mounted into mysbx sandboxes from a dereferenced store tree built by the
# shared ../../../mysbx flake lib (nix/sandbox-config.nix) helper (bd myconfig-ooh).
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  osconfig = config;
  system = pkgs.stdenv.hostPlatform.system;
  cfg = config.myconfig.ai.dev.pi-coding-agent;
  callLib = file: import file { inherit lib pkgs; };

  # The shared mysbx sandbox-config helper (the dereferenced-store-tree
  # machinery, bd myconfig-ooh); consumed like the entry scripts consume
  # ../../../../mysbx/nix/mux-entry-lib.nix.
  sandboxConfigLib = inputs.mysbx.lib.sandboxConfigLib {
    inherit lib;
    runCommand = pkgs.runCommand;
  };

  # The mounted pi-config subtrees, shared with the modules whose payload
  # embeds pi's engine (programs.thinkrail.nix) so both describe ONE set
  # — see ../pi-sandbox-subtrees.nix.
  mysbxPiSubtrees = (import ../pi-sandbox-subtrees.nix { inherit config lib pkgs; }).subtrees;
  # The gate/path/content triple of the sandbox mcp.json mount, from the
  # shared generator — the same import the subtrees list (above) gates
  # on, so `subtrees` and `extraFiles` below cannot drift.
  piMcp = import ../pi-mcp-servers.nix { inherit config lib pkgs; };
  # The sandbox-only files that ride `extraFiles` (the trust extension
  # among them) — the shared contract the check asserts against.
  mysbxPiSandboxOnlyFiles =
    (import ../pi-sandbox-subtrees.nix { inherit config lib pkgs; }).sandboxOnlyFiles;

  # Build a lookup: model name (raw or provider-prefixed) -> contextWindow.
  # Covers both direct local-provider lookups (raw name) and
  # LiteLLM lookups (providerName:modelName).
  #
  # Two sources are merged (first occurrence wins, in source order):
  #
  #  1. `myconfig.ai.llmops.localModels` — the per-backend registry (llama-cpp /
  #     llama-swap). Each model entry may carry a `contextWindow` field.
  #     This populates lookups for both the raw model name and the
  #     provider-prefixed form (`providerName:modelName`).
  #
  #  2. `services.litellm.settings.model_list` — the LiteLLM proxy's own
  #     model list. Each entry's `litellm_params.max_input_tokens` (thing,
  #     auto-generated from the same localModels registry) or
  #     `model_info.max_input_tokens` (the local-proxy pattern in
  #     hosts/shared.litellm.proxy.nix, which carries context windows
  #     scraped by hosts/shared.localModels.update.sh) is the authoritative
  #     context window for that model as served by LiteLLM.
  #
  # Source (2) is essential for hosts that use the *local LiteLLM proxy*
  # pattern (f13, p14): they deliberately do NOT import
  # shared.localModels.litellm.nix, so their `localModels` is empty and
  # source (1) contributes nothing. Without source (2), every LiteLLM
  # model on those hosts would get contextWindow 0 in pi (the bug this
  # fixes). Source (2) also covers models provisioned by the private
  # (tng/priv) flake that register additional `model_list` entries (e.g.
  # trustedtokens/zai-org/GLM-5.2) — as long as the priv flake sets
  # `max_input_tokens` on those entries, pi picks it up here.
  contextWindowLookup =
    let
      fromLocalModels = lib.concatMap (
        provider:
        let
          hostPort = "${provider.host}:${toString provider.port}";
          providerName = if provider.name != null then provider.name else hostPort;
          rawModels = if provider.models != [ ] then provider.models else [ ];
        in
        lib.concatMap (
          m:
          if builtins.isAttrs m && m.contextWindow != null then
            [
              {
                name = m.name;
                value = m.contextWindow;
              }
              {
                name = "${providerName}:${m.name}";
                value = m.contextWindow;
              }
            ]
          else
            [ ]
        ) rawModels
      ) osconfig.myconfig.ai.llmops.localModels;

      # Extract the context window from a litellm model_list entry. The
      # value lives in `litellm_params.max_input_tokens` (the
      # auto-generated entries from modules/myconfig.ai.llmops/services.litellm.nix
      # and the local-proxy entries from hosts/shared.litellm.proxy.nix
      # both put it there) or in `model_info.max_input_tokens` (the
      # local-proxy entries' `model_info` block, also written by
      # litellm.proxy.nix). Prefer litellm_params, fall back to model_info.
      ctxFromLitellmEntry =
        e:
        let
          lp = e.litellm_params or { };
          mi = e.model_info or { };
          fromLp = lp.max_input_tokens or null;
          fromMi = mi.max_input_tokens or null;
        in
        if fromLp != null then fromLp else fromMi;

      fromLitellm = lib.concatMap (
        e:
        let
          cw = ctxFromLitellmEntry e;
          name = e.model_name or null;
        in
        lib.optional (name != null && cw != null) {
          inherit name;
          value = cw;
        }
      ) (osconfig.services.litellm.settings.model_list or [ ]);
    in
    # NOTE: `lib.listToAttrs` would keep the LAST entry for duplicate
    # names, silently discarding whichever contributor came first.
    # Duplicates DO occur: a LiteLLM model group can have several
    # deployments with the same `model_name` — e.g. the tng.nix pool
    # "GLM-5.3" resolves to two member deployments, and only some of
    # the contributions to a merged model_list carry metadata. Merge
    # FIRST-OCCURRENCE-WINS with `lib.foldl` so a metadata-bearing entry
    # is never shadowed by a later metadata-less one (and vice versa the
    # first defined value survives, giving stable, deterministic
    # results regardless of module merge order).
    lib.foldl' (
      acc: e@{ name, value }: if acc ? "${name}" then acc else acc // { "${name}" = value; }
    ) { } (fromLocalModels ++ fromLitellm);

  # Build a lookup: model name -> max OUTPUT tokens, i.e. the value pi
  # puts into `maxTokens` and therefore sends as the request's
  # `max_tokens`. See `mkOpenAiCompatibleProvider` below for why a
  # hard-coded value is harmful.
  #
  # Only LiteLLM `model_list` entries carry this information:
  #   - `litellm_params.max_tokens`  — written by
  #     modules/myconfig.ai.llmops/services.litellm.nix for every auto-generated
  #     local-model entry (`min (contextWindow / 4) 65536`);
  #   - `model_info.max_output_tokens` — written by
  #     modules/myconfig.ai.llmops/litellm.proxy.nix when a forwarded model
  #     declares `maxOutputTokens`.
  # `myconfig.ai.llmops.localModels` has no equivalent field, so it contributes
  # nothing here; those models fall back to the derived default below.
  maxOutputTokensLookup =
    let
      fromEntry =
        e:
        let
          lp = e.litellm_params or { };
          mi = e.model_info or { };
          fromLp = lp.max_tokens or null;
          fromMi = mi.max_output_tokens or null;
        in
        if fromLp != null then fromLp else fromMi;
    in
    # Same first-occurrence-wins merge as `contextWindowLookup` (see the
    # NOTE there): a model group with multiple deployments (e.g. a
    # tng.nix pool) contributes several entries under one `model_name`,
    # and the first one carrying the field must win deterministically.
    lib.foldl' (acc: e@{ name, value }: if acc ? "${name}" then acc else acc // { "${name}" = value; })
      { }
      (
        lib.concatMap (
          e:
          let
            mt = fromEntry e;
            name = e.model_name or null;
          in
          lib.optional (name != null && mt != null) {
            inherit name;
            value = mt;
          }
        ) (osconfig.services.litellm.settings.model_list or [ ])
      );

  # Fallback output budget for models that declare none: a quarter of the
  # context window, capped at 64k. This is the SAME formula
  # modules/myconfig.ai.llmops/services.litellm.nix uses for its generated
  # `litellm_params.max_tokens`, so a model resolved through either path
  # ends up with a consistent budget.
  deriveMaxOutputTokens = contextWindow: lib.min (contextWindow / 4) 65536;

  # Build a provider entry for an OpenAI-compatible base URL.
  mkOpenAiCompatibleProvider =
    {
      key,
      name,
      baseUrl,
      models,
      contextWindowLookup ? { },
    }:
    let
      # Conservative default context window for models whose real value
      # is not discoverable. Without this, pi receives `contextWindow = 0`
      # (pi defaults a *missing* contextWindow to 0, which breaks context
      # budgeting and truncation). 128k is a safe default that most modern
      # local models in this config provide; it only acts as a
      # last-resort fallback, never overrides a real value from
      # contextWindowLookup. Models that genuinely need a different value
      # must set `max_input_tokens` in their litellm model_list entry (or
      # `contextWindow` in localModels).
      defaultContextWindow = 131072;
    in
    {
      inherit key;
      value = {
        inherit name baseUrl;
        api = "openai-completions";
        apiKey = "dummy";
        authHeader = false;
        models = lib.map (
          modelId:
          let
            cw = contextWindowLookup.${modelId} or null;
          in
          {
            id = modelId;
            name = modelId;
            reasoning = false;
            input = [ "text" ];
            cost = {
              input = 0;
              output = 0;
              cacheRead = 0;
              cacheWrite = 0;
            };
          }
          // (
            let
              contextWindow = if cw != null then cw else defaultContextWindow;
            in
            {
              inherit contextWindow;
              # pi sends `maxTokens` as the request's `max_tokens`
              # (see `buildBaseOptions` in @earendil-works/pi-ai), so a
              # too-small value makes the model stop mid-answer with
              # `finish_reason: "length"`, which pi renders as
              #   "Error: Model stopped because it reached the maximum
              #    output token limit. The response may be incomplete."
              # This used to be hard-coded to 4096, which reasoning
              # models blow through with their thinking block alone.
              # See ../docs/debug-litellm-max-output-tokens.md.
              maxTokens = maxOutputTokensLookup.${modelId} or (deriveMaxOutputTokens contextWindow);
            }
          )
        ) models;
      };
    };

  localModelProviders = lib.map (
    model:
    let
      hostPort = "${model.host}:${toString model.port}";
      providerName = if model.name != null then model.name else hostPort;
      # localModels may contain strings or `{ name, kind ? null }`
      # submodules (computed kind tag is unused here).
      modelNames =
        let
          raw = if model.models != [ ] then model.models else [ providerName ];
        in
        lib.map (m: if builtins.isAttrs m then m.name else m) raw;
    in
    mkOpenAiCompatibleProvider {
      key = "local-${providerName}";
      name = hostPort;
      baseUrl = "http://${hostPort}/v1";
      models = modelNames;
      inherit contextWindowLookup;
    }
  ) osconfig.myconfig.ai.llmops.localModels;

  # `host` may be a wildcard (e.g. "0.0.0.0") for external exposure;
  # rewrite to localhost for in-host clients.
  litellmHost =
    if osconfig.services.litellm.host == "0.0.0.0" then "localhost" else osconfig.services.litellm.host;

  # The litellm provider id is shared between the build-time registration
  # (`litellmProviderStatic` below) and the runtime/dynamic litellm-models
  # extension (`litellmModelsExtension`, driven by `cfg.litellmUrl`): both
  # register `${hostname}-litellm`. Exactly ONE of them owns the id at a
  # time: when dynamic discovery is enabled (`cfg.litellmUrl != ""`), the
  # build-time provider is dropped from `allProviders` entirely and the
  # extension registers the full provider config (including models fetched
  # at runtime). pi merges `registerProvider` calls per field, so a split
  # registration (static extension contributing `baseUrl` + dynamic one the
  # models) would race on extension load order; a single owner avoids that
  # and any duplicate-provider conflicts.
  litellmProviderKey = "${osconfig.networking.hostName}-litellm";

  # Environment variable that replaces the litellm provider's baked
  # `baseUrl` (an OpenAI-compatible base URL, `/v1` included) for one
  # session. Read by BOTH generated extensions — the build-time
  # registration in `providersExtension` and the runtime discovery in
  # `litellmModelsExtension` — so whichever owns the provider id honours
  # it.
  #
  # Its consumer is the sandbox tier that mounts this very configuration
  # into a container: the host proxy binds `127.0.0.1` only, which inside
  # a container is the container's own loopback, so a sandboxed session
  # must name the host-side forwarder instead
  # (../../myconfig.ai.dev.litellm-forwarder.nix). Unset — every host
  # session — keeps the baked URL.
  litellmBaseUrlEnvVar = "MYCONFIG_LITELLM_BASE_URL";

  # Static (build-time) litellm provider registration: the full baked model
  # list. Only used when `cfg.litellmUrl == ""` (dynamic model discovery
  # disabled) — see `litellmProvider` below.
  litellmProviderStatic = mkOpenAiCompatibleProvider {
    key = litellmProviderKey;
    name = "LiteLLM (${osconfig.networking.hostName})";
    baseUrl = "http://${litellmHost}:${toString osconfig.services.litellm.port}/v1";
    # `model_list` contains one entry per DEPLOYMENT; a model group
    # with several deployments (e.g. a tng.nix pool like "GLM-5.3"
    # served by trustedtokens + skainet, or a plain alias emitted twice
    # by modules/myconfig.ai.llmops/services.litellm.nix) repeats the same
    # `model_name`. Deduplicate so pi registers each model once.
    models = lib.unique (lib.map (m: m.model_name) osconfig.services.litellm.settings.model_list);
    inherit contextWindowLookup;
  };

  # When the dynamic litellm-models extension owns the provider id, the
  # build-time registration is skipped entirely (single-owner rule above).
  litellmProvider = lib.optional (
    osconfig.services.litellm.enable && cfg.litellmUrl == ""
  ) litellmProviderStatic;

  llamaSwapProvider = lib.optional osconfig.services.llama-swap.enable (mkOpenAiCompatibleProvider {
    key = "llama-swap";
    name = "llama-swap";
    baseUrl = "http://localhost:${toString osconfig.services.llama-swap.port}/v1";
    models = builtins.attrNames osconfig.services.llama-swap.settings.models;
    inherit contextWindowLookup;
  });

  allProviders = localModelProviders ++ litellmProvider ++ llamaSwapProvider;

  # pi ships with a catalog of built-in providers (anthropic, google, openai,
  # groq, ... — 39 of them in 0.85.x) whose default models appear in `/model`
  # and `--list-models` as soon as the user authenticates (via `/login`, a
  # matching `*_API_KEY` env var, or auth.json). pi has no "disable defaults"
  # switch, but `pi.registerProvider(<builtinId>, { models: [] })` REPLACES
  # that provider's entire model list with the empty one (documented in
  # docs/extensions.md: "`models` - Array of model definitions. If provided,
  # replaces all existing models for this provider."). The provider itself
  # stays registered — its auth, `/login` entry and streaming behavior are
  # untouched — but it contributes zero models, so pi only offers the models
  # this repo configures via `allProviders` above.
  #
  # EXCEPTION: built-in providers that can authenticate WITHOUT an API key —
  # via OAuth/subscription login (`/login`, auth.json) or gcloud ADC — keep
  # their model list: anthropic (Claude Pro/Max OAuth; corporate proxies also
  # work through ANTHROPIC_AUTH_TOKEN + a baseUrl override), github-copilot,
  # kimi-coding, openai-codex (ChatGPT Plus/Pro), openrouter, xai
  # (SuperGrok/X Premium) and google-vertex (API key OR application default
  # credentials). Everything else is blanked unconditionally.
  #
  # The provider ids are NOT hard-coded here: they are parsed at eval time
  # from the pinned pi package's own auto-generated catalog
  # (`@earendil-works/pi-ai/dist/models.generated.js`, `Object.keys(MODELS)` —
  # the same source `builtinProviders()` in providers/all.js is built from),
  # so the list tracks the pinned pi version automatically. Parsing uses the
  # same read-a-store-path-at-eval-time pattern as `baseTheme` below.
  #
  # Keyless-capable ids are derived from the pinned package too, not
  # hard-coded: the built-in provider modules that wire an OAuth login flow
  # (`auth/oauth/load.js`) or the Vertex ADC path are exactly the keyless
  # ones. If a future pi version adds another OAuth provider, it is exempt
  # automatically.
  keylessBuiltinProviderIds =
    let
      providersDir = "${pi-coding-agent-pkg}/lib/node_modules/pi-monorepo/node_modules/@earendil-works/pi-ai/dist/providers";
      providerJs = id: providersDir + "/${id}.js";
      # anthropic.js resolves ANTHROPIC_AUTH_TOKEN / ANTHROPIC_OAUTH_TOKEN /
      # ANTHROPIC_API_KEY from the env; github-copilot.js, kimi-coding.js,
      # openai-codex.js, openrouter.js and xai.js import an OAuth login flow
      # from auth/oauth/load.js; google-vertex.js authenticates via gcloud
      # application default credentials.
      isKeyless =
        id:
        builtins.match ".*auth/oauth/load\.js.*" (builtins.readFile (providerJs id)) != null
        || id == "google-vertex";
    in
    builtins.filter isKeyless builtinProviderIds;

  blankedBuiltinProviderIds = lib.subtractLists keylessBuiltinProviderIds builtinProviderIds;

  builtinProviderIds =
    let
      modelsGeneratedJs = "${pi-coding-agent-pkg}/lib/node_modules/pi-monorepo/node_modules/@earendil-works/pi-ai/dist/models.generated.js";
      # `builtins.split "\n"` yields alternating string/list parts; keep only
      # the string lines. The `MODELS` object's keys are indented by four
      # spaces and look like:
      #     "anthropic": ANTHROPIC_MODELS,
      # so anchor the match on the leading whitespace + quoted key.
      keyPattern = ''^ +"([a-z0-9-]+)":.*'';
    in
    builtins.map (line: builtins.head (builtins.match keyPattern line)) (
      builtins.filter (line: builtins.match keyPattern line != null) (
        builtins.filter builtins.isString (builtins.split "\n" (builtins.readFile modelsGeneratedJs))
      )
    );

  # Generate a TypeScript extension that registers all providers via
  # pi.registerProvider(). See:
  # https://github.com/badlogic/pi-mono/blob/main/packages/coding-agent/docs/custom-provider.md
  providersExtension =
    let
      providersJson = builtins.toJSON (
        lib.listToAttrs (
          lib.map (p: {
            name = p.key;
            value = p.value;
          }) allProviders
        )
      );
      builtinIdsJson = builtins.toJSON blankedBuiltinProviderIds;
    in
    pkgs.writeText "pi-providers.ts" ''
      // Auto-generated by myconfig.ai.dev.pi-coding-agent. Do not edit by hand.
      import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

      const providers: Record<string, any> = ${providersJson};
      // Sandbox endpoint override (see `litellmBaseUrlEnvVar` in the
      // generating Nix module): a container sandbox reaches the host's
      // loopback-only LiteLLM proxy under a different URL than the host
      // itself (the port-scoped forwarder behind pasta's
      // `--map-guest-addr` target), and pi has ONE config for both. The
      // baked `baseUrl` above is the host URL; an environment that names
      // another one replaces it for that session only.
      const litellmProviderId = "${litellmProviderKey}";
      const litellmBaseUrlOverride = process.env.${litellmBaseUrlEnvVar};
      if (litellmBaseUrlOverride && providers[litellmProviderId]) {
        providers[litellmProviderId].baseUrl = litellmBaseUrlOverride;
      }
      // Built-in provider ids whose model list is blanked (see
      // `blankedBuiltinProviderIds` in the generating Nix module). Registering
      // `models: []` replaces that provider's built-in model list, so only the
      // repo-configured providers above plus the keyless-capable built-ins
      // (OAuth/subscription or ADC auth) remain. Auth and `/login` entries for
      // these providers keep working; they just expose no models.
      const blankedBuiltinProviderIds: string[] = ${builtinIdsJson};

      export default function (pi: ExtensionAPI) {
        for (const [key, config] of Object.entries(providers)) {
          pi.registerProvider(key, config);
        }
        for (const builtinId of blankedBuiltinProviderIds) {
          pi.registerProvider(builtinId, { models: [] });
        }
      }
    '';

  # Generate a TypeScript extension that discovers the litellm proxy's
  # model list at RUNTIME via GET {litellmUrl}/v1/models (the
  # OpenAI-compatible model list litellm serves) instead of baking it into
  # the derivation at eval time. Deployed only when `cfg.litellmUrl != ""`.
  # See the bead `myconfig-99z` and the option
  # `myconfig.ai.dev.pi-coding-agent.litellmUrl` below.
  #
  # Design (pi 0.85.x extension API, docs/extensions.md):
  #   * The extension factory is ASYNC: pi awaits it before session_start,
  #     so the models fetched at load time are available to `--list-models`,
  #     `/model` and `settings.defaultModel` resolution at startup. This is
  #     the documented pattern for "dynamically discovering available
  #     models".
  #   * The provider config also carries `refreshModels`, which pi calls
  #     during model catalog refresh (e.g. when `/model` opens, or via
  #     `pi update --models`); the returned list replaces the extension's
  #     models without persisting anything (we pass no `persist` publication,
  #     keeping `~/.pi/agent/models-store.json` clean and the list truly
  #     dynamic).
  #   * `/litellm-refresh` re-fetches on demand: pi has no public periodic
  #     refresh API, so "dynamic update" = at every startup, at `/model`
  #     catalog refresh, and via the explicit command. That covers changes on
  #     the proxy side without rebuilding the nix packaging.
  #   * The extension is the ONLY registrant of the `${hostname}-litellm`
  #     provider id when active (see the single-owner comment at
  #     `litellmProvider`), so there are no duplicate-provider conflicts with
  #     the build-time `pi-providers.ts` registration.
  #   * Robustness: any fetch/parse failure is logged as a warning and the
  #     provider stays registered with zero models — pi keeps running. In
  #     `print`/`json` mode (no UI) warnings go to stderr instead of
  #     `ctx.ui.notify`.
  #   * Context-window metadata: litellm's /v1/models carries
  #     `max_input_tokens` (context) and `max_output_tokens` per model (it
  #     does NOT serve model_info there — that is /model/info). Both fields
  #     are OPTIONAL in the OpenAI schema; a missing `max_input_tokens` falls
  #     back to the same conservative 128k default the build-time path uses,
  #     a missing `max_output_tokens` falls back to the same
  #     min(contextWindow/4, 64k) derivation. No silent guessing beyond the
  #     documented fallback.
  #
  #   * Headless smoke test (validated for bd myconfig-99z):
  #       nix build the extension:
  #         nix build --impure --expr '(builtins.getFlake (toString ./.)).nixosConfigurations.test-f13.config.home-manager.users.mhuber.home.file.".pi/agent/extensions/myconfig-litellm-models.ts".source'
  #       then, in an empty dir with an isolated PI_CODING_AGENT_DIR:
  #         PI_CODING_AGENT_DIR=/tmp/pi-agent pi --list-models -e <ext.ts>
  #           -> `[litellm-models] registered N model(s) … (dynamic)` + the
  #              fetched models under provider `<hostname>-litellm`.
  #         sed the BASE_URL to a dead port -> pi still starts, logs
  #           `could not fetch …; registered provider … with no models`,
  #           exit 0 (graceful degradation).
  #         pi -p --no-session --provider <hostname>-litellm --model <id> \
  #           -e <ext.ts> "say pong" -> real completion through a
  #           dynamically discovered model.
  litellmModelsExtension =
    let
      # `apiKey` accepts a literal or an env-var reference (`$ENV` /
      # `${ENV}`). A literal "dummy" matches the build-time registration for
      # keyless litellm deployments; with `litellmApiKeyEnv` set, the key is
      # resolved from that env var at request time (never stored in the
      # nix derivation).
      apiKeyConfig =
        if cfg.litellmApiKeyEnv != null then "\"$''${cfg.litellmApiKeyEnv}\"" else "\"dummy\"";
    in
    pkgs.writeText "pi-litellm-models.ts" ''
      // Auto-generated by myconfig.ai.dev.pi-coding-agent. Do not edit by hand.
      //
      // Dynamic litellm model discovery (bd myconfig-99z): fetches
      // GET {BASE_URL}/models at pi startup (async factory - pi awaits it
      // before session_start), registers the litellm provider with the
      // fetched model list, re-fetches on pi model-catalog refresh
      // (`refreshModels`) and on the `/litellm-refresh` command. Zero
      // hardcoded model names: the model list is whatever the proxy serves
      // at runtime. Proxy-side changes are picked up by restarting pi,
      // reopening /model, or running /litellm-refresh - no nix rebuild
      // involved.
      import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

      const PROVIDER_ID = "${litellmProviderKey}";
      const PROVIDER_NAME = "LiteLLM (${osconfig.networking.hostName})";
      // The baked host URL, overridable per session by
      // ${litellmBaseUrlEnvVar} — what a container sandbox sets, whose
      // route to the host proxy is the port-scoped forwarder rather than
      // the host loopback (the sandbox then discovers the model list
      // through that endpoint at runtime, like any other session).
      const BASE_URL = process.env.${litellmBaseUrlEnvVar} || "${cfg.litellmUrl}/v1";
      // Same shape as the build-time registration in myconfig-providers.ts:
      // keyless litellm deployments use a dummy key (litellm ignores it) and
      // no Authorization header. When `litellmApiKeyEnv` is set in the
      // generating module, the value is `$<ENV>` instead: pi resolves the
      // env var at request time and sends it as the bearer token, so the
      // key never enters the nix store.
      const API_KEY: string = ${apiKeyConfig};
      // Keyless loopback proxies need no Authorization header. With an
      // env-var key, send it as `Authorization: Bearer <key>` like any
      // OpenAI-compatible client.
      const AUTH_HEADER: boolean = ${if cfg.litellmApiKeyEnv != null then "true" else "false"};

      interface LitellmModelEntry {
        id: string;
        max_input_tokens?: number;
        max_output_tokens?: number;
      }

      // Conservative fallbacks, mirroring the build-time defaults in the
      // generating Nix module (`mkOpenAiCompatibleProvider` /
      // `deriveMaxOutputTokens`): 128k context; output budget =
      // min(context/4, 64k).
      const FALLBACK_CONTEXT_WINDOW = 131072;
      const FALLBACK_MAX_OUTPUT = 65536;

      function toPiModel(entry: LitellmModelEntry) {
        const contextWindow =
          typeof entry.max_input_tokens === "number" && entry.max_input_tokens > 0
            ? entry.max_input_tokens
            : FALLBACK_CONTEXT_WINDOW;
        const maxTokens =
          typeof entry.max_output_tokens === "number" && entry.max_output_tokens > 0
            ? entry.max_output_tokens
            : Math.min(Math.floor(contextWindow / 4), FALLBACK_MAX_OUTPUT);
        return {
          id: entry.id,
          name: entry.id,
          reasoning: false,
          input: ["text"] as const,
          cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 },
          contextWindow,
          maxTokens,
        };
      }

      async function fetchModels(signal: AbortSignal): Promise<LitellmModelEntry[]> {
        const response = await fetch(BASE_URL + "/models", { signal });
        if (!response.ok) {
          throw new Error("litellm /v1/models returned HTTP " + response.status);
        }
        const payload = (await response.json()) as { data?: LitellmModelEntry[] };
        const models = Array.isArray(payload.data) ? payload.data : [];
        return models.filter((m) => typeof m?.id === "string" && m.id.length > 0);
      }

      function log(message: string) {
        // Headless (print/json/rpc startup) surfaces have no ctx.ui yet;
        // stderr always works and keeps the failure diagnosable.
        process.stderr.write("[litellm-models] " + message + "\n");
      }

      function providerConfig(models: ReturnType<typeof toPiModel>[]) {
        return {
          name: PROVIDER_NAME,
          baseUrl: BASE_URL,
          api: "openai-completions" as const,
          apiKey: API_KEY,
          authHeader: AUTH_HEADER,
          models,
        };
      }

      export default async function (pi: ExtensionAPI) {
        // Startup fetch. pi awaits async factories before session_start, so
        // the fetched models are available to /model, --list-models and
        // default-model resolution. A failure here must NOT brick the
        // session: register the provider with zero models and continue.
        let models: ReturnType<typeof toPiModel>[] = [];
        let startupError: string | undefined;
        try {
          models = (await fetchModels(AbortSignal.timeout(10_000))).map(toPiModel);
        } catch (error) {
          startupError = error instanceof Error ? error.message : String(error);
        }
        pi.registerProvider(PROVIDER_ID, {
          ...providerConfig(models),
          // Re-fetch on pi model-catalog refresh (e.g. the /model picker,
          // `pi update --models`). Called in a restore phase with
          // `allowNetwork: false` first: returning `undefined` there keeps
          // the current (startup-fetched) list - an empty array would be
          // truthy and would REPLACE the models with none. Nothing is
          // persisted (no `context.publish({ persist })`), so the next
          // startup re-fetches anyway.
          refreshModels: async (context: any) => {
            if (!context.allowNetwork || context.signal.aborted) {
              return undefined;
            }
            const entries = await fetchModels(context.signal);
            return entries.map(toPiModel);
          },
        });
        if (startupError === undefined) {
          log("registered " + models.length + " model(s) from " + BASE_URL + " (dynamic)");
        } else {
          log(
            "could not fetch " + BASE_URL + "/models (" + startupError + "); " +
            "registered provider \"" + PROVIDER_ID + "\" with no models; " +
            "retry via /litellm-refresh"
          );
        }

        // Explicit in-session refresh: force a targeted catalog refresh of
        // just this provider through pi's model registry (same code path
        // as the /model picker), which calls the `refreshModels` callback
        // above and keeps the previous list on failure.
        pi.registerCommand("litellm-refresh", {
          description: "Re-fetch the litellm model list from /v1/models",
          handler: async (_args: string, ctx: any) => {
            try {
              const result = await ctx.modelRegistry.refresh({
                providers: [PROVIDER_ID],
                allowNetwork: true,
                force: true,
                signal: AbortSignal.timeout(10_000),
              });
              if (result.aborted) {
                throw new Error("refresh timed out");
              }
              const errors = Array.from(result.errors.entries());
              if (errors.length > 0) {
                throw new Error(errors.map(([id, error]) => id + ": " + error.message).join("; "));
              }
              const count = ctx.modelRegistry
                .getAll()
                .filter((model: any) => model.provider === PROVIDER_ID).length;
              const note = "refreshed: " + count + " model(s) from " + BASE_URL;
              if (ctx?.hasUI) ctx.ui.notify("litellm-models: " + note, "info");
              log(note);
            } catch (error) {
              const message = error instanceof Error ? error.message : String(error);
              const note = "refresh failed (" + message + "); keeping previous model list";
              if (ctx?.hasUI) ctx.ui.notify("litellm-models: " + note, "warning");
              log(note);
            }
          },
        });
      }
    '';

  # Generated extension persisting `cfg.defaultModel` in NIX. Without a
  # nix-side default, a fresh pi session in a sandbox (where
  # `~/.pi/agent/settings.json` is per-sandbox ephemera — see the mysbx
  # comment above) falls through pi's resolution chain to the built-in
  # per-provider fallback table, whose first entry is anthropic's current
  # flagship Opus. This extension applies the configured model on every
  # session start instead:
  #
  #   * `session_start` with reason "startup"/"new" only — "resume",
  #     "fork" and "reload" must keep the session's own recorded model
  #     (pi restores it from the session transcript), and a reload keeps
  #     whatever the user interactively selected with `/model` in that
  #     very session.
  #   * `pi.setModel()` (session-local, no `persist`): `settings.json`'s
  #     `defaultModel` stays owned by `/model` + `Ctrl+S`, and the nix config
  #     cannot race the in-memory SettingsManager's file writes.
  #   * Fail-soft, mirroring the litellm-models extension: an unknown
  #     provider/model (e.g. a nix config that predates a proxy-side model
  #     rename) logs to stderr and leaves pi's own resolution in place —
  #     a session must never refuse to start over a stale default.
  #   * A model explicitly chosen via CLI `--model` (or `--provider`) must
  #     NOT be overridden: pi records it in the session, but `session_start`
  #     fires before the first model_change entry exists, so the check
  #    below reads pi's own command line instead of the transcript.
  defaultModelExtension =
    if cfg.defaultModel == null then
      null
    else
      pkgs.writeText "pi-default-model.ts" ''
        // Auto-generated by myconfig.ai.dev.pi-coding-agent. Do not edit by hand.
        import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

        const DEFAULT_PROVIDER = "${cfg.defaultModel.provider}";
        const DEFAULT_MODEL = "${cfg.defaultModel.model}";

        function log(message: string) {
          process.stderr.write("[default-model] " + message + "\n");
        }

        function hasCliModelOverride(): boolean {
          const argv = process.argv.slice(2);
          let hasModel = false;
          let hasProvider = false;
          for (const arg of argv) {
            if (arg === "--model" || arg === "-m") hasModel = true;
            if (arg === "--provider") hasProvider = true;
          }
          // `--provider` without `--model` is a pi CLI error anyway; either
          // flag means the user (or a wrapper) pinned the model explicitly.
          return hasModel || hasProvider;
        }

        export default function (pi: ExtensionAPI) {
          pi.on("session_start", async (event: any, ctx: any) => {
            if (event.reason !== "startup" && event.reason !== "new") return;
            if (hasCliModelOverride()) {
              log("cli --model/--provider present, keeping it");
              return;
            }
            const model = ctx.modelRegistry.find(DEFAULT_PROVIDER, DEFAULT_MODEL);
            if (!model) {
              log(
                "configured default " + DEFAULT_PROVIDER + "/" + DEFAULT_MODEL +
                " is not registered; keeping pi's own model resolution",
              );
              return;
            }
            const ok = await pi.setModel(model);
            if (!ok) {
              log(
                "no auth configured for " + DEFAULT_PROVIDER + "/" + DEFAULT_MODEL +
                "; keeping pi's own model resolution",
              );
              return;
            }
            log("session default: " + DEFAULT_PROVIDER + "/" + DEFAULT_MODEL);
          });
        }
      '';

  # Custom theme to flag *un-sandboxed* pi sessions. The plain `pi` wrapper
  # and a mysbx session share the same `~/.pi/agent/settings.json` (hence the
  # same theme), so the two are otherwise indistinguishable in the TUI. The
  # sandboxed session (mysbx sets `PI_JAIL_MARKER=1`) is the safe default and
  # keeps the user's normal theme untouched; only the *un-sandboxed* session
  # is recolored, giving its editor box border a red warning color.
  #
  # The editor border color is not the static `border` token; it is driven by
  # the current *thinking level* via `theme.getThinkingBorderColor(level)`,
  # which maps each level to a dedicated `thinking*` token. See
  # `dist/modes/interactive/theme/theme.js`. To recolor the border regardless
  # of thinking level we override every `thinking*` token with the marker
  # color. `bashMode` is intentionally left untouched so bash mode keeps its
  # normal border color.
  #
  # The `myconfig-jail-marker.ts` extension (below) reads `PI_JAIL_MARKER`
  # (set to "1" by the mysbx user config layer) and, only when it is *not* set,
  # switches to this theme on session start. Crucially it switches via the
  # Theme *object* overload of `ctx.ui.setTheme()`, which is in-memory only
  # and does NOT persist: the string overload writes the theme into the
  # shared `~/.pi/agent/settings.json`, which previously leaked the red marker
  # into the jailed session (both wrappers share `~/.pi`). The extension also
  # self-heals that legacy corruption once (see its header comment).
  pi-coding-agent-pkg = pkgs.nixos-unstable.pi-coding-agent;
  # The nodejs pi's wrapper execs by store path (the package's one
  # buildInput) — the version fact the mysbx `nodeMajor` derivation
  # below reads.
  piNodeJs = lib.findSingle (
    x: lib.hasPrefix "nodejs-" (x.name or "")
  ) null null pi-coding-agent-pkg.buildInputs;

  baseTheme = builtins.fromJSON (
    builtins.readFile "${pi-coding-agent-pkg}/lib/node_modules/pi-monorepo/dist/modes/interactive/theme/light.json"
  );

  # Produce a forked theme: same as `light` but with all thinking-level border
  # tokens overridden to `color`, and renamed to `name`.
  mkForkedTheme =
    {
      name,
      color,
    }:
    let
      forked = baseTheme // {
        inherit name;
        # Inject the marker color as a var, then point every border-driving
        # token at it.
        vars = (baseTheme.vars or { }) // {
          jailMarker = color;
        };
        colors = (baseTheme.colors or { }) // {
          thinkingOff = "jailMarker";
          thinkingMinimal = "jailMarker";
          thinkingLow = "jailMarker";
          thinkingMedium = "jailMarker";
          thinkingHigh = "jailMarker";
          thinkingXhigh = "jailMarker";
          thinkingMax = "jailMarker";
        };
      };
    in
    pkgs.writeText "pi-theme-${name}.json" (builtins.toJSON forked);

  # Red border for the un-jailed (unsandboxed) session. The jailed session
  # keeps the user's default theme unchanged.
  unjailedTheme = mkForkedTheme {
    name = "unjailed";
    color = "#cc2222";
  };

  jailMarkerExtension = pkgs.writeText "pi-jail-marker.ts" ''
    // Auto-generated by myconfig.ai.dev.pi-coding-agent. Do not edit by hand.
    //
    // Visually distinguishes the sandboxed sessions (mysbx, which sets
    // PI_JAIL_MARKER=1 via the generated user config layer `env`) from the
    // un-sandboxed `pi` wrapper (which does not). The un-sandboxed session
    // is the dangerous one (full home access), so it gets a red editor
    // border via the `unjailed` theme; every mysbx session keeps the user's
    // normal theme.
    //
    // Both wrappers share ~/.pi (and thus ~/.pi/agent/settings.json), so the
    // marker must be SESSION-LOCAL and must never be written to settings.json.
    // An earlier version of this extension called ctx.ui.setTheme("unjailed")
    // with a *string*. In the TUI the string overload of setTheme() persists
    // the theme to settings.json (interactive-mode.js calls
    // settingsManager.setTheme(), which writeFileSync()s the file). That
    // leaked the red marker into the shared settings, so the jailed session
    // inherited it and also turned red -- the "jail detection does not work"
    // symptom. (The PI_JAIL_MARKER check itself worked; the persisted theme was
    // the culprit.)
    //
    // Fix:
    //   * Apply the marker via the Theme *object* overload of setTheme()
    //     (ctx.ui.getTheme() -> ctx.ui.setTheme(themeObject)). The object
    //     overload uses setThemeInstance(), which is in-memory only and does
    //     NOT persist, so the marker never reaches settings.json.
    //   * Self-heal leftover corruption: if settings.json still has the marker
    //     theme persisted (from the old buggy extension), overwrite it once
    //     with a real built-in theme via the persisting string overload. This
    //     repairs both the on-disk settings and the in-memory SettingsManager,
    //     so it cannot be re-corrupted. "dark" is pi's own fallback default;
    //     users who prefer another theme can set it via /settings afterwards.
    import { readFileSync } from "node:fs";
    import { homedir } from "node:os";
    import { join } from "node:path";
    import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

    const MARKER_THEME = "unjailed"; // red-border theme, a fork of `light`
    const RECOVERY_THEME = "dark"; // pi's built-in fallback; repairs settings

    // Read the persisted `theme` field directly (read-only) to detect leftover
    // marker corruption. We never write settings.json ourselves -- that would
    // race the in-memory SettingsManager, which re-derives the file on save().
    function readPersistedTheme(): string | undefined {
      const agentDir =
        process.env.PI_CODING_AGENT_DIR ?? join(homedir(), ".pi", "agent");
      try {
        const raw = readFileSync(join(agentDir, "settings.json"), "utf-8");
        const parsed = JSON.parse(raw);
        return typeof parsed?.theme === "string" ? parsed.theme : undefined;
      } catch {
        return undefined;
      }
    }

    export default function (pi: ExtensionAPI) {
      pi.on("session_start", async (_event: any, ctx: any) => {
        // The marker is a TUI-only visual cue; setTheme() is a no-op outside
        // TUI, so there is nothing to do in rpc/print modes.
        if (ctx.mode !== "tui") return;

        const jailed = process.env.PI_JAIL_MARKER === "1";

        // Repair settings.json if the old buggy extension left the marker
        // theme persisted in it. The string overload persists (updating both
        // the on-disk settings and the in-memory SettingsManager), so this is a
        // one-time fix that a later settings save cannot undo.
        if (readPersistedTheme() === MARKER_THEME) {
          ctx.ui.setTheme(RECOVERY_THEME);
          ctx.ui.notify(
            `jail-marker: repaired settings.json (theme was stuck on the "''${MARKER_THEME}" marker); default reset to "''${RECOVERY_THEME}". Set your preferred theme via /settings.`,
            "info",
          );
        }

        if (jailed) {
          // Sandboxed session: keep the user's normal theme. Do not apply the
          // red marker inside the jail. (If we just repaired corruption above,
          // the session now shows RECOVERY_THEME; otherwise the user's theme.)
          return;
        }

        // Un-sandboxed session: show the red warning border for this session
        // only. Passing a Theme *object* (not a name string) to setTheme()
        // takes the setThemeInstance() path, which is in-memory only and does
        // not persist -- so the marker never leaks into the shared settings.
        const marker = ctx.ui.getTheme(MARKER_THEME);
        if (marker) {
          ctx.ui.setTheme(marker);
        } else {
          ctx.ui.notify(
            `jail-marker: theme "''${MARKER_THEME}" not found; is ~/.pi/agent/themes/unjailed.json deployed?`,
            "error",
          );
        }
      });
    }
  '';

  # The mysbx status extension: renders a line in the pi status bar
  # describing WHERE this session runs, built from the `MYSBX_*`
  # identity variables the mysbx backends set inside every sandbox
  # (mysbx-rs/src/{bwrap,krun,podman_gvisor}.rs, the infrastructure
  # block of docs/design/config.md D14 — a config layer cannot spoof
  # them). The three variables every backend sets:
  #
  #   MYSBX_BACKEND     which sandbox backend built this run
  #                     ("bubblewrap", "nono", "podman-gvisor",
  #                     "podman-krun"/"krun")
  #   MYSBX_MULTIPLEXER which interactive payload the sandbox starts
  #                     ("tmux", "workmux", "herdr", "aoe", "orca",
  #                     "none")
  #   MYSBX_HOST        the OUTER host's hostname — the physical
  #                     machine the sandbox runs on (bd
  #                     myconfig-5wf); the sandbox's own hostname is
  #                     backend cover and names nothing
  #   MYSBX_SESSION_NAME the clone's session name — set only by
  #                     session-mode (Clone workspace) runs of a
  #                     session-starting multiplexer
  #
  # The line (e.g. "mysbx: bubblewrap via aoe") is set once at
  # `session_start` via `ctx.ui.setStatus()`, the same status-bar
  # mechanism the gondolin/plan-mode examples use. Outside a mysbx
  # sandbox (`MYSBX_BACKEND` unset — a plain `pi` session) the extension
  # renders nothing at all, so it is deployable unconditionally.
  #
  # The sandbox's fish right prompt renders the same line from the same
  # variables (mysbx/nix/shell-config.nix `sandboxIdentity`); keep the
  # two in step.
  mysbxStatusExtension = pkgs.writeText "pi-mysbx-status.ts" ''
    // Auto-generated by myconfig.ai.dev.pi-coding-agent. Do not edit by hand.
    //
    // Renders the mysbx sandbox identity (MYSBX_BACKEND /
    // MYSBX_MULTIPLEXER / MYSBX_NETWORK / MYSBX_SESSION_NAME /
    // MYSBX_HOST) as a line in the pi status bar, so a session inside
    // a mysbx sandbox says WHERE it runs and WHAT its network grants
    // instead of the agent having to probe. Unset = not a mysbx
    // sandbox = render nothing.
    import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

    const STATUS_KEY = "mysbx";

    export default function (pi: ExtensionAPI) {
      pi.on("session_start", async (_event: any, ctx: any) => {
        // The status bar is a TUI surface; print/json/rpc modes have
        // no footer to render it in.
        if (ctx.mode !== "tui") return;

        const backend = process.env.MYSBX_BACKEND;
        if (!backend) return; // not inside a mysbx sandbox

        // The line, most general form:
        //   mysbx: <backend> via <multiplexer> [net <policy>] [on <host>] [as <session>]
        // Segments are added only when their variables exist and say
        // something beyond the built-in default: the network policy
        // renders only when it narrows the default (`none`,
        // `proxy-only` — `shared` would be noise, like `via none`).
        let line = "mysbx: " + backend;
        const multiplexer = process.env.MYSBX_MULTIPLEXER;
        if (multiplexer && multiplexer !== "none") {
          line += " via " + multiplexer;
        }
        const network = process.env.MYSBX_NETWORK;
        if (network && network !== "shared") {
          line += " net " + network;
        }
        const host = process.env.MYSBX_HOST;
        if (host) {
          line += " on " + host;
        }
        const session = process.env.MYSBX_SESSION_NAME;
        if (session) {
          line += " as " + session;
        }

        ctx.ui.setStatus(STATUS_KEY, ctx.ui.theme.fg("muted", line));
      });
    }
  '';

  # The mysbx trust extension: answers pi's `project_trust` event
  # with a per-process "yes" so a sandboxed session never stalls on
  # the trust prompt. Inside mysbx the home is a throwaway tmpfs
  # (`/mysbx-home`, D14 — only `sessions/` is a state entry), so a
  # saved decision in `~/.pi/agent/trust.json` cannot survive a
  # teardown and pi re-asks on every launch, even though the repo
  # ships trust-gated resources (.agents/skills, .pi) that the agent
  # needs loaded. Auto-trust is a property of the sandbox, not of
  # the user: the workspace mount is the confinement boundary, so
  # trust-gated code can only act within the jail. Deploying this
  # extension to the host (`home.file`) would silence the prompt for
  # UN-jailed pi sessions too, which must stay ask-first — hence it
  # rides into the sandbox tree via `extraFiles` only (see
  # `mysbxSandboxConfig` below) and `mysbx-pi-mounts-test` asserts
  # the host never deploys it.
  mysbxTrustExtension = pkgs.writeText "pi-mysbx-trust.ts" ''
    // Auto-generated by myconfig.ai.dev.pi-coding-agent. Do not edit by hand.
    //
    // Sandbox-only: mounted via extraFiles, never home.file — inside a
    // mysbx sandbox the workspace mount is the confinement boundary, so
    // trusting the project grants nothing the jail does not already
    // enforce; an un-jailed host session must keep the ask-first prompt.
    import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

    export default function (pi: ExtensionAPI) {
      pi.on("project_trust", async () => ({ trusted: "yes", remember: false }));
    }
  '';

  # The subagent example extension (examples/extensions/subagent/) ships as
  # a multi-file extension plus sample agent definitions and workflow prompt
  # templates. pi discovers each from a distinct location (see the example's
  # README.md):
  #   * the extension entry point (`index.ts`) and its sibling `agents.ts`
  #     helper go to ~/.pi/agent/extensions/subagent/ (pi loads
  #     `*/index.ts`; `agents.ts` is imported via a relative "./agents.ts"
  #     specifier, so it must sit next to `index.ts`);
  #   * sample agent markdown files go to ~/.pi/agent/agents/ (discovered by
  #     the extension's `discoverAgents()`, which reads `getAgentDir()/agents`);
  #   * workflow prompt templates go to ~/.pi/agent/prompts/ (invoked via
  #     `/implement`, `/scout-and-plan`, `/implement-and-review`).
  # The file list is enumerated with `builtins.readDir` rather than
  # hardcoded, so files added upstream are picked up automatically. Reading
  # the package store path at eval time is the same pattern already used
  # above for `baseTheme` (`builtins.readFile` of the theme JSON).
  subagentExampleDir = "${pi-coding-agent-pkg}/lib/node_modules/pi-monorepo/examples/extensions/subagent";

  subagentExtensionFiles = lib.mapAttrs' (
    name: _:
    lib.nameValuePair ".pi/agent/extensions/subagent/${name}" {
      source = "${subagentExampleDir}/${name}";
    }
  ) (lib.filterAttrs (name: _: lib.hasSuffix ".ts" name) (builtins.readDir subagentExampleDir));

  # The upstream sample agent `.md` files hard-code a `model:` frontmatter
  # field (e.g. `claude-haiku-4-5`, `claude-sonnet-4-5`) pointing at Anthropic
  # models that are not available through this config's local providers. The
  # field is optional in the subagent extension: `agents.ts` only requires
  # `name` and `description`, and `index.ts` passes `--model` to the spawned
  # subprocess only when `agent.model` is set. With the field absent the
  # subagent inherits the user's configured default model (from
  # `~/.pi/agent/settings.json` or pi's built-in default) -- the desired
  # behavior for a sample-extension deployment. Rather than fork the files to
  # hard-code a *different* model name (which would just move the
  # hard-coding), strip the `model:` line entirely so the sample agents stay
  # close to upstream and always follow the user's default model. The store
  # originals are read-only, so each file is re-emitted via `pkgs.writeText`
  # with the offending line filtered out.
  subagentAgentFiles =
    let
      agentsDir = "${subagentExampleDir}/agents";
      # Drop any frontmatter `model:` line so the subagent falls back to
      # the user's default model instead of a hard-coded upstream model.
      # The sample files have flat (un-indented) frontmatter and their
      # system-prompt bodies contain no line starting with `model:`, so a
      # simple prefix filter is exact here.
      stripModelLine =
        content:
        lib.concatStringsSep "\n" (
          lib.filter (line: !(lib.hasPrefix "model:" line)) (lib.splitString "\n" content)
        );
    in
    lib.mapAttrs' (
      name: _:
      lib.nameValuePair ".pi/agent/agents/${name}" {
        source = pkgs.writeText name (stripModelLine (builtins.readFile "${agentsDir}/${name}"));
      }
    ) (lib.filterAttrs (name: _: lib.hasSuffix ".md" name) (builtins.readDir agentsDir));

  subagentPromptFiles =
    lib.mapAttrs'
      (
        name: _:
        lib.nameValuePair ".pi/agent/prompts/${name}" {
          source = "${subagentExampleDir}/prompts/${name}";
        }
      )
      (
        lib.filterAttrs (name: _: lib.hasSuffix ".md" name) (
          builtins.readDir "${subagentExampleDir}/prompts"
        )
      );

  # The handoff example (examples/extensions/handoff.ts) is a single-file
  # extension that adds a `/handoff` command for transferring context to a
  # new focused session instead of lossy compaction. Deployed as a global
  # single-file extension at ~/.pi/agent/extensions/handoff.ts (pi
  # auto-discovers `~/.pi/agent/extensions/*.ts`). Its runtime value imports
  # (`@earendil-works/pi-ai`, `@earendil-works/pi-coding-agent`, ...) resolve
  # against pi's own node_modules at load time, unlike the generated
  # `myconfig-*` extensions above which only use type-only imports.
  handoffExtension = "${pi-coding-agent-pkg}/lib/node_modules/pi-monorepo/examples/extensions/handoff.ts";

  # `pi-token-speed` (https://github.com/gsanhueza/pi-token-speed): a
  # third-party pi extension that displays real-time tokens-per-second
  # (TPS), time-to-first-token (TTFT) and color-coded speed tiers in the
  # pi status bar while a response is streaming, plus a `/tps` settings
  # command. The upstream source is pinned by nvfetcher
  # (`nvfetcher.toml` / `_sources/generated.nix`, like the skills sources);
  # upstream has no npm runtime dependencies (its peer deps on pi itself are
  # provided by pi's extension loader as virtual modules), so the fetched
  # source tree is deployed as-is - no `npm install` required.
  #
  # Deployed as a multi-file global extension at
  # `~/.pi/agent/extensions/pi-token-speed/`: pi loads `*/index.ts`, and
  # upstream 0.12.0 (`46a3dd4`) keeps it at `src/index.ts` (its package
  # manifest declares `"pi": { "extensions": ["./src/index.ts"] }`), so a
  # thin loader re-exports the real entry while the sibling `./src/...`
  # imports keep resolving via relative specifiers. Vendor files upstream
  # does not need at runtime (tests, package.json, tsconfig) are dropped
  # for a clean read-only extension directory. Upstream targets
  # pi >= 0.85.1 (peerDependencies), matching the pinned
  # `pkgs.nixos-unstable.pi-coding-agent` here.
  tokenSpeedSrc = (pkgs.callPackage ../../../../_sources/generated.nix { }).pi-token-speed.src;

  tokenSpeedExtension = pkgs.runCommand "pi-token-speed-extension" { } ''
    mkdir -p $out
    cp -r ${tokenSpeedSrc}/src $out/src
    printf 'export { default } from "./src/index.ts";\n' > $out/index.ts
  '';

  # The `/mysbx-inform` skill (bd myconfig-a2nj): a GENERATED skill,
  # not a static one — its whole point is that the sandbox's per-run
  # state is BAKED into the skill text, so the in-sandbox agent never
  # has to look up the `MYSBX_*` environment. The facts that vary per
  # run (backend, network policy, egress endpoint, host, multiplexer,
  # session) are only known inside the sandbox, after the backend set
  # the `MYSBX_*` identity variables (config.md D14's infrastructure
  # class) — so a host-deployed static skill cannot carry them.
  #
  # Instead the deployed extension (mysbx-inform-extension.ts) renders
  # the template (mysbx-inform-template.md) from those variables at
  # pi startup (`resources_discover`: the extension event for
  # contributing skill paths), writes the result below the agent dir
  # (inside a sandbox that is the writable tmpfs home; on the host it
  # is ~/.pi/agent, where ~/.pi is persistent user data) and registers
  # it as a skill — `/skill:mysbx-inform` then presents the run's own
  # backend capabilities and network policy as prose, with the design
  # docs referenced for deeper digging.
  #
  # Deployed unconditionally like the status extension: outside a
  # mysbx sandbox MYSBX_BACKEND is unset and the handler contributes
  # nothing, so plain pi sessions are unaffected.
  mysbxInformExtension =
    let
      # The template as a JSON string literal: JSON escaping keeps
      # every markdown character (backticks, quotes, newlines) safe
      # inside the generated TypeScript source, with no Nix-string
      # escaping of the template body to get right.
      templateJson = builtins.toJSON (builtins.readFile ./mysbx-inform-template.md);
      # The extension body with the template substituted for the
      # `{{template}}` placeholder. `replaceStrings` on the literal
      # marker avoids `${...}` interpolation entirely (the body
      # contains no other `{{template}}` text).
      body = builtins.replaceStrings [ "{{template}}" ] [ templateJson ] (
        builtins.readFile ./mysbx-inform-extension.ts
      );
    in
    pkgs.writeText "pi-mysbx-inform.ts" body;

  # Thin workmux-driven worktree launcher. `mkWorkmuxWorktree` builds the
  # user-facing `pi-worktree` wrapper (which requires tmux and calls
  # `workmux add --agent pi`).
  mkWorkmuxWorktree = callLib ../../fns/workmux-worktree.nix;
  piWorktree = mkWorkmuxWorktree {
    name = "pi-worktree";
    agentName = "pi";
    agentType = "pi";
    innerPkg = pkgs.nixos-unstable.pi-coding-agent;
    workmuxPkg = osconfig.myconfig.ai.dev.workmux.package;
    mainRepoEnv = "WORKTREE_MAIN_REPO";
    gitDirEnv = "WORKTREE_GIT_DIR";
  };

  # --- mysbx integration (../../../../mysbx) -------------------------------------
  #
  # pi is the first coding agent wired into the `mysbx` sandbox tier. Three
  # things are needed for a usable session inside a mysbx sandbox:
  #
  #   1. the `pi` binary on the sandbox PATH -> `myconfig.ai.dev.mysbx.extraTools`
  #   2. pi's *configuration* visible inside the sandbox -> read-only mounts
  #      in the generated user config layer (`…mysbx.config.mounts`), each
  #      bound from a self-contained store tree of dereferenced copies built
  #      by the shared `nix/sandbox-config.nix` helper of the mysbx flake (the sandbox
  #      binds real files because the podman-gvisor container mounts nothing
  #      from the host /nix/store — see the helper for the full rationale)
  #   3. the jail-marker extension must NOT flag the session as un-jailed:
  #      a mysbx sandbox is a jail, so the generated user layer also sets
  #      `PI_JAIL_MARKER=1` for every mysbx run (`…mysbx.config.env`).
  #      Without it the red `unjailed` border
  #      would apply to mysbx sessions: mysbx forwards only
  #      TERM/COLORTERM/LANG/LC_ALL/EDITOR/VISUAL
  #      (../../../../mysbx/mysbx-rs/src/lib.rs `FORWARDED_ENV_VARS`), so the
  #      extension would see no marker and misclassify the session.
  #
  #   4. pi's *sessions* survive the sandbox -> a `state-dirs` entry
  #      (`…mysbx.config.stateDirs`, ../../../../mysbx/docs/design/config.md D15).
  #      pi keeps every runtime artefact below `$HOME/.pi/agent` (its
  #      `getAgentDir()`: `sessions/`, `settings.json`, `trust.json`,
  #      `auth.json`, `models.json` — no XDG directories), and `HOME` is
  #      the throwaway tmpfs `/mysbx-home` inside the sandbox, so without
  #      this entry every session died with its sandbox and `pi -c`/`-r`
  #      inside mysbx found nothing — the gap this closes. Only
  #      `sessions/` is persisted, exactly like opencode persists its
  #      session store: `settings.json`/`auth.json`/`trust.json` are
  #      per-repo sandbox ephemera (credentials deliberately never reach
  #      the sidecar, and a state entry is always a directory, so the
  #      files could not be persisted this way anyway).
  #
  #   5. pi's MCP server definitions reach the sandbox -> the rendered
  #      `~/.pi/agent/mcp.json` (bd myconfig-9y4c). Source is
  #      home-manager's `programs.mcp.servers` registry, rendered by the
  #      shared `../pi-mcp-servers.nix` generator and mounted from the
  #      sandbox tree via `extraFiles` — never `home.file`, so the
  #      host's copy stays runtime-managed via `pi mcp add`.
  #
  # What is mounted: one read-only mount per pi-config subtree, from the
  # self-contained store tree built by the shared `sandbox-config.nix`
  # helper (NOT the host home — see `mysbxSandboxConfig` below).
  # `~/.pi` itself is deliberately NOT mounted — it is pi's
  # writable state directory (sessions, settings, credentials); inside
  # the sandbox it stays the throwaway tmpfs home except for the
  # persisted `sessions/` subtree. Persisting a WIDER state entry (`.pi`
  # or `.pi/agent`) is not an option: the read-only config mounts below
  # would land inside a writable state dest, which
  # `check_symlinkable_dests` refuses
  # (../../../../mysbx/mysbx-rs/src/bwrap.rs) — payload-writable content must
  # never sit above a mount point.

  # The pi-config mounts for the mysbx sandbox, built by the shared
  # helper (../../../mysbx flake lib (nix/sandbox-config.nix) — the dereferenced-store-
  # tree machinery that landed here with bd myconfig-576, extracted into
  # the mysbx library with bd myconfig-ooh so other tool modules can
  # mount their home-manager-deployed config the same way). The helper
  # builds its copies from home-manager's merged `home.file` — the
  # single source of truth, so the sandbox tree can never drift from
  # what the host deploys — and its anti-circularity argument applies
  # here verbatim: the result feeds `…mysbx.config.mounts`, which the
  # mysbx module renders into `home.file` keyed by `.config/mysbx/…`, a
  # prefix no subtree below matches (Nix is lazy, so `mkIf`-gating the
  # only reference below suffices).
  #
  # Skills are the one exception to "home.file is the source of truth":
  # when the agent-skills framework manages them, they are rsynced at
  # ACTIVATION time, not linked from `home.file`. The framework exposes
  # per-target bundles (`programs.agent-skills.targetBundlePaths`), so the
  # exact tree the host syncs to `~/.agents/skills` (the `agents` target
  # pi reads) is passed in via `extraFiles` — read from the user whose
  # config layer mysbx generates, lazily, like `hmHomeFile` consumers.
  # The mysbx trust extension rides along the same way: `extraFiles`
  # exists exactly for sandbox-only content, and the extension must NOT
  # land in `home.file` (it would auto-trust un-jailed host sessions;
  # `mysbx-pi-mounts-test` asserts this stays true).
  mysbxPiSkillsBundle =
    let
      hmSkills = config.home-manager.users.mhuber.programs.agent-skills or null;
    in
    if hmSkills == null || !hmSkills.enable then
      { }
    else if !(hmSkills.targetBundlePaths ? agents) then
      { }
    else
      { ".agents/skills" = hmSkills.targetBundlePaths.agents; };
  mysbxSandboxConfig = sandboxConfigLib.mkSandboxConfig {
    homeFile = config.home-manager.users.mhuber.home.file;
    homeDirectory = config.home-manager.users.mhuber.home.homeDirectory;
    subtrees = mysbxPiSubtrees;
    name = "pi-sandbox-config";
    # `mysbxTrustExtension` is the sanctioned-exception pattern this
    # parameter exists for: content the sandbox needs but the host must
    # never deploy. Keep it OUT of `home.file` (see the comment above).
    # pi's mcp.json rides the same way (bd myconfig-9y4c): the host's
    # copy stays runtime-managed via `pi mcp add`, the sandbox gets the
    # registry rendered from the shared generator.
    extraFiles =
      (lib.optionalAttrs piMcp.enable { ${piMcp.path} = piMcp.json; })
      // mysbxPiSkillsBundle
      // (builtins.listToAttrs (
        map (f: lib.nameValuePair f mysbxTrustExtension) mysbxPiSandboxOnlyFiles
      ));
  };

  mysbxPiMounts = mysbxSandboxConfig.mounts;
in
{
  options.myconfig = with lib; {
    ai.dev.pi-coding-agent = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          pi coding agent, the `pi-worktree` workmux wrapper and the
          generated pi extensions (providers, jail marker, subagent/handoff
          examples).

          When a litellm proxy is configured
          (`services.litellm.enable`), the litellm provider's model list is
          discovered DYNAMICALLY at pi runtime via
          `myconfig.ai.dev.pi-coding-agent.litellmUrl` (generated
          `myconfig-litellm-models.ts` extension: GET {url}/v1/models at
          startup, on catalog refresh and via the `/litellm-refresh`
          command) — see `litellmUrl` and the bead `myconfig-99z`. Set
          `litellmUrl = ""` to fall back to the build-time baked model
          list.
        '';
      };

      # Dynamic litellm model discovery (bd myconfig-99z). When non-empty,
      # the generated `myconfig-litellm-models.ts` extension is deployed and
      # owns the `${hostname}-litellm` provider: it fetches
      # GET {url}/v1/models at pi startup (and on catalog refresh / the
      # `/litellm-refresh` command) and registers the models it finds — no
      # build-time baking of litellm model lists. The build-time provider
      # registration (`pi-providers.ts`) is skipped for litellm in that case,
      # so the two never conflict.
      litellmUrl = mkOption {
        type = types.str;
        description = ''
          Base URL of an OpenAI-compatible litellm proxy whose model list
          pi discovers at RUNTIME via `GET {url}/v1/models` (generated
          `myconfig-litellm-models.ts` extension). The model list is never
          baked into the derivation; proxy-side changes appear after a pi
          restart, a model-catalog refresh or the `/litellm-refresh`
          command — no nix rebuild involved.

          Empty string (default) disables the dynamic plugin and keeps the
          build-time litellm provider registration (the full model list
          evaluated from `services.litellm.settings.model_list` at nix
          eval time).
        '';
        default =
          if osconfig.services.litellm.enable then
            "http://${
              if osconfig.services.litellm.host == "0.0.0.0" then "localhost" else osconfig.services.litellm.host
            }:${toString osconfig.services.litellm.port}"
          else
            "";
        defaultText = literalExpression "(http://<services.litellm.host>:<services.litellm.port> when services.litellm.enable, \"\" otherwise)";
      };

      # Optional name of an environment variable that carries the litellm
      # proxy's API key. pi's `apiKey` config accepts `$ENV` references and
      # resolves them at request time, so the key never enters the store.
      # Null (default) registers the literal "dummy" key — correct for the
      # keyless loopback-only deployments this repo configures (see
      # hosts/shared.litellm.proxy.nix: no master_key configured).
      litellmApiKeyEnv = mkOption {
        type = types.nullOr types.str;
        description = ''
          Name of an environment variable holding the litellm proxy's API
          key. The generated extension registers `$<litellmApiKeyEnv>` as
          the provider `apiKey`, which pi resolves from the environment at
          request time (and sends as the Authorization header).

          Null (default) registers the literal dummy key, matching the
          build-time provider registration for keyless litellm
          deployments.
        '';
        default = null;
      };

      # pi's startup model, persisted in NIX instead of pi's own
      # `~/.pi/agent/settings.json` (`defaultProvider`/`defaultModel`): the
      # sandbox deployments mount `~/.pi/agent` config from the store and
      # pi's fallback chain (no `--model`, no settings entry) ends at its
      # built-in per-provider preference table, which pins
      # `anthropic/claude-opus-4-8` — an old Opus — as the very first match.
      # A generated extension (`defaultModelExtension` below) applies the
      # configured default on every session start, overriding that
      # fallback without writing `settings.json` (which would race the
      # in-memory SettingsManager and leak host state into sandboxes).
      defaultModel = mkOption {
        type = types.nullOr (
          types.submodule {
            options = {
              provider = mkOption { type = types.str; };
              model = mkOption { type = types.str; };
            };
          }
        );
        default = null;
        description = ''
          Startup model for pi sessions (`{ provider, model }` with pi's
          provider id and model id, e.g. `{ provider = "f13-litellm"; model = "GLM-5.3"; }`).
          Deployed as the generated `myconfig-default-model.ts` extension,
          which sets the model on session start without touching
          `~/.pi/agent/settings.json`.

          Null (default) keeps pi's own resolution: CLI `--model`, the
          `enabledModels` scope, then `settings.json`, then pi's built-in
          per-provider fallback table (which prefers `anthropic`'s current
          flagship Opus).
        '';
      };

      # pi-token-speed (https://github.com/gsanhueza/pi-token-speed): a
      # third-party extension that displays real-time tokens-per-second
      # (TPS) in the pi status bar while responses stream. Disabled by
      # default: it is upstream code this repo does not control, so hosts
      # opt in explicitly (e.g. hosts/host.f13/ai.f13.nix).
      tokenSpeed.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Deploy the pi-token-speed extension
          (https://github.com/gsanhueza/pi-token-speed, pinned via
          `_sources/generated.nix`) to `~/.pi/agent/extensions/pi-token-speed/`.
          It shows real-time tokens-per-second and time-to-first-token in
          the pi status bar and adds a `/tps` settings command. Configure
          it via the `tokenSpeed` section in `~/.pi/agent/settings.json`
          (see the upstream README).
        '';
      };
    };
  };
  config = lib.mkIf config.myconfig.ai.dev.pi-coding-agent.enable {
    # Register the worktree launcher as a workmux "named agent" so
    # `pi-worktree` (below) can `workmux add --agent pi`.
    myconfig.ai.dev.workmux.agents.pi = piWorktree.agent;

    # mysbx tier integration (see `mysbxPiMounts` above). Gated on mysbx
    # being enabled too: the two features are independent, and the mounts
    # would otherwise be generated for a host that has no mysbx config.
    # `config.env.PI_JAIL_MARKER` marks every mysbx sandbox as jailed for the
    # jail-marker extension, so the red `unjailed` border stays reserved for
    # genuinely un-sandboxed sessions (plain `pi`).
    myconfig.ai.dev.mysbx = lib.mkIf config.myconfig.ai.dev.mysbx.enable {
      extraTools = [ pi-coding-agent-pkg ];
      # C7b (bd myconfig-f23e): pi's own nodejs, by major — pi execs
      # `node` by store path (never on the sandbox PATH, so no probe
      # could find it), and the mysbx wrapper needs the declared
      # major to gate the NODE_USE_ENV_PROXY injection. mkDefault: a
      # host that pins `nodeMajor` itself wins over this derivation.
      nodeMajor = lib.mkDefault (lib.toInt (lib.versions.major piNodeJs.version));
      # The agent registry entry (bd myconfig-fv4h, ../../../../mysbx/docs/
      # design/config.md D32): `pi` is a SELECTABLE kind — the CLI
      # resolves `default-agent = "pi"` (seeded below) or `--pi`
      # against this command. The bare name resolves from the sandbox
      # PATH (`extraTools` above carries the package).
      agents.pi.command = lib.mkDefault "pi";
      defaultAgent = lib.mkDefault "pi";
      config.mounts = mysbxPiMounts;
      config.env.PI_JAIL_MARKER = "1";
      # The litellm endpoint of the podman-gvisor backend: pi's mounted
      # configuration names the host loopback, which inside a container
      # is the container's own. `podman.env` reaches that backend ALONE
      # (the bubblewrap backend shares the host network namespace, where
      # the baked URL is right), and both generated extensions read the
      # variable (`litellmBaseUrlEnvVar` above).
      podman.env = lib.mkIf osconfig.myconfig.ai.dev.litellm-forwarder.enable {
        ${litellmBaseUrlEnvVar} = osconfig.myconfig.ai.dev.litellm-forwarder.endpoint;
      };
      # The litellm endpoint of the microvm-qemu backend (bd
      # myconfig-mzys): the SLiRP guest's loopback is its own — the
      # same class of wrong URL as the container's — but the SLiRP
      # gateway `10.0.2.2` maps to the HOST loopback, where the
      # keyless proxy listens (no forwarder needed, unlike pasta).
      # Both generated extensions read the variable
      # (`litellmBaseUrlEnvVar` above).
      microvm-qemu.env = lib.mkIf osconfig.services.litellm.enable {
        ${litellmBaseUrlEnvVar} = "http://10.0.2.2:${toString osconfig.services.litellm.port}/v1";
      };
      # The microvm-ch backend's passt guest uses the same address plan.
      microvm-ch.env = lib.mkIf osconfig.services.litellm.enable {
        ${litellmBaseUrlEnvVar} = "http://10.0.2.2:${toString osconfig.services.litellm.port}/v1";
      };
      # pi's session store, backed by `<repo>.mysbx/state/.pi/agent/sessions`
      # (D15). No nesting conflict with the opencode/rtk entries
      # (`.local/...`) — `check_state_dirs` enforces that pairwise anyway.
      config.stateDirs = [ ".pi/agent/sessions" ];
    };

    # fd in the sandbox (shared hook, see
    # ../myconfig.ai.dev.sandboxTools.nix) for pi's `@`
    # file-path completion, which shells out to `fd`.
    myconfig.ai.dev.sandboxTools.extraPackages = [ pkgs.fd ];

    home-manager.sharedModules = [
      (
        { lib, ... }:
        {
          myconfig.persistence.directories = [ ".pi" ];
          home.file = lib.mkMerge [
            {
              ".pi/agent/extensions/myconfig-providers.ts".source = providersExtension;
              ".pi/agent/extensions/myconfig-jail-marker.ts".source = jailMarkerExtension;
              ".pi/agent/extensions/myconfig-mysbx-status.ts".source = mysbxStatusExtension;
              ".pi/agent/extensions/myconfig-mysbx-inform.ts".source = mysbxInformExtension;
              ".pi/agent/extensions/handoff.ts".source = handoffExtension;
              ".pi/agent/themes/unjailed.json".source = unjailedTheme;
              # Override the default `app.interrupt` (escape) with `ctrl+c`
              # for reliability across SSH and tmux; `app.clear` moves to
              # `ctrl+q` to avoid the conflict. `app.model.cycleForward`
              # (default `ctrl+p`) and `app.model.cycleBackward` (default
              # `shift+ctrl+p`) are disabled so those keys are never swallowed
              # by model cycling. See ./keybindings.json.
              ".pi/agent/keybindings.json".source = ./keybindings.json;
            }
            subagentExtensionFiles
            subagentAgentFiles
            subagentPromptFiles
            # Dynamic litellm model discovery (bd myconfig-99z): deployed
            # ONLY when `litellmUrl != ""`; it then solely owns the
            # `${hostname}-litellm` provider id (see `litellmProvider`).
            # `optionalAttrs` (not `mkIf` on `.source`): with `mkIf` the
            # `home.file.<path>` attribute would still be declared while its
            # `.source` is left undefined, which breaks home-manager
            # activation for hosts that disable the plugin.
            (lib.optionalAttrs (cfg.litellmUrl != "") {
              ".pi/agent/extensions/myconfig-litellm-models.ts".source = litellmModelsExtension;
            })
            # Nix-side startup model (see `defaultModel` option): session-local
            # `/model`-style override on every fresh session start. Deployed
            # ONLY when `defaultModel != null`.
            (lib.optionalAttrs (cfg.defaultModel != null) {
              ".pi/agent/extensions/myconfig-default-model.ts".source = defaultModelExtension;
            })
            # pi-token-speed: third-party TPS status-bar extension (see the
            # option above and the `tokenSpeedExtension` let binding).
            # `optionalAttrs` (not `mkIf`) so the `home.file.<path>` attribute
            # is absent entirely on hosts that disable it.
            (lib.optionalAttrs cfg.tokenSpeed.enable {
              ".pi/agent/extensions/pi-token-speed/index.ts".source = "${tokenSpeedExtension}/index.ts";
              ".pi/agent/extensions/pi-token-speed/src".source = "${tokenSpeedExtension}/src";
            })
          ];
          home.packages = [
            pkgs.nixos-unstable.pi-coding-agent
            pkgs.fd
            piWorktree.wrapper
          ];
        }
      )
    ];
  };
}
