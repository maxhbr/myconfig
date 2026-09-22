# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# pi coding agent, its wrappers (pi/piBwrap, agent-bubblewrap-pi,
# agent-qemu-pi, worktree variants), the generated pi extensions
# (providers, jail marker, subagent/handoff examples) and the mysbx
# tier integration: pi's config is mounted into mysbx sandboxes from a
# dereferenced store tree built by the shared
# ../../mysbx/nix/sandbox-config.nix helper (bd myconfig-ooh).
{
  config,
  lib,
  pkgs,
  jail,
  ...
}:
let
  osconfig = config;
  system = pkgs.stdenv.hostPlatform.system;
  cfg = config.myconfig.ai.dev.pi-coding-agent;
  callLib = file: import file { inherit lib pkgs; };
  callJailLib =
    file:
    import file {
      inherit
        lib
        pkgs
        jail
        osconfig
        ;
    };
  jail-app = callJailLib ../../fns/bubblewrap-app.nix;

  # The shared mysbx sandbox-config helper (the dereferenced-store-tree
  # machinery, bd myconfig-ooh); consumed like the entry scripts consume
  # ../../mysbx/nix/mux-entry-lib.nix.
  sandboxConfigLib = import ../../mysbx/nix/sandbox-config.nix {
    inherit lib;
    runCommand = pkgs.runCommand;
  };

  # Jail library handle + the jail-to-host channel combinator. The combinator
  # exposes a program *inside* the jail that forwards its single argument over
  # a FIFO to a handler running *outside* the jail. We use it so the jailed pi
  # can report its workmux status to the tmux server (which lives in the
  # worktree pane's un-jailed environment where $TMUX/$TMUX_PANE and the tmux
  # socket are valid) without exposing any tmux socket or $TMUX to the jail.
  jailLib = jail.init pkgs;
  inherit (jailLib.combinators) jail-to-host-channel add-runtime;

  # Runtime permission for plain `agent-bubblewrap-pi`: when the working directory is a
  # git repository that has a sibling `../<basename>__worktrees` directory
  # (the convention used by the worktree tooling), bind that directory
  # read-write into the jail so the agent can read/edit sibling worktrees.
  # The check is done at runtime (in the wrapper, before `bwrap` starts) so
  # the bind only happens when the `.git` marker exists on the host.
  # `mount-cwd` already binds `$PWD` itself; this adds the out-of-tree
  # worktrees directory next to it. Creation is ATTEMPTED when the directory
  # is missing (bubblewrap can only bind an existing path, so otherwise the
  # agent could not create the first worktree of a repository), but the mount
  # stays OPTIONAL: when the repository's parent is not writable - e.g. a
  # user-owned checkout below a root-owned directory - the jail must still
  # start, since most sessions never touch a worktree at all. The
  # herdr-specific launcher in ../programs.herdr.nix hard-fails instead,
  # because worktree creation is its whole point.
  worktreesSiblingPerm = add-runtime ''
    if [ -e "$PWD/.git" ]; then
      _jp_worktrees="$(dirname "$PWD")/$(basename "$PWD")__worktrees"
      mkdir -p "$_jp_worktrees" 2>/dev/null || true
      if [ -d "$_jp_worktrees" ]; then
        RUNTIME_ARGS+=(--bind "$_jp_worktrees" "$_jp_worktrees")
      fi
    fi
  '';

  # Make the `workmux` binary available *inside* the agent sandboxes (jail and
  # bubblewrap) whenever workmux is enabled. The status-tracking hooks/
  # extensions installed by `myconfig.ai.dev.workmux` all shell out to
  # `workmux set-window-status`, and un-jailed/plain worktree agents may run
  # `workmux merge` / `workmux remove --keep-branch` from their pane, so the
  # binary must resolve on PATH inside the sandbox (empty list on hosts without
  # workmux). NOTE: the *jailed* worktree variant
  # (`agent-bubblewrap-pi-worktree-inner`) does NOT carry the real binary; it installs
  # `workmuxStatusShim` instead, which can only route `set-window-status` (see
  # the shim below). `workmux merge`/`remove` from inside that jail cannot
  # reach the tmux socket and will fail.
  workmuxDevTools = lib.optional osconfig.myconfig.ai.dev.workmux.enable osconfig.myconfig.ai.dev.workmux.package;

  # A jail-to-host channel exposing `workmux_status_channel` *inside* the jail.
  # Calling `workmux_status_channel <status>` sends `<status>` over a FIFO to
  # the handler below, which runs *outside* the jail in the worktree tmux
  # pane's environment. There, $TMUX/$TMUX_PANE and the tmux socket are valid,
  # so `workmux set-window-status` can update the pane's tmux window name.
  #
  # LOAD-BEARING: the handler runs *outside* the jail as a background process
  # forked by the launcher pane, and relies on inheriting the launcher pane's
  # $TMUX / $TMUX_PANE (and thus the tmux socket). This inheritance is correct
  # because the launcher execs the inner jail wrapper in-pane and the jail's
  # non-empty `cleanup` means the wrapper does not `exec bwrap` (so the
  # background handler survives). $TMUX is never leaked *into* the jail
  # (bwrap --clearenv).
  #
  # The channel name must be a valid POSIX identifier (the combinator asserts
  # `isValidPosixName`), hence underscores rather than hyphens. The handler is
  # wrapped in a `writeShellApplication` with no runtimeInputs, so it sets its
  # own PATH to make `workmux` and `tmux` resolvable. Gated on workmux being
  # enabled (empty list otherwise).
  #
  # Errors are logged (rather than fully swallowed) to $TMPDIR/-/tmp so that a
  # misconfigured channel leaves a diagnosable trail instead of failing
  # silently (the pi extension also `.catch()`es the in-jail call).
  workmuxStatusChannelPerms = lib.optional osconfig.myconfig.ai.dev.workmux.enable (
    jail-to-host-channel "workmux_status_channel" ''
      export PATH=${
        lib.makeBinPath [
          osconfig.myconfig.ai.dev.workmux.package
          pkgs.tmux
        ]
      }:$PATH
      workmux set-window-status "$1" >>"''${TMPDIR:-/tmp}/workmux_status_channel.log" 2>&1 || true
    ''
  );

  # A `workmux` PATH-shim installed *inside* the worktree jail in place of the
  # real workmux binary. It intercepts `set-window-status` and routes it
  # through the `workmux_status_channel` program (exposed by the channel
  # above), so status updates reach the host tmux server without the jail
  # needing a tmux socket. Any other subcommand is forwarded to the real
  # workmux binary by absolute store path. Installing the shim instead of the
  # real `workmux` avoids a PATH name collision on `workmux`.
  #
  # NOTE: only `set-window-status` is rescued. Other subcommands (`workmux
  # merge`, `workmux remove`, ...) exec the real binary, which still needs the
  # tmux socket that is not present inside the jail, so they fail. This matches
  # the pre-change behaviour (the inner jail previously carried the real binary,
  # equally unable to reach tmux from inside the sandbox).
  #
  # The real-binary forward uses `lib.getExe'` (not `lib.getExe`) because the
  # upstream workmux flake package sets no `meta.mainProgram`; `getExe'` with
  # the explicit `"workmux"` binary name silences the getExe deprecation
  # warning while producing the identical `${pkg}/bin/workmux` path.
  workmuxStatusShim = pkgs.writeShellApplication {
    name = "workmux";
    text = ''
      if [ "''${1-}" = "set-window-status" ]; then
        shift
        exec workmux_status_channel "''${1-}"
      fi
      exec ${lib.getExe' osconfig.myconfig.ai.dev.workmux.package "workmux"} "$@"
    '';
  };

  # Build a lookup: model name (raw or provider-prefixed) -> contextWindow.
  # Covers both direct local-provider lookups (raw name) and
  # LiteLLM lookups (providerName:modelName).
  #
  # Two sources are merged (first occurrence wins, in source order):
  #
  #  1. `myconfig.ai.localModels` — the per-backend registry (llama-cpp /
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
      ) osconfig.myconfig.ai.localModels;

      # Extract the context window from a litellm model_list entry. The
      # value lives in `litellm_params.max_input_tokens` (the
      # auto-generated entries from modules/myconfig.ai/services.litellm.nix
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
  #     modules/myconfig.ai/services.litellm.nix for every auto-generated
  #     local-model entry (`min (contextWindow / 4) 65536`);
  #   - `model_info.max_output_tokens` — written by
  #     modules/myconfig.ai/litellm.proxy.nix when a forwarded model
  #     declares `maxOutputTokens`.
  # `myconfig.ai.localModels` has no equivalent field, so it contributes
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
  # modules/myconfig.ai/services.litellm.nix uses for its generated
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
  ) osconfig.myconfig.ai.localModels;

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
    # by modules/myconfig.ai/services.litellm.nix) repeats the same
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

  # Custom theme to flag *non-jailed* pi sessions. Both `pi`/`piBwrap` and
  # `agent-bubblewrap-pi` wrap the same `pi-coding-agent` binary and share the same
  # `~/.pi/agent/settings.json` (hence the same theme), so the two are
  # otherwise indistinguishable in the TUI. The jailed (sandboxed) session is the safe
  # default and keeps the user's normal theme untouched; only the *un-jailed*
  # session is recolored, giving its editor box border a red warning color.
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
  # (set to "1" inside the `agent-bubblewrap-pi` jail) and, only when it is *not* set,
  # switches to this theme on session start. Crucially it switches via the
  # Theme *object* overload of `ctx.ui.setTheme()`, which is in-memory only
  # and does NOT persist: the string overload writes the theme into the
  # shared `~/.pi/agent/settings.json`, which previously leaked the red marker
  # into the jailed session (both wrappers share `~/.pi`). The extension also
  # self-heals that legacy corruption once (see its header comment).
  pi-coding-agent-pkg = pkgs.nixos-unstable.pi-coding-agent;

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
    // Visually distinguishes the sandboxed sessions (`agent-bubblewrap-pi`
    // and the mysbx tier, which set PI_JAIL_MARKER=1) from the un-sandboxed
    // `pi`/`piBwrap` wrappers (which do not). The un-sandboxed session is the
    // dangerous one (full home access), so it gets a red editor border via
    // the `unjailed` theme; every sandboxed session — the agent-bubblewrap
    // jail (marker set in the wrapper's runtime env) and mysbx (marker set
    // via the generated mysbx user config layer `env`) — keeps the user's
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
  # `~/.pi/agent/extensions/pi-token-speed/` (pi loads `*/index.ts`; the
  # sibling `./src/...` imports resolve via relative specifiers). Vendor
  # files upstream does not need at runtime (tests, package.json,
  # tsconfig) are dropped for a clean read-only extension directory.
  # Upstream targets pi >= 0.85.1 (peerDependencies), matching the pinned
  # `pkgs.nixos-unstable.pi-coding-agent` here.
  tokenSpeedSrc = (pkgs.callPackage ../../../../_sources/generated.nix { }).pi-token-speed.src;

  tokenSpeedExtension = pkgs.runCommand "pi-token-speed-extension" { } ''
    mkdir -p $out
    cp -r ${tokenSpeedSrc}/index.ts ${tokenSpeedSrc}/src $out/
  '';

  piBwrap = callLib ../../fns/bubblewrap-simple-app.nix {
    name = "pi";
    pkg = pkgs.nixos-unstable.pi-coding-agent;
    extraRuntimeInputs = [
      pkgs.wget
      pkgs.curl
      pkgs.jq
    ]
    ++ workmuxDevTools;
    writableDirs = [
      ".pi"
    ];
    # Read-only bind of `~/.agents/skills/`, where `myconfig.ai.dev.skills`
    # deploys handcrafted skills for pi (which has no `programs.pi.skills`
    # option). See `modules/myconfig.ai.dev/skills/default.nix`.
    readOnlyConfigDirs = [ ".agents" ];
  };

  # `agent-bubblewrap-pi` is an alternative to `piBwrap` that uses the jail.nix library
  # (vendored at ./vendor/alexdavid-jail.nix) instead of a hand-rolled
  # bubblewrap wrapper. See `../../fns/bubblewrap-app.nix` for the shared defaults.
  #
  # `~/.pi` is rw-bound because the agent picks up the auto-generated
  # provider extension installed by home-manager
  # (`~/.pi/agent/extensions/myconfig-providers.ts`) and so session and
  # credential state persists across invocations.
  agent-bubblewrap-pi = jail-app {
    name = "agent-bubblewrap-pi";
    pkg = pkgs.nixos-unstable.pi-coding-agent;
    userDataDirs = [ ".pi" ];
    # Read-only bind of `~/.agents/skills/` so pi discovers the handcrafted
    # skills deployed there by `myconfig.ai.dev.skills`. Bound via
    # `extraConfigDirs` (try-ro-bind inside `bubblewrap-app.nix`), so the jail still
    # starts on a host where `.agents` has not been deployed.
    extraConfigDirs = [ ".agents" ];
    # Marker so `agent-bubblewrap-pi` sessions are visually distinguishable from the
    # plain `pi`/`piBwrap` wrappers (which share the same `~/.pi/agent/settings.json`,
    # hence the same theme). The `myconfig-jail-marker.ts` extension (deployed
    # below) reads this variable: when set (jailed) it leaves the user's theme
    # untouched (after self-healing any legacy marker corruption); when unset
    # (un-jailed) it applies the red-border `unjailed` theme in-memory only.
    extraRuntimeEnv.PI_JAIL_MARKER = "1";
    # `pi` itself on the jail `PATH`: the jailed process only gets the binary
    # exec'd by the jail wrapper, so anything that shells out to `pi` by name
    # (subagents, `/handoff`, scripts and skills in the workspace) fails with
    # "command not found" unless the package is also added to `PATH`.
    extraDevTools = [ pi-coding-agent-pkg ] ++ workmuxDevTools;
    # Bind the host path named by `PI_WORKTREE_MAIN_REPO` read-only into the
    # jail. The `*-worktree` wrapper scripts set this to the *original* git
    # repository (the worktree's linked main repo) before exec'ing `agent-bubblewrap-pi`,
    # so git operations against the worktree can resolve the shared `.git`
    # object store, refs and config that live in the main repo. Without this
    # bind, `mount-cwd` only exposes the worktree directory itself and git
    # fails with `fatal: not a git repository` because the worktree's `.git`
    # file points into the main repo's `.git/worktrees/<name>/`. Unset for
    # the plain (`agent-bubblewrap-pi`, `agent-bubblewrap-pi-tmp`) variants, where the
    # conditional `--ro-bind-try` skips the bind silently.
    extraReadOnlyEnvPaths = [ "PI_WORKTREE_MAIN_REPO" ];
    # Also expose a sibling `../<basename>__worktrees` directory read-write
    # when running from a git repo that has one (see `worktreesSiblingPerm`).
    extraPermissions = [ worktreesSiblingPerm ];
  };

  # Worktree-only variant: expose the linked main checkout read-only, then
  # remount its shared Git directory (`.git`) read-write. Git needs this for
  # objects, refs, index locks, and worktree administration when committing
  # from a linked worktree.
  # This is separate from `agent-bubblewrap-pi` so a normal invocation cannot request a
  # writable bind to another repository merely by setting an environment var.
  agent-bubblewrap-pi-worktree-inner = jail-app {
    name = "agent-bubblewrap-pi-worktree-inner";
    pkg = pkgs.nixos-unstable.pi-coding-agent;
    userDataDirs = [ ".pi" ];
    extraConfigDirs = [ ".agents" ];
    # Install the `workmux` PATH-shim (not the real binary) so
    # `workmux set-window-status` — invoked by the jailed pi's
    # `workmux-status.ts` extension — is routed through the jail-to-host
    # channel below instead of shelling out to tmux (which is unreachable
    # inside the jail). Gated on workmux being enabled.
    # Same rationale as `agent-bubblewrap-pi` above: keep `pi` reachable by
    # name inside the jail.
    extraDevTools = [
      pi-coding-agent-pkg
    ]
    ++ lib.optional osconfig.myconfig.ai.dev.workmux.enable workmuxStatusShim;
    extraRuntimeEnv.PI_JAIL_MARKER = "1";
    extraReadOnlyEnvPaths = [ "PI_WORKTREE_MAIN_REPO" ];
    # This bind is emitted after the read-only main-repo bind by jail-app.
    extraReadWriteEnvPaths = [ "PI_WORKTREE_GIT_DIR" ];
    # Expose the `workmux_status_channel` program inside the jail and run its
    # host-side handler in the worktree tmux pane's environment.
    extraPermissions = workmuxStatusChannelPerms;
  };

  # Thin workmux-driven replacement for the previous bespoke worktree script.
  # `mkWorkmuxWorktree` builds the launcher run inside the worktree pane (which
  # resolves the shared git dir, exports PI_WORKTREE_* and execs
  # `agent-bubblewrap-pi-worktree-inner`) and the user-facing `agent-bubblewrap-pi-worktree`
  # wrapper (which requires tmux and calls `workmux add --agent agent-bubblewrap-pi`).
  mkWorkmuxWorktree = callLib ../../fns/workmux-worktree.nix;
  agentBubblewrapPiWorktree = mkWorkmuxWorktree {
    name = "agent-bubblewrap-pi-worktree";
    agentName = "agent-bubblewrap-pi";
    agentType = "pi";
    innerPkg = agent-bubblewrap-pi-worktree-inner;
    workmuxPkg = osconfig.myconfig.ai.dev.workmux.package;
  };
  # Non-jailed variant driving the bubblewrap `piBwrap` wrapper. It exposes
  # the worktree's shared git dir via the sandboxed-app `WORKTREE_*` env vars.
  piWorktree = mkWorkmuxWorktree {
    name = "pi-worktree";
    agentName = "pi";
    agentType = "pi";
    innerPkg = piBwrap;
    workmuxPkg = osconfig.myconfig.ai.dev.workmux.package;
    mainRepoEnv = "WORKTREE_MAIN_REPO";
    gitDirEnv = "WORKTREE_GIT_DIR";
  };

  # `agent-qemu-pi` — the microVM analogue of `agent-bubblewrap-pi`. Same ergonomics
  # (run it from a project subdirectory; the working directory is the only
  # writable thing the agent sees), but instead of a bubblewrap jail the
  # agent runs inside a real microvm.nix VM with its own kernel, an ephemeral
  # root filesystem and an unprivileged `agent` user. See
  # ../../myconfig.ai.dev.qemu-agent-sandbox for the guest/runner and the rationale for
  # qemu + user-mode networking over cloud-hypervisor.
  #
  # The current working directory is shared read-write at /workspace via
  # virtiofs; the agent is reached over SSH on a host-localhost forwarded port
  # using a throwaway keypair generated per invocation. LLM credentials
  # (OPENAI_API_KEY etc.) are forwarded over the SSH environment at launch —
  # never baked into the Nix store, never in process argv. The VM is torn down
  # and all guest state discarded on exit; only the workspace persists.
  agent-qemu-pi = pkgs.writeShellApplication {
    name = "agent-qemu-pi";
    runtimeInputs = with pkgs; [
      nix
      openssh
      coreutils
      gnugrep
    ];
    text = ''
      # Refuse to run in $HOME: like agent-bubblewrap-pi, the working directory is
      # shared writable into the sandbox, and sharing the whole home
      # directory would defeat the isolation. Run from a project subdirectory.
      if [ "$PWD" = "$HOME" ]; then
        echo "agent-qemu-pi: refusing to run in home directory ($HOME):" >&2
        echo "agent-qemu-pi: the working directory is shared writable into the VM." >&2
        echo "agent-qemu-pi: run from a project subdirectory instead." >&2
        exit 1
      fi

      workspace="$(realpath "$PWD")"
      if [ ! -d "$workspace" ]; then
        echo "agent-qemu-pi: workspace is not a directory: $workspace" >&2
        exit 1
      fi

      # Per-invocation runtime state (throwaway SSH key, VM control socket).
      runtime_dir="$(mktemp -d "''${XDG_RUNTIME_DIR:-/tmp}/agent-qemu-pi.XXXXXX")"
      # Pick a pseudo-random host-localhost port for the forwarded guest SSH.
      ssh_port=$(( (RANDOM % 20000) + 20000 ))

      vm_pid=""
      cleanup() {
        # Killing the launcher triggers its own trap, which tears down the
        # qemu VM and the virtiofsd daemon(s) it started.
        if [ -n "$vm_pid" ] && kill -0 "$vm_pid" 2>/dev/null; then
          kill "$vm_pid" 2>/dev/null || true
          wait "$vm_pid" 2>/dev/null || true
        fi
        rm -rf "$runtime_dir"
      }
      trap cleanup EXIT INT TERM

      # Throwaway SSH keypair authorizing the launcher into the guest.
      ssh-keygen -q -t ed25519 -N "" -f "$runtime_dir/id" -C agent-qemu-pi

      export QEMU_AGENT_SANDBOX_KIND=pi
      export AGENT_QEMU_PI_WORKSPACE="$workspace"
      export AGENT_QEMU_PI_SSH_PORT="$ssh_port"
      export AGENT_QEMU_PI_AUTHORIZED_KEYS="$runtime_dir/id.pub"
      export AGENT_QEMU_PI_NETWORK=1
      # Shared sandbox tools (myconfig.ai.sandboxTools), baked in at build
      # time as a JSON array of store paths; read by the impure
      # standalone qemu-agent-sandbox runner expression.
      export AGENT_QEMU_PI_EXTRA_PACKAGES='${sandboxToolsJson}'

      echo "agent-qemu-pi: building microvm runner for workspace: $workspace" >&2
      # Evaluate the module-owned expression directly. The runner must be
      # built impurely because it embeds the per-launch workspace path,
      # forwarded SSH port, and throwaway authorized-keys file.
      runner=$(nix build --impure --no-link --print-out-paths \
        --file ${config.myconfig.ai.dev.qemu-agent-sandbox.runnerExpression})

      # microvm.nix's qemu runner connects to the virtiofs daemons over
      # RELATIVE unix socket paths (e.g. `agent-qemu-pi-virtiofs-nix-store.sock`),
      # and virtiofsd creates those sockets in its own working directory.
      # The runner's `bin/sandboxed-launch` starts the (rootless) virtiofsd
      # daemon(s), waits for their sockets, then runs qemu — all from the
      # current directory. Run it from $runtime_dir so the relative socket
      # paths resolve to a stable, per-invocation location and are cleaned up
      # on exit.
      cd "$runtime_dir"

      echo "agent-qemu-pi: starting microvm (guest SSH forwarded to 127.0.0.1:$ssh_port)" >&2
      "$runner/bin/sandboxed-launch" >"$runtime_dir/console.log" 2>&1 &
      vm_pid=$!

      # Wait for the guest SSH server to accept our key (or the VM to die).
      ssh_opts=(
        -p "$ssh_port"
        -i "$runtime_dir/id"
        -o StrictHostKeyChecking=no
        -o UserKnownHostsFile=/dev/null
        -o ConnectTimeout=3
        -o LogLevel=ERROR
      )
      ready=0
      for _ in $(seq 1 120); do
        if ! kill -0 "$vm_pid" 2>/dev/null; then
          echo "agent-qemu-pi: microvm exited before SSH became ready; console log:" >&2
          tail -n 40 "$runtime_dir/console.log" >&2 || true
          exit 1
        fi
        if ssh "''${ssh_opts[@]}" agent@127.0.0.1 true 2>/dev/null; then
          ready=1
          break
        fi
        sleep 1
      done
      if [ "$ready" -ne 1 ]; then
        echo "agent-qemu-pi: timed out waiting for guest SSH; console log:" >&2
        tail -n 40 "$runtime_dir/console.log" >&2 || true
        exit 1
      fi

      # Forward LLM credentials over the SSH environment (never in argv or the
      # store). Only forward variables that are actually set on the host.
      for var in OPENAI_API_KEY OPENAI_BASE_URL OPENROUTER_BASE_URL ANTHROPIC_API_KEY; do
        if [ -n "''${!var:-}" ]; then
          ssh_opts+=(-o "SetEnv=$var=''${!var}")
        fi
      done

      # Seed the in-guest `pi` configuration from the host `~/.pi` (and the
      # shared skills/git config). The runner carries a `seed-agent-config`
      # script that copies the ALLOWLISTED, denylist-filtered host
      # configuration into the guest `/home/agent` over SSH — never baking
      # anything into the store and never copying credential files (keys keep
      # flowing over the SSH environment above). See
      # ../../myconfig.ai.qemu-agent-sandbox/builders.nix (`mkSeedScript`) and
      # ../../fns/seed-agent-config.nix. Run it BEFORE the interactive session
      # so the agent starts already configured.
      if [ -x "$runner/bin/seed-agent-config" ]; then
        echo "agent-qemu-pi: seeding guest agent config from host" >&2
        "$runner/bin/seed-agent-config" "$ssh_port" "$runtime_dir/id" 127.0.0.1 agent \
          || echo "agent-qemu-pi: warning: config seeding reported errors (continuing)" >&2
      fi

      # Build a safely-quoted remote command: cd into the workspace and exec
      # pi with the caller's argument vector preserved via printf %q.
      remote_cmd='cd /workspace && exec pi'
      for a in "$@"; do
        remote_cmd+=" $(printf '%q' "$a")"
      done

      # Interactive session (-t for a TTY so the pi TUI works).
      ssh -tt "''${ssh_opts[@]}" agent@127.0.0.1 "$remote_cmd"
    '';
  };

  # Shared sandbox tools (myconfig.ai.dev.sandboxTools) as a JSON array of store
  # paths, baked into the `agent-qemu-pi` wrapper and read (via the
  # AGENT_QEMU_PI_EXTRA_PACKAGES env var) by the impure runner expression that
  # builds the per-invocation VM runner. Same pattern as
  # AGENT_QEMU_HERDR_AGENT_PACKAGES in ../programs.herdr.nix.
  sandboxToolsJson = builtins.toJSON (
    map (p: p.outPath) config.myconfig.ai.dev.sandboxTools.extraPackages
  );

  # --- mysbx integration (../mysbx) -------------------------------------
  #
  # pi is the first coding agent wired into the `mysbx` sandbox tier. Three
  # things are needed for a usable session inside a mysbx sandbox:
  #
  #   1. the `pi` binary on the sandbox PATH -> `myconfig.ai.dev.mysbx.extraTools`
  #   2. pi's *configuration* visible inside the sandbox -> read-only mounts
  #      in the generated user config layer (`…mysbx.config.mounts`), each
  #      bound from a self-contained store tree of dereferenced copies built
  #      by the shared `mysbx/nix/sandbox-config.nix` helper (the sandbox
  #      binds real files because the podman-gvisor container mounts nothing
  #      from the host /nix/store — see the helper for the full rationale)
  #   3. the jail-marker extension must NOT flag the session as un-jailed:
  #      a mysbx sandbox is a jail just like `agent-bubblewrap-pi`, so the
  #      generated user layer also sets `PI_JAIL_MARKER=1` for every mysbx
  #      run (`…mysbx.config.env`). Without it the red `unjailed` border
  #      would apply to mysbx sessions: mysbx forwards only
  #      TERM/COLORTERM/LANG/LC_ALL/EDITOR/VISUAL
  #      (../../mysbx/mysbx-rs/src/lib.rs `FORWARDED_ENV_VARS`), so the
  #      extension would see no marker and misclassify the session.
  #
  #   4. pi's *sessions* survive the sandbox -> a `state-dirs` entry
  #      (`…mysbx.config.stateDirs`, ../../mysbx/docs/design/config.md D15).
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
  # (../../mysbx/mysbx-rs/src/bwrap.rs) — payload-writable content must
  # never sit above a mount point.

  # `~/.agents/skills/` only exists when a handcrafted skill is registered
  # (../skills/default.nix deploys the registry there for pi, which has no
  # `programs.pi.skills` option). Gate the mount on the same condition.
  piHasHandcraftedSkills = (config.myconfig.ai.dev.skills.handcrafted or { }) != { };

  mysbxPiSubtrees = [
    # Generated + example extensions (myconfig-providers.ts,
    # myconfig-jail-marker.ts, handoff.ts, subagent/) — always deployed
    # by the `home.file` block below.
    ".pi/agent/extensions"
    # Sub-agent definitions (upstream samples + handcrafted ones).
    ".pi/agent/agents"
    # Workflow prompt templates (`/implement`, `/commit`, …).
    ".pi/agent/prompts"
    # Themes (`unjailed.json`).
    ".pi/agent/themes"
    # Keybinding overrides (./keybindings.json).
    ".pi/agent/keybindings.json"
  ]
  # Handcrafted skills, discovered by pi from `~/.agents/skills/`.
  ++ lib.optional piHasHandcraftedSkills ".agents/skills";

  # The pi-config mounts for the mysbx sandbox, built by the shared
  # helper (../../mysbx/nix/sandbox-config.nix — the dereferenced-store-
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
  mysbxSandboxConfig = sandboxConfigLib.mkSandboxConfig {
    homeFile = config.home-manager.users.mhuber.home.file;
    homeDirectory = config.home-manager.users.mhuber.home.homeDirectory;
    subtrees = mysbxPiSubtrees;
    name = "pi-sandbox-config";
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
          pi coding agent, its wrappers (pi/piBwrap, agent-bubblewrap-pi,
          agent-qemu-pi, worktree variants) and the generated pi
          extensions (providers, jail marker, subagent/handoff examples).

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
    # Register the jailed worktree launcher as a workmux "named agent" so
    # `agent-bubblewrap-pi-worktree` (below) can `workmux add --agent agent-bubblewrap-pi`.
    myconfig.ai.dev.workmux.agents.agent-bubblewrap-pi = agentBubblewrapPiWorktree.agent;
    myconfig.ai.dev.workmux.agents.pi = piWorktree.agent;

    # mysbx tier integration (see `mysbxPiMounts` above). Gated on mysbx
    # being enabled too: the two features are independent, and the mounts
    # would otherwise be generated for a host that has no mysbx config.
    # `config.env.PI_JAIL_MARKER` marks every mysbx sandbox as jailed for the
    # jail-marker extension, so the red `unjailed` border stays reserved for
    # genuinely un-sandboxed sessions (plain `pi`/`piBwrap`).
    myconfig.ai.dev.mysbx = lib.mkIf config.myconfig.ai.dev.mysbx.enable {
      extraTools = [ pi-coding-agent-pkg ];
      config.mounts = mysbxPiMounts;
      config.env.PI_JAIL_MARKER = "1";
      # The litellm endpoint of the podman-gvisor backend: pi's mounted
      # configuration names the host loopback, which inside a container
      # is the container's own. `gvisor.env` reaches that backend ALONE
      # (the bubblewrap backend shares the host network namespace, where
      # the baked URL is right), and both generated extensions read the
      # variable (`litellmBaseUrlEnvVar` above).
      gvisor.env = lib.mkIf osconfig.myconfig.ai.dev.litellm-forwarder.enable {
        ${litellmBaseUrlEnvVar} = osconfig.myconfig.ai.dev.litellm-forwarder.endpoint;
      };
      # pi's session store, backed by `<repo>.mysbx/state/.pi/agent/sessions`
      # (D15). No nesting conflict with the opencode/rtk entries
      # (`.local/...`) — `check_state_dirs` enforces that pairwise anyway.
      config.stateDirs = [ ".pi/agent/sessions" ];
    };

    home-manager.sharedModules = [
      {
        myconfig.persistence.directories = [ ".pi" ];
        # Deploy the generated extensions/theme plus the subagent example
        # extension (multi-file, plus sample agents and workflow prompts) and
        # the handoff single-file extension. `mkMerge` is required because the
        # subagent files are produced as attrsets (enumerated with
        # `builtins.readDir` in the `let` bindings above) and must be combined
        # with the dotted `home.file.*` entries within this same module.
        home.file = lib.mkMerge [
          {
            ".pi/agent/extensions/myconfig-providers.ts".source = providersExtension;
            ".pi/agent/extensions/myconfig-jail-marker.ts".source = jailMarkerExtension;
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
          piBwrap
          agent-bubblewrap-pi
          agent-qemu-pi
          (pkgs.writeShellApplication {
            name = "pi-tmp";
            runtimeInputs = with pkgs; [ coreutils ];
            text = ''
              cd "$(mktemp -d)" && exec ${lib.getExe piBwrap} "$@"
            '';
          })
          (pkgs.writeShellApplication {
            name = "agent-bubblewrap-pi-tmp";
            runtimeInputs = with pkgs; [ coreutils ];
            text = ''
              cd "$(mktemp -d)" && exec ${lib.getExe agent-bubblewrap-pi} "$@"
            '';
          })
          piWorktree.wrapper
          agentBubblewrapPiWorktree.wrapper
        ];
      }
    ];
  };
}
