# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# "Remote LiteLLM proxy" pattern: forward all model requests to an
# upstream LiteLLM instance (e.g. thing's LiteLLM via vserver's Caddy
# reverse proxy).
#
# The proxy requires no authentication (no master_key is configured, so
# LiteLLM does not enforce API keys). This allows agent users —
# including the network-isolated "offline" agent, which can only reach
# loopback — to use AI models without credentials. The LiteLLM service
# runs as a dynamic system user, so its outbound requests to the
# upstream are not subject to the offline agent's iptables egress block.
#
# Because a host using this pattern does not register any localModels
# providers, the auto-generated model_list in
# modules/myconfig.ai/services.litellm.nix (a mkDefault) is empty.
# This module supplies the model_list directly with raw-name
# pass-through forward entries. The plain `=` (priority 100) still
# *merges* (concatenates) with other priority-100 contributors — most
# importantly the skainet/ and trustedtokens/ entries registered by
# the tng.nix flake modules (which also use a plain `=`). Using
# `mkForce` (priority 50) would silently discard those contributors.
#
# ON TOP of the declared `models`, this module can additionally forward
# Anthropic models: when the host provides Anthropic credentials in
# EITHER of two ways —
#   * an explicit `anthropicAuthEnvironmentFile` (e.g. a priv-repo
#     `writeText` env file), or
#   * a provisioned `myconfig.secrets` entry named by
#     `anthropicAuthSecretName` (default `litellm-anthropic-env`) whose
#     env-format source carries `ANTHROPIC_AUTH_TOKEN=` (and optionally
#     `ANTHROPIC_BASE_URL=`) —
# the module appends two wildcard routes (`claude-*` for Claude Code's
# bare slugs on /v1/messages, `anthropic/*` for litellm-convention
# prefixed names) and wires the decrypted secret file into the litellm
# unit as `EnvironmentFile=`. Providing credentials is the entire
# per-host opt-in: hosts without them evaluate exactly as before.
{
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.ai.litellm.proxy;

  # Whether the host has provisioned Anthropic credentials — and thus
  # gets the wildcard forwarding entries — in EITHER of two ways:
  #   * an explicitly provided `anthropicAuthEnvironmentFile` (e.g. a
  #     priv-repo `writeText` env file), or
  #   * the priv repo provisioning the `myconfig.secrets` entry named by
  #     `anthropicAuthSecretName` (a `myconfig.secrets` entry with
  #     `source = ...`). Entries without `source` are filtered out
  #     before they reach `age.secrets` (see modules/myconfig.secrets.nix),
  #     so the probe is only true on a host whose private repo actually
  #     provides the env-format file.
  # Public/CI builds and hosts that provide neither keep the exact
  # previous behavior.
  haveAnthropicCredentials =
    config.myconfig.ai.litellm.proxy.enable
    && (
      cfg.anthropicAuthEnvironmentFile != null || (config.age.secrets ? "${cfg.anthropicAuthSecretName}")
    );

  # A model spec is either a bare string (the model name, no metadata) or
  # an attrset { name; contextWindow?; maxOutputTokens?; }. The attrset
  # form lets a caller advertise the model's context window so LiteLLM can
  # do context_window fallback routing and clients can size prompts. These
  # numbers are *not* derivable by LiteLLM from the upstream (its
  # /v1/models is a fixed OpenAI-schema list and the upstream /model/info
  # reports null for these local models); they are scraped from the
  # backends' llama-server `--ctx-size` by
  # hosts/shared.localModels.update.sh. They surface on the proxy's
  # /model/info endpoint (model_info.max_input_tokens /
  # max_output_tokens), never on /v1/models.
  #
  # `contextWindow` deliberately mirrors the field of the same name on
  # myconfig.ai.localModels[].models, so the shared model list
  # (hosts/shared.localModels.litellm.models.nix) can be consumed by both
  # this module and hosts/shared.localModels.litellm.nix unchanged.
  #
  # Provider handling: `openai` (the default) forwards to
  # `upstreamApiBase` with `apiKey` as the OpenAI-style bearer. Any other
  # provider (e.g. `anthropic`) is *env-var driven*: no `api_base`/`api_key`
  # is emitted, so litellm resolves credentials from the unit environment
  # (see `anthropicBaseUrl` / `anthropicAuthEnvironmentFile` below).
  mkForwardEntry =
    m:
    let
      spec = if lib.isString m then { name = m; } else m;
      isOpenai = !(spec ? provider) || spec.provider == "openai";
      # Per-entry override of the API base; falls back to the upstream
      # LiteLLM base for the default openai provider. Null-aware because
      # the submodule fills `apiBase = null` into entries that did not
      # set it (and string specs never set it).
      apiBase =
        if (spec.apiBase or null) != null then
          spec.apiBase
        else
          lib.optionalString isOpenai cfg.upstreamApiBase;
      # Drop unset (null) fields so the string form and metadata-less
      # attrsets produce byte-identical output to the previous behavior.
      modelInfo = lib.filterAttrs (_: v: v != null) {
        max_input_tokens = spec.contextWindow or null;
        max_output_tokens = spec.maxOutputTokens or null;
      };
    in
    {
      model_name = if (spec.modelName or null) != null then spec.modelName else spec.name;
      litellm_params = {
        model = "${spec.provider or "openai"}/${spec.name}";
        request.allowPrivateNetwork = true;
      }
      // lib.optionalAttrs (apiBase != "") { api_base = apiBase; }
      // lib.optionalAttrs isOpenai { api_key = cfg.apiKey; };
    }
    // lib.optionalAttrs (modelInfo != { }) { model_info = modelInfo; };
in
{
  options.myconfig.ai.litellm.proxy = {
    enable = lib.mkEnableOption "a remote LiteLLM proxy that forwards all models to an upstream LiteLLM instance";

    upstreamApiBase = lib.mkOption {
      type = lib.types.str;
      description = ''
        Upstream LiteLLM API base URL (e.g.
        `http://litellm.thing.wg0.maxhbr.local:80/v1`).
        Each model is forwarded as `openai/<model>` pointing at this
        base.
      '';
    };

    models = lib.mkOption {
      type =
        with lib.types;
        listOf (
          either str (submodule {
            options = {
              name = lib.mkOption {
                type = str;
                description = "Model name to forward (becomes `<provider>/<name>`).";
              };
              provider = lib.mkOption {
                type = str;
                default = "openai";
                description = ''
                  LiteLLM provider prefix for the upstream call. The
                  default `"openai"` forwards to `upstreamApiBase`
                  with `apiKey`. Set `"anthropic"` to forward via
                  an Anthropic-compatible API resolved from the
                  service environment (`anthropicBaseUrl` /
                  `anthropicAuthEnvironmentFile`): the entry becomes
                  `anthropic/<name>` with no `api_base`/`api_key`,
                  so litellm reads `ANTHROPIC_BASE_URL` and
                  `ANTHROPIC_AUTH_TOKEN`/`ANTHROPIC_API_KEY` from
                  the unit environment. Other providers are passed
                  through as `<provider>/<name>` env-var driven
                  likewise.

                  A `*` in `name` makes the entry a LiteLLM wildcard
                  route (PatternMatchRouter): e.g.
                  `{ name = "claude-*"; provider = "anthropic"; }`
                  serves every `claude-<slug>` request as an
                  `anthropic/<slug>` upstream call, without enumerating
                  model slugs. Exact `model_name` matches always win
                  over wildcards.
                '';
              };
              modelName = lib.mkOption {
                type = nullOr str;
                default = null;
                description = ''
                  Client-facing name on the proxy (`model_name`,
                  what clients request). Defaults to `name`. Only
                  needed for wildcard entries whose request pattern
                  differs from the upstream model pattern, e.g.
                  `{ name = "*"; modelName = "anthropic/*"; provider = "anthropic"; }`
                  serves litellm-convention `anthropic/<model>`
                  requests as `anthropic/<model>` upstream calls.
                '';
              };
              apiBase = lib.mkOption {
                type = nullOr str;
                default = null;
                description = ''
                  Per-entry override of the upstream API base. When
                  null (the default), openai entries use
                  `upstreamApiBase` and other providers use no
                  `api_base` at all (env-var driven).
                '';
              };
              contextWindow = lib.mkOption {
                type = nullOr int;
                default = null;
                description = ''
                  Context window in tokens. Emitted as
                  `model_info.max_input_tokens`. Scraped from the
                  backend's llama-server `--ctx-size` by
                  hosts/shared.localModels.update.sh; LiteLLM cannot
                  derive it from the upstream. Mirrors the field of the
                  same name on `myconfig.ai.localModels[].models`.
                '';
              };
              maxOutputTokens = lib.mkOption {
                type = nullOr int;
                default = null;
                description = "Max output tokens. Emitted as `model_info.max_output_tokens`.";
              };
            };
          })
        );
      default = [ ];
      description = ''
        Models to forward. Each entry is either a bare model-name string
        or an attrset
        `{ name; provider?; modelName?; apiBase?; contextWindow?; maxOutputTokens?; }`.
        Each becomes a `<provider>/<name>` entry (default provider
        `openai`) pointing at the upstream API base; the optional fields
        add a `model_info` block. A `*` in `name` creates a LiteLLM
        wildcard route (see the `provider` option description).
      '';
    };

    anthropicBaseUrl = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Base URL of an Anthropic-compatible upstream API (e.g. another
        LiteLLM proxy). Wired into the litellm unit environment as
        `ANTHROPIC_BASE_URL` so `anthropic/*` model entries (see
        `models`) resolve their upstream from it. Not a secret; leave
        null to not set the variable (litellm then falls back to
        `https://api.anthropic.com`, or the same variable from
        `anthropicAuthEnvironmentFile` when provided).
      '';
    };

    anthropicAuthEnvironmentFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = ''
        Environment file (systemd `EnvironmentFile=`) carrying the
        Anthropic credentials for `anthropic/*` model entries, e.g. a
        line `ANTHROPIC_AUTH_TOKEN=<bearer>` (litellm sends it as
        `Authorization: Bearer`; `ANTHROPIC_API_KEY=<key>` would use
        the `x-api-key` header instead). The file must not live in the
        Nix store: secrets are provisioned via the separate `priv/`
        repository. The intended shape is a `myconfig.secrets` (agenix)
        entry whose decrypted env-format file is referenced here —
        declare the stub in the public repo
        (`myconfig.secrets."litellm-anthropic-env".dest =
        "/run/agenix/litellm-anthropic-env";`) with `source = ...` set
        in priv, and reference
        `config.myconfig.secrets."litellm-anthropic-env".dest` here.
        Leave null to not load any file.

        When this option is set, the module AUTOMATICALLY adds the
        anthropic wildcard model entries (see `anthropicAuthSecretName`)
        on top of `models` — no separate `models` entries are needed.
      '';
    };

    anthropicAuthSecretName = lib.mkOption {
      type = lib.types.str;
      default = "litellm-anthropic-env";
      description = ''
        Name of the `myconfig.secrets` (agenix) entry whose decrypted
        env-format file carries the Anthropic credentials
        (`ANTHROPIC_AUTH_TOKEN=<bearer>`, optionally
        `ANTHROPIC_BASE_URL=<upstream>`; litellm sends the token as
        `Authorization: Bearer`).

        When a secret with this name is provisioned — i.e. the host's
        private repo sets `myconfig.secrets."<name>".source = ...` —
        this module AUTOMATICALLY, additively on top of `models`:

          * appends the wildcard model entries `claude-*` (what Claude
            Code sends in the `model` field of `/v1/messages`) and
            `anthropic/*` (litellm-convention prefixed names), both
            routed upstream as env-var-driven `anthropic/<slug>` calls
            (no `api_base`/`api_key`, so litellm resolves
            `ANTHROPIC_BASE_URL` and `ANTHROPIC_AUTH_TOKEN` from the
            unit environment);
          * wires `services.litellm.environmentFile` to the secret's
            decrypted path.

        A host that provides neither this secret nor an explicit
        `anthropicAuthEnvironmentFile` gets no anthropic forwarding at
        all: no entries, no environment file, and the evaluation is
        identical to before. Provisioning credentials is therefore the
        entire per-host opt-in.
      '';
    };

    apiKey = lib.mkOption {
      type = lib.types.str;
      default = "not-needed";
      description = ''
        API key for the upstream LiteLLM instance. Defaults to
        `"not-needed"` since the upstream typically does not enforce
        authentication.
      '';
    };
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      services.litellm = {
        enable = true;
        # The declared models, PLUS the anthropic wildcard entries once
        # credentials are provisioned (priv repo secret or explicit
        # `anthropicAuthEnvironmentFile` — see
        # `haveAnthropicCredentials`).
        # `lib.optionals` keeps the list byte-identical for hosts
        # without the secret. Exact `model_name` matches always win over
        # the wildcards, so existing entries are unaffected.
        settings.model_list = map mkForwardEntry (
          cfg.models
          ++ lib.optionals haveAnthropicCredentials [
            {
              # Claude Code sends bare slugs (`claude-sonnet-4-5`, …)
              # in the `model` field of `/v1/messages`; route them
              # upstream as `anthropic/<slug>`.
              name = "claude-*";
              provider = "anthropic";
            }
            {
              # litellm-convention prefixed names for OpenAI-spec
              # clients: request `anthropic/<model>` -> upstream
              # `anthropic/<model>`.
              name = "*";
              modelName = "anthropic/*";
              provider = "anthropic";
            }
          ]
        );
      };
    })

    # Plain (non-secret) environment. Each block is a *top-level* mkIf so
    # that nothing is defined when the option is null: a definition that
    # exists but is discharged by a nested `mkIf false` would still count
    # as a definition of `services.litellm.environment` and silently
    # discard the nixpkgs module's option defaults.
    #
    # Once ANY definition of `services.litellm.environment` exists, the
    # nixpkgs module's option default (the telemetry-disabling vars
    # SCARF_NO_ANALYTICS / DO_NOT_TRACK / ANONYMIZED_TELEMETRY) is
    # discarded entirely, so this block re-asserts them with `mkDefault`
    # (a host can still override them with a plain `=`).
    (lib.mkIf (cfg.enable && cfg.anthropicBaseUrl != null) {
      services.litellm.environment = {
        SCARF_NO_ANALYTICS = lib.mkDefault "True";
        DO_NOT_TRACK = lib.mkDefault "True";
        ANONYMIZED_TELEMETRY = lib.mkDefault "False";
        ANTHROPIC_BASE_URL = cfg.anthropicBaseUrl;
      };
    })

    # Secret-safe credential injection via systemd `EnvironmentFile=`.
    # Explicit option first (plain priority).
    (lib.mkIf (cfg.enable && cfg.anthropicAuthEnvironmentFile != null) {
      services.litellm.environmentFile = cfg.anthropicAuthEnvironmentFile;
    })

    # Automatic credential injection when the priv-provisioned secret
    # exists (see `anthropicAuthSecretName`). Mutually exclusive with
    # the block above by construction. `age.secrets.<name>.path` is the
    # decrypted destination (`myconfig.secrets` default:
    # `/run/agenix/<name>`); systemd reads the file as root when the
    # service starts, before dropping to the dynamic user.
    (lib.mkIf
      (
        cfg.enable
        && cfg.anthropicAuthEnvironmentFile == null
        && (config.age.secrets ? "${cfg.anthropicAuthSecretName}")
      )
      {
        services.litellm.environmentFile = config.age.secrets."${cfg.anthropicAuthSecretName}".path;
      }
    )
  ];
}
