# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.microvm — guest-side, BOOT-TIME model discovery.
#
# Problem
# -------
# The coding-agent configs a sandbox gets are COPIES of the host primary
# user's rendered dotfiles, staged at launch time (see config-seed.nix):
#
#   * pi   → ~/.pi/agent/extensions/myconfig-providers.ts (generated from
#            `services.litellm.settings.model_list` at BUILD time)
#   * opencode → ~/.config/opencode/opencode.json (same source, same time)
#
# Those model lists are frozen when the guest image is built. The host LiteLLM
# proxy, by contrast, decides its ACTUAL model list at RUNTIME (models pulled /
# unpulled, llama-swap entries, per-host proxy config, a rebuilt proxy the guest
# image predates). A sandbox therefore routinely offers models the proxy no
# longer serves — and hides models it does serve.
#
# Solution
# --------
# A guest boot-time oneshot (`agent-model-config.service`, running as the
# unprivileged `agent` user) queries the SAME endpoint every guest agent talks
# to — the loopback LiteLLM forwarder `http://127.0.0.1:<litellmPort>/v1/models`
# (network.nix / guest.nix forward it to the bridge-only host endpoint) — and
# renders the model list into per-agent config that OVERRIDES the frozen copy:
#
#   pi        an EXTRA extension `~/.pi/agent/extensions/zz-microvm-models.ts`.
#             pi auto-discovers `~/.pi/agent/extensions/*.ts` and later
#             `registerProvider(<same key>)` calls replace earlier ones, and the
#             `zz-` prefix sorts after `myconfig-providers.ts`, so the runtime
#             list wins. Written INTO the home, NEXT TO the copy — which is a
#             plain, agent-owned file the config seeder wrote with
#             `cp -R --dereference` (../config-seed.nix), not a store symlink,
#             but is left untouched all the same.
#   opencode  an OVERLAY config at `$XDG_RUNTIME_DIR`-like
#             `/run/agent-model-config/opencode.json`, pointed at by
#             `OPENCODE_CONFIG`. opencode loads that file IN ADDITION to (and
#             after) the global `~/.config/opencode/opencode.json`, deep-merging
#             it — so the overlay only has to carry the provider's `models` map.
#
# Fail-SOFT by design: if the endpoint is unreachable (offline network profile,
# host proxy down, boot race) the script logs and exits 0, leaving the
# build-time configs in place. A sandbox must still boot and be usable for
# non-model work.
#
# No secrets are involved: the discovered data is a list of model IDs, and the
# API key stays the `not-needed` placeholder (§17 — the real upstream
# credential never leaves the host LiteLLM proxy).
{
  config,
  lib,
  pkgs,
  agentNetwork,
  # The ONE definition of the RUNTIME configuration staging (../config-seed.nix):
  # the unit that provisions the guest home, which this one must be ordered
  # after (it writes into that same home).
  agentConfigSeed,
  ...
}:
let
  cfg = config.myconfig.ai.microvm;
  mc = cfg.guestModelConfig;

  # The guest-visible model endpoint: the loopback forwarder, i.e. the exact
  # base URL the copied agent configs already point at.
  endpoint = "http://127.0.0.1:${toString cfg.litellmPort}";

  runtimeDirName = "agent-model-config";
  runtimeDir = "/run/${runtimeDirName}";
  opencodeConfig = "${runtimeDir}/opencode.json";
  piExtension = ".pi/agent/extensions/zz-microvm-models.ts";

  agent-model-config = pkgs.writeShellApplication {
    name = "agent-model-config";
    runtimeInputs = with pkgs; [
      coreutils
      curl
      jq
    ];
    text = ''
      set -euo pipefail

      readonly ENDPOINT=${lib.escapeShellArg endpoint}
      readonly PROVIDER_KEY=${lib.escapeShellArg mc.providerKey}
      readonly PROVIDER_NAME=${lib.escapeShellArg mc.providerName}
      readonly OUT_DIR=${lib.escapeShellArg runtimeDir}
      readonly OPENCODE_OUT=${lib.escapeShellArg opencodeConfig}
      readonly PI_OUT="$HOME/"${lib.escapeShellArg piExtension}
      readonly DEFAULT_CONTEXT_WINDOW=${toString mc.defaultContextWindow}
      readonly MAX_TOKENS_CAP=${toString mc.maxTokensCap}
      readonly ATTEMPTS=${toString mc.attempts}
      readonly RETRY_DELAY=${toString mc.retryDelaySeconds}
      readonly TIMEOUT=${toString mc.timeoutSeconds}

      log() { printf 'agent-model-config: %s\n' "$*" >&2; }

      # --- 1. discover the models the proxy ACTUALLY serves ---------------
      models_raw=""
      for attempt in $(seq 1 "$ATTEMPTS"); do
          if models_raw=$(curl -fsS --max-time "$TIMEOUT" "$ENDPOINT/v1/models"); then
              break
          fi
          models_raw=""
          log "attempt $attempt/$ATTEMPTS: $ENDPOINT/v1/models unreachable"
          if [[ "$attempt" -lt "$ATTEMPTS" ]]; then
              sleep "$RETRY_DELAY"
          fi
      done

      # Fail SOFT: keep the build-time agent configs, boot normally.
      if [[ -z "$models_raw" ]]; then
          log "model endpoint unreachable; keeping the build-time agent configs"
          exit 0
      fi

      if ! ids=$(jq -ce '[.data[]?.id // empty] | unique' <<<"$models_raw"); then
          log "unparsable /v1/models response; keeping the build-time agent configs"
          exit 0
      fi
      if [[ "$(jq -r 'length' <<<"$ids")" -eq 0 ]]; then
          log "endpoint reports zero models; keeping the build-time agent configs"
          exit 0
      fi
      log "discovered $(jq -r 'length' <<<"$ids") model(s) at $ENDPOINT"

      # --- 2. enrich with real context windows, when exposed -------------
      # LiteLLM's /model/info (admin route, may be unauthorised) carries the
      # per-model max_input_tokens. Purely optional: on any failure every
      # model falls back to DEFAULT_CONTEXT_WINDOW, exactly like the host-side
      # generators do for models with no discoverable value.
      context_windows='{}'
      if info_raw=$(curl -fsS --max-time "$TIMEOUT" "$ENDPOINT/model/info" 2>/dev/null); then
          context_windows=$(
              jq -ce '
                [ .data[]?
                  | { key: (.model_name // ""),
                      value: ( .litellm_params.max_input_tokens
                             // .model_info.max_input_tokens
                             // empty ) }
                  | select(.key != "" and (.value | type) == "number")
                ] | from_entries
              ' <<<"$info_raw"
          ) || context_windows='{}'
      fi

      # --- 3. opencode overlay config ------------------------------------
      # Deep-merged by opencode ON TOP of the copied global config, so only the
      # provider block needs to be written. `npm`/`options` are repeated so the
      # overlay also stands on its own if the global copy is absent.
      mkdir -p -- "$OUT_DIR"
      opencode_tmp=$(mktemp "$OUT_DIR/opencode.json.XXXXXX")
      jq -n \
          --argjson ids "$ids" \
          --argjson cw "$context_windows" \
          --argjson dcw "$DEFAULT_CONTEXT_WINDOW" \
          --arg key "$PROVIDER_KEY" \
          --arg name "$PROVIDER_NAME" \
          --arg base "$ENDPOINT/v1" \
          '{
             provider: {
               ($key): {
                 npm: "@ai-sdk/openai-compatible",
                 name: $name,
                 options: { baseURL: $base },
                 models: ( $ids
                           | map({ key: ., value: { name: ., contextWindowSize: ($cw[.] // $dcw) } })
                           | from_entries )
               }
             }
           }' >"$opencode_tmp"
      mv -f -- "$opencode_tmp" "$OPENCODE_OUT"
      log "wrote $OPENCODE_OUT"

      # --- 4. pi extension ------------------------------------------------
      # The extension directory is inside the DISPOSABLE guest home, which
      # ../config-seed.nix creates as agent-owned `u+rwX` and fills with a
      # symlink-dereferencing copy — so it is always a real, writable directory
      # (or absent, on a host that stages no pi config at all). There is no
      # guest home-manager any more, hence no read-only store symlink to work
      # around here.
      pi_dir=$(dirname -- "$PI_OUT")
      mkdir -p -- "$pi_dir"
      providers=$(
          jq -n \
              --argjson ids "$ids" \
              --argjson cw "$context_windows" \
              --argjson dcw "$DEFAULT_CONTEXT_WINDOW" \
              --argjson maxTokensCap "$MAX_TOKENS_CAP" \
              --arg key "$PROVIDER_KEY" \
              --arg name "$PROVIDER_NAME" \
              --arg base "$ENDPOINT/v1" \
              '{
                 ($key): {
                   name: $name,
                   baseUrl: $base,
                   api: "openai-completions",
                   apiKey: "dummy",
                   authHeader: false,
                   models: ( $ids | map({
                     id: .,
                     name: .,
                     reasoning: false,
                     input: [ "text" ],
                     cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 },
                     # Output budget pi sends as the request `max_tokens`.
                     # A quarter of the context window, capped — the same
                     # formula the host-side generators use. A fixed small
                     # value (this used to be 4096) makes reasoning models
                     # stop mid-answer with finish_reason "length".
                     maxTokens: ([ ((($cw[.] // $dcw) / 4) | floor), $maxTokensCap ] | min),
                     contextWindow: ($cw[.] // $dcw)
                   }) )
                 }
               }'
      )
      pi_tmp=$(mktemp "$pi_dir/.zz-microvm-models.ts.XXXXXX")
      cat >"$pi_tmp" <<EOF
      // Auto-generated at guest boot by agent-model-config (myconfig.ai.microvm).
      // Runtime model list of the host LiteLLM proxy; overrides the build-time
      // myconfig-providers.ts registration of the same provider key.
      import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

      const providers: Record<string, any> = $providers;

      export default function (pi: ExtensionAPI) {
        for (const [key, config] of Object.entries(providers)) {
          pi.registerProvider(key, config);
        }
      }
      EOF
      mv -f -- "$pi_tmp" "$PI_OUT"
      log "wrote $PI_OUT"
    '';
    meta = with lib; {
      description = "Render guest agent model configs from the live LiteLLM model list (myconfig.ai.microvm)";
      platforms = platforms.linux;
    };
  };

  # Only meaningful when the resolved network profile actually allows the model
  # API: under `offline` there is no forwarder to query, so no unit is created.
  active = mc.enable && agentNetwork.caps.litellm;

  guestModule = lib.optionalAttrs active {
    environment.systemPackages = [ agent-model-config ];

    systemd.services.agent-model-config = {
      description = "Render guest agent model configs from the live LiteLLM model list";
      wantedBy = [ "multi-user.target" ];
      wants = [
        "network-online.target"
        "litellm-forwarder.socket"
      ];
      after = [
        "network-online.target"
        "litellm-forwarder.socket"
      ]
      # Whatever PROVISIONS the home must be finished first: this unit writes
      # `$HOME/.pi/agent/extensions/zz-microvm-models.ts` into that same home,
      # and the launch-time seeding oneshot does `cp -R` + `chown -R` +
      # `chmod -R` over the WHOLE home, so writing into it concurrently would
      # race (../config-seed.nix states the matching `before=`).
      ++ [ agentConfigSeed.guestUnit ];
      # Ordered before the batch job CONTROLLER (which starts the untrusted
      # worker), exactly like agent-state-link, so an unattended job never
      # starts against a stale model list. Interactive logins happen far later.
      before = [ "agent-job-controller.service" ];
      # Explicit, so the script's `$HOME` never depends on systemd's account
      # lookup; this is the guest `agent` user's home (guest.nix).
      environment.HOME = "/home/agent";
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        User = "agent";
        # Owned by `agent`, and kept for the lifetime of the unit
        # (RemainAfterExit) so `OPENCODE_CONFIG` stays valid for every session.
        RuntimeDirectory = runtimeDirName;
        RuntimeDirectoryMode = "0755";
        RuntimeDirectoryPreserve = "yes";
        ExecStart = lib.getExe agent-model-config;
      };
    };
  };
in
{
  options.myconfig.ai.microvm.guestModelConfig = with lib; {
    enable = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Run a guest boot-time oneshot that queries the loopback LiteLLM
        endpoint (`http://127.0.0.1:<litellmPort>/v1/models`) and renders the
        LIVE model list into pi + opencode config, overriding the build-time
        model lists copied from the host primary user's dotfiles. Fails soft:
        an unreachable endpoint leaves the copied configs untouched.
      '';
    };

    providerKey = mkOption {
      type = types.str;
      default = "${config.networking.hostName}-litellm";
      defaultText = literalExpression ''"''${config.networking.hostName}-litellm"'';
      description = ''
        Provider key registered in the generated pi extension and opencode
        overlay. Defaults to the SAME key the host-side generators use
        (`<hostname>-litellm`), so the runtime model list replaces (rather
        than duplicates) the build-time one.
      '';
    };

    providerName = mkOption {
      type = types.str;
      default = "LiteLLM (${config.networking.hostName} microVM)";
      defaultText = literalExpression ''"LiteLLM (''${config.networking.hostName} microVM)"'';
      description = "Human-readable provider name shown by the agents.";
    };

    defaultContextWindow = mkOption {
      type = types.ints.positive;
      default = 131072;
      description = ''
        Context window used for models whose real value the endpoint does not
        expose. Same last-resort fallback as the host-side pi/opencode
        generators; never overrides a discovered value.
      '';
    };

    maxTokensCap = mkOption {
      type = types.ints.positive;
      default = 65536;
      description = ''
        Upper bound for the `maxTokens` reported per model in the generated
        pi extension. The reported value is
        `min(contextWindow / 4, maxTokensCap)` — the same formula the
        host-side generators use. pi sends this as the request's
        `max_tokens`, so a small value truncates long (especially
        reasoning) answers with `finish_reason: "length"`.
      '';
    };

    attempts = mkOption {
      type = types.ints.positive;
      default = 5;
      description = "Number of times the model endpoint is queried before giving up.";
    };

    retryDelaySeconds = mkOption {
      type = types.ints.positive;
      default = 2;
      description = "Delay between endpoint query attempts.";
    };

    timeoutSeconds = mkOption {
      type = types.ints.positive;
      default = 5;
      description = "Per-request timeout for the endpoint queries.";
    };
  };

  config._module.args.agentModelConfig = {
    inherit
      active
      guestModule
      endpoint
      opencodeConfig
      piExtension
      ;
    script = agent-model-config;
    # Environment the agents need to pick the generated config up. Merged into
    # the guest's single `modelEndpointEnv` (guest.nix), so BOTH the interactive
    # login shell and the non-login batch worker see it.
    guestEnvironment = lib.optionalAttrs active { OPENCODE_CONFIG = opencodeConfig; };
  };
}
