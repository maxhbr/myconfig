# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The idempotent `~/.hermes/config.yaml` seeder shared by every hermes CLI
# tier (the mysbx sandbox launcher of ./mysbx.nix, the host CLI launcher of
# ./service.nix).
#
# WHY a seeder and not a home-manager-managed file: hermes keeps
# `config.yaml`, `.env`, `auth.json`, `state.db` and `sessions/` in ONE root
# (`hermes_constants.py` `get_hermes_home()`), i.e. configuration is
# inseparable from credentials and runtime state there — a `home.file`
# entry would fight the CLI's own writes (`hermes config`, `hermes model`,
# the setup wizard, the aoe hook installer). The same mechanism
# ../programs/programs.claude-code uses (its `noninteractive-startup.nix`
# launcher seeder).
#
# WHY the merge is REPAIR-shaped and not seed-only: the original
# "write only a fresh home" seeder proved insufficient in practice. The
# persisted config is rewritten by hermes itself (`hermes setup`/`migrate`
# write the full `DEFAULT_CONFIG` document) and by other tools (aoe installs
# its `hooks:` block), and ONE such rewrite on 2026-10-19 left the sandbox's
# `state-dirs` config with `model: ''` — no provider at all, and a seeder
# that refuses to touch an existing file can never heal that. So the seeder
# now runs on EVERY launch and deep-merges the LiteLLM route back in.
#
# The merge rules (hermes-seed-merge below) are deliberately narrow:
#
#   * `model.provider`, `model.base_url`, `model.api_key` are ALWAYS
#     re-asserted — they are the endpoint facts; anything else leaves hermes
#     unusable (no provider configured → first-run wizard). The podman/
#     microvm backends override `base_url` per run via `CUSTOM_BASE_URL`
#     anyway, so re-asserting the host URL never pins a container session to
#     an unreachable endpoint.
#   * `model.default` is only set when MISSING/empty — a `hermes model`
#     choice (bd myconfig-j6i's `/model` flow) survives.
#   * the named `providers.<hostname>-litellm` entry (host tier only, when
#     `providerEntry != null`) is created when absent and its
#     `base_url`/`api_key`/`models` re-asserted, so the model picker knows
#     the proxy and its model list; OTHER provider entries the user added
#     are never touched.
#   * `skills.external_dirs` is UNION-merged: the seeded entries (when
#     `externalSkillDirs != []`) are appended to whatever the config
#     already lists, and nothing is ever removed — same repair shape as
#     the providers rule above.
#   * everything else (hooks, toolsets, display, the aoe blocks, …) is
#     preserved byte-for-byte in shape — the file is rewritten via
#     `yaml.safe_dump`, but only the keys above change.
#
# A corrupt/unparseable config is NEVER overwritten — hermes has its own
# recovery, and destroying the file could lose credentials.
{
  lib,
  pkgs,
  # OpenAI-compatible base URL of the model route (incl. `/v1`).
  baseUrl,
  # Placeholder key the loopback proxy accepts (upstream value: `local-key`).
  apiKey,
  # Default model name (or `null`); written only when the config has none.
  defaultModel,
  # Named `providers.<key>` entry for the model picker, or `null` for the
  # sandbox tier (which must stay minimal). Shape:
  #   { key = "f13-litellm"; name = "LiteLLM (f13)"; models = [ … ]; }
  providerEntry ? null,
  # Extra skill directories to union-merge into the config's
  # `skills.external_dirs` (empty list = leave the skills section
  # untouched).
  externalSkillDirs ? [ ],
}:
let
  mergePy =
    pkgs.writers.writePython3 "hermes-seed-merge"
      {
        libraries = with pkgs.python3Packages; [ pyyaml ];
        flakeIgnore = [
          "E401"
          "E501"
          "W503"
        ];
      }
      ''
        import json
        import os
        import sys
        import yaml
        from pathlib import Path

        route_json, provider_json, skills_json, config_path = sys.argv[1], sys.argv[2], sys.argv[3], Path(sys.argv[4])
        # `route_json`/`provider_json` are FILE paths (see the `routeFile`
        # comment), not inline JSON strings.
        with open(route_json) as f:
            route = json.load(f)
        provider = None
        if provider_json != "null":
            with open(provider_json) as f:
                provider = json.load(f)
        skills_dirs = None
        if skills_json != "null":
            with open(skills_json) as f:
                skills_dirs = json.load(f)

        config = {}
        if config_path.exists():
            try:
                with open(config_path) as f:
                    config = yaml.safe_load(f) or {}
            except Exception as e:
                # Never destroy a file we cannot parse: hermes has its own
                # recovery, and the file also holds credentials.
                print(f"hermes-seed-merge: {config_path} unreadable ({e}) — left untouched", file=sys.stderr)
                sys.exit(0)
        if not isinstance(config, dict):
            config = {}

        # `model` may be missing, a plain string (the DEFAULT_CONFIG shape) or
        # a dict (the configured shape). `_normalize_root_model_keys` upstream
        # treats a non-empty string as `model.default`, so keep it when set.
        default_keep = ""
        model = config.get("model")
        if isinstance(model, str):
            default_keep = model.strip()
            model = {}
        elif not isinstance(model, dict):
            model = {}

        model["provider"] = "custom"
        model["base_url"] = route["base_url"]
        model["api_key"] = route["api_key"]
        # `route["default"]` is null when no default model is configured
        # (myconfig.ai.dev.hermes.model.default = null): only seed it when
        # the route actually carries one.
        if not (model.get("default") or default_keep):
            if route["default"] is not None:
                model["default"] = route["default"]
        elif default_keep and not model.get("default"):
            model["default"] = default_keep
        config["model"] = model

        if provider is not None:
            providers = config.get("providers")
            if not isinstance(providers, dict):
                providers = {}
            entry = providers.get(provider["key"])
            if not isinstance(entry, dict):
                entry = {}
            entry["name"] = provider["name"]
            entry["base_url"] = route["base_url"]
            entry["api_key"] = route["api_key"]
            entry["models"] = provider["models"]
            providers[provider["key"]] = entry
            config["providers"] = providers

        if skills_dirs is not None:
            skills = config.get("skills")
            if not isinstance(skills, dict):
                skills = {}
            dirs = skills.get("external_dirs") or []
            if isinstance(dirs, str):
                dirs = [dirs]
            elif not isinstance(dirs, list):
                dirs = []
            skills["external_dirs"] = dirs + [d for d in skills_dirs if d not in dirs]
            config["skills"] = skills

        tmp = str(config_path) + ".hermes-seed-merge.tmp"
        with open(tmp, "w") as f:
            yaml.safe_dump(config, f, default_flow_style=False, sort_keys=False)
        os.replace(tmp, config_path)
      '';

  # The merge facts travel as FILES, not argv: the provider model list is
  # long (every LiteLLM `model_name`), and a JSON literal on the command
  # line would also trip shellcheck's SC2140 (`writeShellApplication`
  # runs it on the generated script).
  routeFile = pkgs.writeText "hermes-seed-route.json" (
    builtins.toJSON {
      base_url = baseUrl;
      api_key = apiKey;
      default = defaultModel;
    }
  );
  providerFile = pkgs.writeText "hermes-seed-provider.json" (
    if providerEntry == null then
      "null"
    else
      builtins.toJSON {
        inherit (providerEntry) key name;
        models = providerEntry.models;
      }
  );
  skillsFile = pkgs.writeText "hermes-seed-skills.json" (
    if externalSkillDirs == [ ] then "null" else builtins.toJSON externalSkillDirs
  );

  seedScript = ''
    hermes_home="''${HERMES_HOME:-$HOME/.hermes}"
    config="$hermes_home/config.yaml"

    if [ -e "$config" ]; then
      # Existing config (a `state-dirs` store, the host home): repair-merge
      # the LiteLLM route into it. Never a destructive rewrite.
      # (`writePython3` emits a single executable AT $out — no `bin/`
      # inside — so the script is referenced directly, not via getExe.)
      ${mergePy} ${routeFile} ${providerFile} ${skillsFile} "$config" || true
    else
      mkdir -p "$hermes_home"
      (umask 077 && cat ${seedConfig} >"$config")
    fi
  '';

  # The seeded fresh-home config: model route (and the extra skill
  # directories, when seeded) only, so every other key keeps the upstream
  # default. `provider = "custom"` + a non-empty
  # `base_url`/`api_key` is also what `main.py` `_has_any_provider_configured`
  # accepts, so no first-run setup wizard appears in a fresh sandbox home.
  seedConfig = (pkgs.formats.yaml { }).generate "hermes-mysbx-config.yaml" (
    {
      model = {
        provider = "custom";
        base_url = baseUrl;
        api_key = apiKey;
      }
      // (lib.optionalAttrs (defaultModel != null) {
        default = defaultModel;
      });
    }
    // lib.optionalAttrs (providerEntry != null) {
      providers."${providerEntry.key}" = {
        name = providerEntry.name;
        base_url = baseUrl;
        api_key = apiKey;
        models = providerEntry.models;
      };
    }
    // lib.optionalAttrs (externalSkillDirs != [ ]) {
      skills.external_dirs = externalSkillDirs;
    }
  );
in
{
  inherit mergePy seedConfig;
  seed = pkgs.writeShellApplication {
    name = "hermes-mysbx-seed-config";
    runtimeInputs = [ pkgs.coreutils ];
    text = seedScript;
  };
}
