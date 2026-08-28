# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

{
  pkgs,
  lib,
  config,
  ...
}:
{

  imports = [
    #{
    #  # TODO: Litellm is packaged without generated prisma schema and they can not be generated on the fly with being in /nix/store:  https://github.com/NixOS/nixpkgs/issues/432925
    #  config = lib.mkIf (config.myconfig.ai.enable && config.services.litellm.enable && config.services.litellm.settings.general_settings.disable_spend_logs == false) {
    #    services.litellm = {
    #      settings.general_settings = {
    #        store_prompts_in_spend_logs = true;
    #        # disable_spend_logs = false;
    #        maximum_spend_logs_retention_period = "120d";
    #        database_url = "postgresql://litellm:litellm@127.0.0.1:${toString config.services.postgresql.port}/litellm";
    #      };
    #    };
    #    services.postgresql = {
    #      enable = true;
    #      port = 5432;
    #      ensureDatabases = [ "litellm" ];
    #      authentication = pkgs.lib.mkOverride 10 ''
    #        #type database DBuser origin-address auth-method
    #        local all      all     trust
    #        # ipv4
    #        host  all      all     127.0.0.1/32   trust
    #        # ipv6
    #        host  all      all     ::1/128        trust
    #      '';
    #      initialScript = pkgs.writeText "backend-initScript" ''
    #        CREATE ROLE litellm WITH LOGIN PASSWORD 'litellm' CREATEDB;
    #        CREATE DATABASE litellm;
    #        GRANT ALL PRIVILEGES ON DATABASE litellm TO litellm;
    #      '';
    #    };
    #  };
    #}
  ];
  config = lib.mkIf (config.myconfig.ai.enable && config.services.litellm.enable) {
    # # The nixpkgs litellm module uses DynamicUser = true, which requires
    # # /var/lib/litellm to be a symlink → /var/lib/private/litellm.
    # # However, its own systemd.tmpfiles.rules creates /var/lib/litellm as a
    # # real directory (via "d /var/lib/litellm/ui"), so when litellm.service
    # # starts, DynamicUser's symlink creation fails with STATUS=238/STATE_DIRECTORY
    # # ("File exists"). On impermanence systems this happens on every boot.
    # #
    # # Fix: run a oneshot service after tmpfiles-setup but before litellm that
    # # converts the real directory to the symlink DynamicUser expects.
    # systemd.services.litellm-state-dir-fix = {
    #   description = "Fix litellm state directory for DynamicUser";
    #   before = [ "litellm.service" ];
    #   wantedBy = [ "litellm.service" ];
    #   after = [ "systemd-tmpfiles-setup.service" ];
    #   serviceConfig = {
    #     Type = "oneshot";
    #     RemainAfterExit = true;
    #     ExecStart = pkgs.writeShellScript "litellm-state-dir-fix" ''
    #       set -euo pipefail
    #       # Ensure the private state directory exists (DynamicUser target)
    #       mkdir -p /var/lib/private/litellm
    #       # If tmpfiles created /var/lib/litellm as a real directory, replace it
    #       # with the relative symlink that DynamicUser expects.
    #       if [ -d /var/lib/litellm ] && [ ! -L /var/lib/litellm ]; then
    #         rm -rf /var/lib/litellm
    #         ln -s private/litellm /var/lib/litellm
    #       elif [ ! -e /var/lib/litellm ]; then
    #         ln -s private/litellm /var/lib/litellm
    #       fi
    #     '';
    #   };
    # };

    services.litellm = {
      host = lib.mkForce "127.0.0.1";
      port = lib.mkForce 4000;
      # `expression`, `prometheus-client` and the other proxy runtime deps
      # are all covered by the nixpkgs `optional-dependencies` lists below
      # (`expression` 5.6.0 is packaged by the pinned `nixpkgs` input now,
      # so no local build is needed anymore).
      #
      # litellm 1.97.0 still imports `fastapi.dependencies.utils.get_flat_dependant`,
      # which fastapi >= 0.140.7 removed (the pinned `nixpkgs` input ships
      # 0.141.x), so the proxy dies at startup with that `ImportError`.
      # Apply the upstream fix (BerriAI/litellm f9b86b253a3f) to the pinned
      # source instead: the `get_flat_params` + `ParamTypes` form works in
      # fastapi 0.139.0 and 0.141.x alike. The patch step is
      # self-invalidating — its `grep` matches nothing while a litellm
      # source without that import is bundled, so it silently does nothing
      # once nixpkgs bundles a release containing the fix.
      # See doc/TODOs/drop-litellm-fastapi-source-patch.md.
      #
      # The pinned litellm definition is a lazy def; only `overridePythonAttrs`
      # (not a plain `overrideAttrs`) accepts the classic one-argument
      # `oldAttrs` lambda. `postPatch` is appended to litellm's own
      # (which pins the maturin version in `pyproject.toml`);
      # `pythonImportsCheck` adds `litellm` itself plus the module that
      # used to crash (`_declared_query_params`), so the build fails
      # loudly again if the source-vs-fastapi incompatibility regresses.
      package = pkgs.python3Packages.toPythonApplication (
        pkgs.python3Packages.litellm.overridePythonAttrs (oldAttrs: {
          postPatch = (oldAttrs.postPatch or "") + ''
                        # BerriAI/litellm f9b86b253a3f ("fix(proxy): restore
                        # query-param validation under fastapi>=0.140.7"). No-op while
                        # the bundled litellm source lacks the old import.
                        files=$(grep -r --include="*.py" -l "^from fastapi.dependencies.utils import get_flat_dependant" . || true)
                        if [ -n "$files" ]; then
                          python - $files <<'PYEOF'
            import re
            import sys

            OLD_IMPORT = "from fastapi.dependencies.utils import get_flat_dependant"
            NEW_IMPORTS = (
                "from fastapi.dependencies.utils import get_flat_params"
                "\nfrom fastapi.params import ParamTypes"
            )

            pat = re.compile(
                r"^(\s*)return frozenset\(field\.alias"
                r" for field in get_flat_dependant\(dependant, skip_repeats=True\)"
                r"\.query_params\)\s*$",
                re.MULTILINE,
            )

            for path in sys.argv[1:]:
                with open(path) as f:
                    s = f.read()
                s = s.replace(OLD_IMPORT, NEW_IMPORTS)
                m = pat.search(s)
                assert m, (
                    f"unexpected litellm source in {path}: old "
                    "get_flat_dependant import found, but the expected call "
                    "shape is missing or shaped differently; refusing to "
                    "rewrite only part of it"
                )
                s = s[: m.start()] + (
                    m.group(1) + "return frozenset(\n"
                    + m.group(1) + "    field.alias\n"
                    + m.group(1) + "    for field in get_flat_params(dependant)\n"
                    + m.group(1) + '    if getattr(field.field_info, "in_", None) == ParamTypes.query\n'
                    + m.group(1) + ")"
                ) + s[m.end():]
                assert "get_flat_dependant" not in s, (
                    f"unexpected litellm source in {path}: get_flat_dependant still "
                    "present after rewrite"
                )
                with open(path, "w") as f:
                    f.write(s)
                print("fastapi>=0.140.7 compatibility patch applied to", path)
            PYEOF
                        fi
          '';
          # Extend any existing import checks rather than replacing them.
          pythonImportsCheck = (oldAttrs.pythonImportsCheck or [ ]) ++ [
            "litellm"
            "litellm.proxy.management_endpoints.management_v1.common"
          ];
          # PEP 517 closure: base dependencies plus the extra lists the
          # nixpkgs module adds for the service: proxy + extra_proxy +
          # proxy-runtime (the last includes prometheus-client, needed
          # for the `prometheus` callback configured below when
          # observability is enabled; `expression` ships in these lists
          # too).
          dependencies =
            (oldAttrs.dependencies or [ ])
            ++ pkgs.python3Packages.litellm.optional-dependencies.proxy
            ++ pkgs.python3Packages.litellm.optional-dependencies.extra_proxy
            ++ pkgs.python3Packages.litellm.optional-dependencies.proxy-runtime;
        })
      );

      settings.general_settings = {
        disable_spend_logs = true;
        request_timeout = 3600; # 60 minutes, upstream default is 600s (10 min)
      };
      settings.litellm_settings = lib.mkIf config.myconfig.observability.client.enable {
        callbacks = [ "prometheus" ];
      };
      # `mkDefault` (priority 1000) so that a host can override this
      # generated list with a plain `=` (priority 100) while still
      # *merging* (concatenating) with other priority-100 definitions
      # — e.g. the skainet/trustedtokens entries added by the tng.nix
      # flake. Without this, a host that wants to replace the
      # auto-generated prefixed entries has to use `mkForce` (priority
      # 50), which silently discards those other contributors. See
      # hosts/shared.litellm.proxy.nix for the override case.
      settings.model_list = lib.mkDefault (
        lib.optionals (config.myconfig.ai.localModels != [ ]) (
          lib.concatMap (
            model:
            let
              hostPort = "${model.host}:${toString model.port}";
              providerName = if model.name != null then model.name else hostPort;
              modelNames = if model.models != [ ] then model.models else [ providerName ];
            in
            lib.concatMap (
              modelEntry:
              let
                modelName = if lib.isAttrs modelEntry then modelEntry.name else modelEntry;
                # Computed in router.nix / llama-swap.nix:
                #   - "base" / "variant" -> a real llama-cpp model section,
                #     published as `${providerName}:${modelName}`.
                #   - "alias"            -> piggybacks on a base/variant
                #     model. Additionally emit a bare `${modelName}`
                #     litellm entry so callers can use the short form
                #     across providers (this preserves the old
                #     three-entries-per-alias behaviour where aliases
                #     used to be nested under a parent model entry).
                #   - null               -> upstream-provided name with
                #     no classification (e.g. shared.localModels.litellm.nix);
                #     emit only the prefixed form.
                modelKind = if lib.isAttrs modelEntry then (modelEntry.kind or null) else null;
                modelTags = if lib.isAttrs modelEntry then (modelEntry.tags or [ ]) else [ ];
                modelContextWindow = if lib.isAttrs modelEntry then (modelEntry.contextWindow or null) else null;
                # `litellm_params.tags` is LiteLLM's standard tag field
                # (used by tag-based routing and surfaced on
                # /model/info). The list assembled here is, in order
                # (deduped, first occurrence wins):
                #   1. the localModels `kind` ("base"/"variant"/"alias"),
                #      so downstream tools can filter on classification
                #      without re-parsing the model name;
                #   2. the provider name (e.g. "rtx5090", "gfx1151") so
                #      tag-based routing can pin requests to a specific
                #      backend GPU. When `serviceProviderName` is unset,
                #      `providerName` falls back to "<host>:<port>",
                #      which is still a stable per-backend label;
                #   3. the lineage + user-provided tags the publisher
                #      attached (see
                #      `myconfig.ai.localModels.<provider>.models[*].tags`).
                tagList = lib.unique (
                  (lib.optional (modelKind != null) modelKind) ++ [ providerName ] ++ modelTags
                );
                litellmParams = {
                  model = "openai/${modelName}";
                  api_base = "http://${hostPort}/v1";
                  api_key = "not-needed";
                  request = {
                    allowPrivateNetwork = true;
                  };
                }
                // lib.optionalAttrs (tagList != [ ]) { tags = tagList; }
                // lib.optionalAttrs (modelContextWindow != null) {
                  max_input_tokens = modelContextWindow;
                  max_tokens = lib.min (modelContextWindow / 4) 65536;
                };
                entry = {
                  model_name = "${providerName}:${modelName}";
                  litellm_params = litellmParams;
                };
              in
              [ entry ]
              ++ lib.optional (modelKind == "alias") {
                model_name = modelName;
                litellm_params = litellmParams;
              }
            ) modelNames
          ) config.myconfig.ai.localModels
        )
      );
      # settings.router_settings = {
      #   model_group_alias = {
      #     "gpt-4" = "gpt-4o"
      #   };
      # };
    };

    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          (writeShellApplication {
            name = "litellm-logs";
            text = ''
              set -euo pipefail
              journalctl --follow --pager-end --unit litellm.service
            '';
          })
          (writeShellApplication {
            name = "litellm-restart";
            text = ''
              set -euo pipefail
              echo "Restarting LiteLLM..."
              sudo systemctl restart litellm.service
              echo "LiteLLM restarted. Check status with: systemctl status litellm.service"
            '';
          })
        ];
      }
    ];
  };
}
