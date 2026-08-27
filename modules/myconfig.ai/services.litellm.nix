# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

{
  pkgs,
  lib,
  config,
  ...
}:
let
  # `expression>=5.6.0,<6.0` is a runtime dependency of litellm 1.97.0's
  # `[proxy]` extra — imported in
  # litellm/proxy/_experimental/mcp_server/outbound_credentials/types.py.
  # It is not packaged in the pinned `nixpkgs` yet, only in the `master`
  # nixpkgs input (upstream PR #555030, merged 2026-08-24, commit
  # 432724c94f15). Build it here against the pinned nixpkgs' Python so the
  # interpreter derivation matches the one litellm uses — mixing Python
  # packages built from different nixpkgs revisions fails the nixpkgs
  # same-Python-interpreter check, even when the version string is identical.
  # TODO: replace with `pkgs.python3Packages.expression` once the `nixpkgs`
  # input packages it.
  expression = pkgs.python3Packages.buildPythonPackage (finalAttrs: {
    pname = "expression";
    version = "5.6.0";
    pyproject = true;
    src = pkgs.python3Packages.fetchPypi {
      inherit (finalAttrs) pname version;
      sha256 = "sha256-RU9v4Tg0cZSkPH+HjZWO/puEucx3DkYgEMelLhgFgGU=";
    };
    build-system = [ pkgs.python3Packages.poetry-core ];
    dependencies = [ pkgs.python3Packages.typing-extensions ];
    pythonImportsCheck = [ "expression" ];
  });
in
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
      # litellm 1.97.0's `[proxy]` extra needs two things that are not
      # both available in a single nixpkgs input right now:
      #
      # 1. `expression>=5.6.0,<6.0` — imported at runtime in
      #    litellm/proxy/_experimental/mcp_server/outbound_credentials/types.py.
      #    Not packaged in the pinned `nixpkgs` (only in `master`, PR #555030).
      #    Built locally against the pinned Python above (see `expression`
      #    let-binding) so the interpreter derivation matches litellm's.
      #
      # 2. fastapi — litellm 1.97.0 imports `get_flat_dependant` from
      #    `fastapi.dependencies.utils`, which was removed in fastapi 0.141.x.
      #    The `master` input already ships fastapi 0.141.1 (broken), while the
      #    pinned `nixpkgs` input still ships fastapi 0.139.0 (works).
      #
      # So the litellm package is taken from the pinned `nixpkgs` input (for
      # a compatible fastapi), and `expression` is built locally. The nixpkgs
      # `litellm` by-name wrapper adds proxy + extra_proxy + proxy-runtime
      # (the last already includes prometheus-client, needed for the
      # `prometheus` callback configured below when observability is enabled);
      # we replicate that here and add the missing `expression`.
      # TODO: once the `nixpkgs` input packages `expression` (or litellm drops
      # the `get_flat_dependant` import for newer fastapi), replace this with
      # plain `pkgs.litellm`.
      package = pkgs.python3Packages.toPythonApplication (
        pkgs.python3Packages.litellm.overridePythonAttrs (oldAttrs: {
          dependencies =
            (oldAttrs.dependencies or [ ])
            ++ pkgs.python3Packages.litellm.optional-dependencies.proxy
            ++ pkgs.python3Packages.litellm.optional-dependencies.extra_proxy
            ++ pkgs.python3Packages.litellm.optional-dependencies.proxy-runtime
            ++ [ expression ];
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
