# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Prometheus exporter for llama-server / llama-swap model state AND
# token-generation timing metrics extracted from the systemd journal.
#
# Auto-enabled when ``myconfig.ai.llama-cpp.serviceVariant`` is set
# (either ``"llama-server"`` or ``"llama-swap"``). Port and listen
# address are derived from the service config so the user only needs
# to declare the llama-cpp service — the exporter follows automatically.
#
# Scrapes the OpenAI-compatible /v1/models JSON endpoint and exposes
# Prometheus gauges so the local vmagent can scrape them into the
# central VictoriaMetrics instance.
#
# Metrics produced (all carry the `host` label via vmagent external_labels):
#
#   * llama_server_model_status{model}           1=loaded, 0=unloaded
#   * llama_server_model_info{model,owned_by}    constant-1 info series
#   * llama_server_model_alias{model,alias}      alias registered on a model
#   * llama_server_model_ctx_size{model}         context size (from meta)
#   * llama_server_model_n_params{model}         parameter count (from meta)
#   * llama_server_model_n_embd{model}           embedding dim (from meta)
#   * llama_server_model_n_vocab{model}          vocabulary size (from meta)
#   * llama_server_model_n_gpu_layers{model}     GPU layers (parsed from args)
#   * llama_server_model_count{value}            counts per load state
#   * llama_server_scrape_success                 1 if last fetch succeeded
#   * llama_server_scrape_timestamp_seconds       unix time of last fetch
#
# Timing metrics (written to the node_exporter textfile directory,
# parsed from the llama-cpp systemd journal via journalctl -f):
#
#   * llama_server_tg_tokens_per_second          live rolling tg rate (periodic lines)
#   * llama_server_task_tg_tokens_per_second     final tg t/s per completed task
#   * llama_server_task_prompt_eval_tokens_per_second  prefill t/s per completed task
#   * llama_server_task_total_time_ms            total wall time per completed task (ms)
#   * llama_server_task_n_decoded_tokens         tokens generated per completed task
#   * llama_server_task_n_prompt_tokens          prompt tokens per completed task
#   * llama_server_timing_journal_lines_total    lines processed from journal (counter)
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.observability;
  clientCfg = cfg.client;
  llamaCfg = clientCfg.llamaServer;

  # Whether the host runs a llama-server / llama-swap service that
  # exposes /v1/models.  The path must exist (the llama-cpp module was
  # imported) AND serviceVariant must be non-null.
  hasLlamaService =
    lib.hasAttrByPath [ "myconfig" "ai" "llama-cpp" ] config
    && (config.myconfig.ai.llama-cpp.serviceVariant or null) != null;

  # Derived scrape target — maps ``0.0.0.0`` to ``localhost`` so the
  # exporter doesn't try to resolve the wildcard.
  resolvedScrapeHost =
    if hasLlamaService then
      let
        addr = config.myconfig.ai.llama-cpp.serviceListenAddress or "127.0.0.1";
      in
      if addr == "0.0.0.0" then "localhost" else addr
    else
      "localhost";

  resolvedScrapePort =
    if hasLlamaService then config.myconfig.ai.llama-cpp.servicePort or 33656 else 33656;

  pythonEnv = pkgs.python3.withPackages (
    ps: with ps; [
      prometheus-client
      requests
    ]
  );

  # Systemd unit name for the llama-cpp service whose journal we tail.
  # `services.llama-cpp` (nixpkgs upstream) creates `llama-cpp.service`.
  llamaSystemdUnit = "llama-cpp";

  # Journal-tailing timing exporter.  The Python logic lives in
  # scripts/llama-server-timing-exporter.py (no Nix interpolations
  # needed — outfile path and unit name are passed as CLI args).
  timingScript = pkgs.writeShellApplication {
    name = "llama-server-timing-exporter";
    runtimeInputs = with pkgs; [
      systemd
      python3
    ];
    text = ''
      exec ${pkgs.python3}/bin/python3 -u \
        ${./scripts/llama-server-timing-exporter.py} \
        "${llamaCfg.timingTextfileDir}/llama-server-timing.prom" \
        "${llamaSystemdUnit}"
    '';
  };

  # Model-state exporter.  The Python logic lives in
  # scripts/llama-server-model-exporter.py; config is passed via
  # environment variables so the script contains no Nix interpolations.
  exporter = pkgs.writeShellApplication {
    name = "llama-server-model-exporter";
    runtimeInputs = [ pythonEnv ];
    text = ''
      export LLAMA_EXPORTER_HOST="${llamaCfg.scrapeHost}"
      export LLAMA_EXPORTER_PORT="${toString llamaCfg.scrapePort}"
      export LLAMA_EXPORTER_EXPORTER_PORT="${toString llamaCfg.exporterPort}"
      export LLAMA_EXPORTER_SCRAPE_INTERVAL="${toString llamaCfg.scrapeIntervalSeconds}"
      exec ${pythonEnv}/bin/python3 -u ${./scripts/llama-server-model-exporter.py}
    '';
  };
in
{
  options.myconfig.observability.client.llamaServer = with lib; {
    enable = mkEnableOption {
      text = ''
        Prometheus exporter that scrapes the llama-server / llama-swap
        ``GET /v1/models`` JSON endpoint and exposes Prometheus gauges
        for the local vmagent to collect. Works with both the
        ``serviceVariant = "llama-server"`` (single llama-server
        instance) and ``serviceVariant = "llama-swap"`` (llama-swap
        multiplexer) backends. Auto-enabled when
        ``myconfig.ai.llama-cpp.serviceVariant`` is set (non-null).
      '';
      default = false;
    };

    scrapeHost = mkOption {
      type = types.str;
      default = "localhost";
      description = ''
        Hostname or IP of the llama-server / llama-swap instance to
        scrape. Auto-derived from
        ``myconfig.ai.llama-cpp.serviceListenAddress`` (with
        ``0.0.0.0`` mapped to ``localhost``) when the service is
        present. Override to point at a container or remote instance.
      '';
    };

    scrapePort = mkOption {
      type = types.port;
      default = 33656;
      description = ''
        Port of the llama-server / llama-swap ``/v1/models`` endpoint.
        Auto-derived from ``myconfig.ai.llama-cpp.servicePort`` when
        the service is present.
      '';
    };

    exporterPort = mkOption {
      type = types.port;
      default = 9234;
      description = ''
        Port the Python exporter listens on (loopback only). This is
        the endpoint scraped by vmagent.
      '';
    };

    scrapeIntervalSeconds = mkOption {
      type = types.int;
      default = 15;
      description = ''
        How often the exporter polls the ``/v1/models`` endpoint
        (in seconds). Defaults to ``15`` to match the vmagent default
        scrape interval.
      '';
    };

    timingTextfileDir = mkOption {
      type = types.str;
      default = "/var/lib/prometheus-node-exporter-text-files";
      description = ''
        Directory into which the journal-tailing timing exporter writes
        ``llama-server-timing.prom``. Must match the
        ``--collector.textfile.directory`` passed to node_exporter
        (the same directory used by ``client.system-age.nix``).
      '';
    };
  };

  config = lib.mkMerge [
    # --- Auto-enable and derive port/host from the llama-cpp service ---
    (lib.mkIf hasLlamaService {
      myconfig.observability.client.llamaServer = {
        enable = lib.mkDefault true;
        scrapeHost = lib.mkDefault resolvedScrapeHost;
        scrapePort = lib.mkDefault resolvedScrapePort;
      };
    })

    # --- Exporter systemd unit + vmagent scrape job ---
    (lib.mkIf llamaCfg.enable {
      systemd.services.llama-server-model-exporter = {
        description = "Prometheus exporter for llama-server model state (/v1/models)";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        serviceConfig = {
          ExecStart = "${exporter}/bin/llama-server-model-exporter";
          Restart = "on-failure";
          RestartSec = "5s";

          # The exporter only needs a loopback listener + egress TCP to
          # the llama-server on localhost. No filesystem access.
          DynamicUser = true;
          NoNewPrivileges = true;
          ProtectSystem = "strict";
          ProtectHome = true;
          PrivateTmp = true;
          ProtectKernelTunables = true;
          ProtectKernelModules = true;
          ProtectControlGroups = true;
          RestrictAddressFamilies = [
            "AF_INET"
          ];
          RestrictNamespaces = true;
          LockPersonality = true;
          MemoryDenyWriteExecute = true;
          SystemCallArchitectures = "native";
        };
      };

      services.vmagent = {
        prometheusConfig = {
          scrape_configs = [
            {
              job_name = "llama-server";
              static_configs = [
                {
                  targets = [ "127.0.0.1:${toString llamaCfg.exporterPort}" ];
                }
              ];
            }
          ];
        };
      };

      # --- Journal-tailing timing exporter -----------------------------------
      # Tails the llama-cpp systemd unit journal, parses print_timing lines,
      # and writes a textfile .prom picked up by the node_exporter textfile
      # collector.  No extra network port required.
      systemd.tmpfiles.rules = [
        "d ${llamaCfg.timingTextfileDir} 0755 root root - -"
      ];

      systemd.services.llama-server-timing-exporter = {
        description = "Parse llama-cpp journal timing lines into Prometheus textfile metrics";
        wantedBy = [ "multi-user.target" ];
        # Start after the llama-cpp service so journalctl can resolve the unit.
        after = [
          "network.target"
          "llama-cpp.service"
        ];
        serviceConfig = {
          ExecStart = "${timingScript}/bin/llama-server-timing-exporter";
          Restart = "on-failure";
          RestartSec = "5s";

          # Needs read access to the journal and write access to the textfile dir.
          # Run as root so journalctl --system works without extra ACLs.
          # ProtectSystem=full still allows writes to /var/lib via
          # ReadWritePaths.
          User = "root";
          ProtectSystem = "full";
          ProtectHome = true;
          PrivateTmp = true;
          NoNewPrivileges = true;
          ReadWritePaths = [ llamaCfg.timingTextfileDir ];
        };
      };
    })
  ];
}
