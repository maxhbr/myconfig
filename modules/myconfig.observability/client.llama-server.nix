# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Prometheus exporter for llama-server / llama-swap model state.
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

  # Python script that polls the /v1/models endpoint and serves
  # Prometheus metrics on a loopback HTTP endpoint.
  exporter = pkgs.writeShellApplication {
    name = "llama-server-model-exporter";
    runtimeInputs = [ pythonEnv ];
    text = ''
      ${pythonEnv}/bin/python3 -u <<'PYTHON'
      import json
      import sys
      import time

      import requests
      from prometheus_client import start_http_server, Gauge

      HOST = "${llamaCfg.scrapeHost}"
      PORT = ${toString llamaCfg.scrapePort}
      EXPORTER_PORT = ${toString llamaCfg.exporterPort}
      SCRAPE_INTERVAL = ${toString llamaCfg.scrapeIntervalSeconds}

      # --- Prometheus gauges -------------------------------------------------

      # Model presence: always 1 while the model is known to the API
      g_model_info = Gauge(
          "llama_server_model_info",
          "Constant-1 info series for every model registered in llama-server",
          ["model", "owned_by"],
      )

      # 1 = loaded, 0 = unloaded
      g_model_status = Gauge(
          "llama_server_model_status",
          "Current load state of the model (1=loaded, 0=unloaded)",
          ["model"],
      )

      # Alias — 1 if this alias is registered for the model
      g_model_alias = Gauge(
          "llama_server_model_alias",
          "Constant-1 series for each alias registered on a model",
          ["model", "alias"],
      )

      # Architecture modalities
      g_model_input_modality = Gauge(
          "llama_server_model_input_modality",
          "1 if the model accepts this input modality",
          ["model", "modality"],
      )
      g_model_output_modality = Gauge(
          "llama_server_model_output_modality",
          "1 if the model produces this output modality",
          ["model", "modality"],
      )

      # Numeric metadata from the `meta` block (only populated for loaded models)
      g_model_ctx_size = Gauge(
          "llama_server_model_ctx_size",
          "Context size (n_ctx from meta, 0 if unavailable)",
          ["model"],
      )
      g_model_n_params = Gauge(
          "llama_server_model_n_params",
          "Parameter count (n_params from meta, 0 if unavailable)",
          ["model"],
      )
      g_model_n_embd = Gauge(
          "llama_server_model_n_embd",
          "Embedding dimension (n_embd from meta, 0 if unavailable)",
          ["model"],
      )
      g_model_n_vocab = Gauge(
          "llama_server_model_n_vocab",
          "Vocabulary size (n_vocab from meta, 0 if unavailable)",
          ["model"],
      )

      # GPU layers — parsed from the --n-gpu-layers arg in the model status
      g_model_n_gpu_layers = Gauge(
          "llama_server_model_n_gpu_layers",
          "Number of layers offloaded to GPU (from --n-gpu-layers arg, 0 if unavailable)",
          ["model"],
      )

      # Aggregate counts per load state
      g_model_count = Gauge(
          "llama_server_model_count",
          "Number of models in each load state",
          ["value"],
      )

      # Scrape health
      g_scrape_success = Gauge(
          "llama_server_scrape_success",
          "1 if the most recent /v1/models fetch succeeded, 0 otherwise",
      )
      g_scrape_timestamp = Gauge(
          "llama_server_model_scrape_timestamp_seconds",
          "Unix timestamp of the most recent /v1/models fetch",
      )

      def parse_gpu_layers(args):
          """Extract --n-gpu-layers value from the model args list."""
          try:
              idx = args.index("--n-gpu-layers")
              val = args[idx + 1]
              if val == "all":
                  return -1  # special sentinel meaning all layers on GPU
              return int(val)
          except (ValueError, IndexError, TypeError):
              return 0

      def scrape():
          url = f"http://{HOST}:{PORT}/v1/models"
          try:
              resp = requests.get(url, timeout=10)
              resp.raise_for_status()
              data = resp.json()
          except Exception as exc:
              print(f"scrape error: {exc}", file=sys.stderr)
              g_scrape_success.set(0)
              g_scrape_timestamp.set(time.time())
              # Clear model counts on failure so dashboards reflect the outage
              g_model_count.set(0, labels={"value": "loaded"})
              g_model_count.set(0, labels={"value": "unloaded"})
              return

          g_scrape_success.set(1)
          g_scrape_timestamp.set(time.time())

          models_by_id = {m["id"]: m for m in data.get("data", [])}

          # --- Update per-model gauges ---
          loaded_count = 0
          unloaded_count = 0

          for model_id, model in models_by_id.items():
              owned_by = model.get("owned_by", "")
              g_model_info.labels(model=model_id, owned_by=owned_by).set(1)

              status = model.get("status", {}).get("value", "unknown")
              is_loaded = status == "loaded"
              if is_loaded:
                  loaded_count += 1
              else:
                  unloaded_count += 1
              g_model_status.labels(model=model_id).set(1 if is_loaded else 0)

              # Aliases
              for alias in model.get("aliases", []):
                  g_model_alias.labels(model=model_id, alias=alias).set(1)

              # Modalities
              arch = model.get("architecture", {})
              for mod in arch.get("input_modalities", ["text"]):
                  g_model_input_modality.labels(model=model_id, modality=mod).set(1)
              for mod in arch.get("output_modalities", ["text"]):
                  g_model_output_modality.labels(model=model_id, modality=mod).set(1)

              # Meta block (only populated for loaded models)
              meta = model.get("meta", {})
              g_model_ctx_size.labels(model=model_id).set(meta.get("n_ctx") or 0)
              g_model_n_params.labels(model=model_id).set(meta.get("n_params") or 0)
              g_model_n_embd.labels(model=model_id).set(meta.get("n_embd") or 0)
              g_model_n_vocab.labels(model=model_id).set(meta.get("n_vocab") or 0)

              # GPU layers from args
              args = model.get("status", {}).get("args", [])
              gpu_layers = parse_gpu_layers(args)
              g_model_n_gpu_layers.labels(model=model_id).set(gpu_layers)

          # --- Clean up gauges for models that disappeared ---
          known_ids = set(models_by_id.keys())

          for gauge in [g_model_info, g_model_status, g_model_alias,
                        g_model_input_modality, g_model_output_modality,
                        g_model_ctx_size, g_model_n_params, g_model_n_embd,
                        g_model_n_vocab, g_model_n_gpu_layers]:
              to_remove = []
              for label_key in gauge._metrics:
                  model_key = label_key[0] if label_key else ""
                  if model_key not in known_ids:
                      to_remove.append(label_key)
              for label_key in to_remove:
                  del gauge._metrics[label_key]

          # --- Aggregate counts ---
          g_model_count.set(loaded_count, labels={"value": "loaded"})
          g_model_count.set(unloaded_count, labels={"value": "unloaded"})

      def main():
          start_http_server(EXPORTER_PORT, addr="127.0.0.1")
          print(f"llama-server-model-exporter listening on 127.0.0.1:{EXPORTER_PORT}")
          scrape()  # initial scrape on startup
          while True:
              time.sleep(SCRAPE_INTERVAL)
              scrape()

      if __name__ == "__main__":
          main()
      PYTHON
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
    })
  ];
}
