# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Generators for the per-(model, device) `llama-server_*` and
# `llama-bench_*` shell wrappers, plus the aggregate `llama-bench-all`
# and `llama-bench_<device>` runners.
#
# These are pure functions: they take a `devices` lib (see ./devices.nix)
# plus lib/pkgs and return derivations. They have no knowledge of
# llama-swap or NixOS modules.
{
  lib,
  pkgs,
  devices,
}:
let
  inherit (devices) llamaServerFor llamaBenchFor envForDevice;

  # Build a shell application that launches llama-server for a specific
  # (model, device) combo. Usage: `<script> <port> [extra-args...]`.
  mkLlamaScript =
    {
      model,
      device,
    }:
    let
      server = llamaServerFor device;
      safeName = lib.replaceStrings [ ":" ] [ "-" ] "${model.name}";
      scriptName = "llama-server_${device}_${safeName}";
      envExports = lib.concatStringsSep "\n" (map (e: "export ${e}") (envForDevice device));
      ctxSizeFlag =
        lib.optionalString (model.ctxSize != null)
          "--ctx-size ${toString (model.ctxSize * model.parallel)}";
      cacheTypeFlag =
        lib.optionalString (model.cacheType != null)
          "--cache-type-k ${model.cacheType} --cache-type-v ${model.cacheType} --spec-draft-type-k ${model.cacheType} --spec-draft-type-v ${model.cacheType}";
      parallelFlag = lib.optionalString (
        model.parallel > 1
      ) "--parallel ${toString model.parallel} --cont-batching";
      aliasesFlag = lib.optionalString (
        model.aliases != [ ]
      ) "--alias ${lib.concatStringsSep "," model.aliases}";
      paramsStr = lib.concatStringsSep " " (map lib.escapeShellArg model.params);
    in
    pkgs.writeShellApplication {
      name = scriptName;
      runtimeInputs = [ ];
      text = ''
        ${envExports}
        exec ${server} \
          --port "''${1:-22545}" \
          -m ${lib.escapeShellArg model.path} \
          --gpu-layers all \
          --flash-attn on \
          --mlock \
          --metrics \
          --no-webui ${ctxSizeFlag} ${cacheTypeFlag} ${parallelFlag} ${aliasesFlag} ${paramsStr} "''${@:2}"
      '';
    };

  mkLlamaBenchScript =
    {
      model,
      device,
    }:
    let
      bench = llamaBenchFor device;
      safeName = lib.replaceStrings [ ":" ] [ "-" ] "${model.name}";
      scriptName = "llama-bench_${device}_${safeName}";
      # Exported for capture_metadata's llama-server invocation. llama-bench
      # itself ignores LLAMA_ARG_DEVICE and uses the explicit -dev CLI flag.
      envExports = lib.concatStringsSep "\n" (map (e: "export ${e}") (envForDevice device));
      # Matching llama-server script — used to capture
      # runtime metadata via /props before benchmarking.
      serverScript = mkLlamaScript { inherit model device; };
    in
    pkgs.writeShellApplication {
      name = scriptName;
      runtimeInputs = [
        pkgs.curl
        pkgs.jq
      ];
      text = ''
        ${envExports}
        dir="$HOME/benchmarks/llama-bench/$(date +%Y-%m-%d)"
        mkdir -p "$dir"
        capture_metadata_port=22799

        bench() (
          set -x
          ${bench} \
            -m "${model.path}" \
            -dev "${device}" \
            -ngl 999 \
            -fa 1 \
            -d 0,8192,16384 `# ,4096,32768` \
            -p 2048 \
            -n 128 \
            -ub 2048 \
            -o csv -oe md
        )

        capture_metadata() {
          local pp2048_at_8k="$1"
          local tg128_at_8k="$2"
          local tg128_at_16k="$3"

          # --- Capture model metadata by briefly starting llama-server and querying /props ---
          local props_json="$dir/${scriptName}.props.json"
          local server_log="$dir/${scriptName}.server.log"
          local server_pid=""

          cleanup_server() {
            if [[ -n "$server_pid" ]] && kill -0 "$server_pid" 2>/dev/null; then
              kill "$server_pid" 2>/dev/null || true
              wait "$server_pid" 2>/dev/null || true
            fi
          }
          trap cleanup_server RETURN

          echo "[metadata] sleeping 2s before starting llama-server" >&2
          sleep 2
          echo "[metadata] starting llama-server on port $capture_metadata_port to capture /props" >&2
          ${lib.getExe serverScript} "$capture_metadata_port" --no-warmup >"$server_log" 2>&1 &
          server_pid=$!

          # Wait until /props responds (or the server dies / we time out)
          local waited=0
          while (( waited < 120 )); do
            if ! kill -0 "$server_pid" 2>/dev/null; then
              echo "[metadata] llama-server exited before becoming ready; see $server_log" >&2
              return 1
            fi
            if curl -fsS "http://127.0.0.1:$capture_metadata_port/props" -o "$props_json" 2>/dev/null; then
              break
            fi
            sleep 1
            waited=$((waited + 1))
          done
          if [[ ! -s "$props_json" ]]; then
            echo "[metadata] failed to fetch /props within timeout" >&2
            return 1
          fi

          # Pull the most interesting fields out of /props. The exact shape of
          # /props varies between llama.cpp versions; use jq with // empty so
          # missing fields just yield blanks.
          local n_ctx model_path
          n_ctx=$(jq -r '
            (.default_generation_settings.n_ctx
             // .default_generation_settings.params.n_ctx
             // .n_ctx
             // empty)' "$props_json")
          model_path=$(jq -r '(.model_path // .default_generation_settings.model // empty)' "$props_json")

          # <device>.metadata.csv: one row per script, deduped by script name. Written
          # with jq -r @csv so embedded commas/quotes are escaped properly.
          local meta="$dir/${device}.metadata.csv"
          local header="timestamp,script,device,model,prompt ingestion speed,normal chat streaming speed,long-context streaming speed,n_ctx"
          if [[ ! -f "$meta" ]]; then
            printf '%s\n' "$header" > "$meta"
          fi
          # Drop any existing row for this script so we always reflect the
          # latest captured values. The script name is the 2nd CSV column.
          if grep -q ",\"${scriptName}\"," "$meta" 2>/dev/null; then
            grep -v ",\"${scriptName}\"," "$meta" > "$meta.tmp" && mv "$meta.tmp" "$meta"
          fi
          local timestamp
          timestamp=$(date -u +%Y-%m-%dT%H:%M:%SZ)
          jq -rn \
            --arg ts       "$timestamp" \
            --arg script   "${scriptName}" \
            --arg device   "${device}" \
            --arg model    "${model.name}" \
            --arg pp2048   "$pp2048_at_8k" \
            --arg tg128_8k  "$tg128_at_8k" \
            --arg tg128_16k "$tg128_at_16k" \
            --arg n_ctx    "$n_ctx" \
            '[$ts,$script,$device,$model,$pp2048,$tg128_8k,$tg128_16k,$n_ctx] | @csv' \
            >> "$meta"

          # Print a structured human-readable summary of the captured metadata
          # to stderr so it shows up in the terminal next to the bench output.
          {
            printf '%s\n' "[metadata] ---- captured metadata ----"
            printf '[metadata]   %-40s %s\n' \
              "timestamp"                              "$timestamp" \
              "script"                                 "${scriptName}" \
              "device"                                 "${device}" \
              "model"                                  "${model.name}" \
              "model_path"                             "$model_path" \
              "n_ctx"                                  "$n_ctx" \
              "prompt ingestion speed(pp2048@8k)"      "$pp2048_at_8k" \
              "normal chat streaming speed(tg128@8k)"   "$tg128_at_8k" \
              "long-context streaming speed(tg128@16k)" "$tg128_at_16k"
            printf '%s\n' "[metadata] ---------------------------"
            printf '%s\n' "[metadata] wrote row for ${scriptName} to $meta"
          } >&2
        }

        ###########################################################################################
        ##  run  ##################################################################################
        ###########################################################################################

        echo "[bench] sleeping 2s before running llama-bench" >&2
        sleep 2
        # Results from all models on the same device are aggregated into a
        # single per-device CSV. When the file already exists, strip the CSV
        # header line emitted by llama-bench so we don't get repeated header
        # rows. Stderr is tee'd to both the per-script log and the terminal.
        csv="$dir/${device}.csv"
        # shellcheck disable=SC2094
        if [[ -f "$csv" ]]; then
          bench 2> >(tee -a "$dir/${scriptName}.log" >&2) | tee "$dir/${scriptName}.csv" | tail -n +2 >> "$csv"
        else
          bench 2> >(tee -a "$dir/${scriptName}.log" >&2) | tee "$dir/${scriptName}.csv" >> "$csv"
        fi

        get_llama_bench_metric() {
          local csv="''${1:?usage: get_llama_bench_metric CSV N_PROMPT N_GEN N_DEPTH}"
          local want_n_prompt="''${2:?usage: get_llama_bench_metric CSV N_PROMPT N_GEN N_DEPTH}"
          local want_n_gen="''${3:?usage: get_llama_bench_metric CSV N_PROMPT N_GEN N_DEPTH}"
          local want_n_depth="''${4:?usage: get_llama_bench_metric CSV N_PROMPT N_GEN N_DEPTH}"

          awk -F, \
            -v want_n_prompt="$want_n_prompt" \
            -v want_n_gen="$want_n_gen" \
            -v want_n_depth="$want_n_depth" '
        function trimq(s) {
          gsub(/\r$/, "", s)
          gsub(/^"|"$/, "", s)
          return s
        }

        # Skip header and non-data lines.
        $1 !~ /^"/ {
          next
        }

        {
          # Do not use header-derived column indexes with awk -F, here.
          # This file contains quoted fields with commas, especially gpu_info:
          #
          #   "NVIDIA GeForce RTX 5090, Radeon 8060S Graphics ..."
          #
          # awk -F, is therefore not a real CSV parser and shifts all earlier columns.
          # However, the fields we need are at the end and contain no commas:
          #
          #   n_prompt,n_gen,n_depth,test_time,avg_ns,stddev_ns,avg_ts,stddev_ts
          #
          n_prompt = trimq($(NF - 7)) + 0
          n_gen    = trimq($(NF - 6)) + 0
          n_depth  = trimq($(NF - 5)) + 0
          avg_ts   = trimq($(NF - 1))

          if (n_prompt == want_n_prompt && n_gen == want_n_gen && n_depth == want_n_depth) {
            print avg_ts
            found = 1
            exit 0
          }
        }

        END {
          if (!found) {
            printf "metric not found: n_prompt=%s n_gen=%s n_depth=%s in %s\n", want_n_prompt, want_n_gen, want_n_depth, FILENAME > "/dev/stderr"
            exit 3
          }
        }
        ' "$csv"
        }

        get_pp2048_at_8k() {
          get_llama_bench_metric "$1" 2048 0 8192
        }

        get_tg128_at_8k() {
          get_llama_bench_metric "$1" 0 128 8192
        }

        get_tg128_at_16k() {
          get_llama_bench_metric "$1" 0 128 16384
        }

        pp2048_at_8k="$(get_pp2048_at_8k "$dir/${scriptName}.csv")"
        tg128_at_8k="$(get_tg128_at_8k "$dir/${scriptName}.csv")"
        tg128_at_16k="$(get_tg128_at_16k "$dir/${scriptName}.csv")"

        # Try to capture metadata, but never block the benchmark on failures.
        capture_metadata "$pp2048_at_8k" "$tg128_at_8k" "$tg128_at_16k" || echo "[metadata] capture failed; continuing with benchmark" >&2

        times
      '';
    };

  # Build an aggregate "run every matching llama-bench script" wrapper.
  # `scripts` is the list of script *names* to invoke; `runtimeInputs` is
  # the list of script derivations that must be on PATH.
  mkLlamaBenchAggregate =
    {
      name,
      scripts,
      runtimeInputs,
    }:
    pkgs.writeShellApplication {
      inherit name runtimeInputs;
      text = ''
        dir="$HOME/benchmarks/llama-bench/$(date +%Y-%m-%d)"
        mkdir -p "$dir"
        log="$dir/${name}-$(date -u +%Y-%m-%dT%H-%M-%SZ).log"
        echo "Logging combined output to $log"

        # Tee all subsequent output (stdout+stderr) to the log file while still
        # showing it on the terminal.
        exec > >(tee -a "$log") 2>&1

        scripts=(${lib.concatStringsSep " " scripts})
        echo "Running ''${#scripts[@]} llama-bench script(s)..."
        failed=()
        for s in "''${scripts[@]}"; do
          echo "=== Running $s ==="
          if ! "$s" "$@"; then
            echo "!!! $s failed" >&2
            failed+=("$s")
          fi
        done
        if (( ''${#failed[@]} > 0 )); then
          echo "Failed scripts: ''${failed[*]}" >&2
          exit 1
        fi
        echo "All llama-bench scripts completed successfully."
      '';
    };
in
{
  inherit mkLlamaScript mkLlamaBenchScript mkLlamaBenchAggregate;
}
