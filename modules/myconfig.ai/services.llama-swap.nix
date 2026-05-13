# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  myconfig,
  options,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.llama-swap;

  hasGpuVariant =
    v:
    (
      let
        hasMyconfigProperty = lib.hasAttrByPath [ "myconfig" "hardware" "gpu" "variant" ] options;
        hasGpuVariant = builtins.elem v config.myconfig.hardware.gpu.variant;
      in
      if hasMyconfigProperty then hasGpuVariant else true
    );

  # Determine whether a given device string is supported by the current hardware
  guardDevice =
    device:
    if lib.hasPrefix "Vulkan" device then
      (hasGpuVariant "amd" || hasGpuVariant "amd-no-rocm")
    else if lib.hasPrefix "ROCm" device then
      (hasGpuVariant "amd")
    else if lib.hasPrefix "CUDA" device then
      (hasGpuVariant "nvidia")
    else
      false;

  # Select the correct llama-server binary for a device
  llamaServerFor =
    device:
    if lib.hasPrefix "Vulkan" device then
      lib.getExe' pkgs.llama-cpp-vulkan "llama-server"
    else if lib.hasPrefix "ROCm" device then
      lib.getExe' pkgs.llama-cpp-rocm "llama-server"
    else
      lib.getExe' pkgs.llama-cpp "llama-server";
  llamaBenchFor =
    device:
    if lib.hasPrefix "Vulkan" device then
      lib.getExe' pkgs.llama-cpp-vulkan "llama-bench"
    else if lib.hasPrefix "ROCm" device then
      lib.getExe' pkgs.llama-cpp-rocm "llama-bench"
    else
      lib.getExe' pkgs.llama-cpp "llama-bench";

  # Build environment variables for a device
  envForDevice =
    device:
    [ "LLAMA_ARG_DEVICE=${device}" ]
    ++ lib.optional (
      lib.hasPrefix "Vulkan" device || lib.hasPrefix "ROCm" device
    ) "CUDA_VISIBLE_DEVICES=";

  # Build a shell application that launches llama-server for a specific model+device combo.
  # Usage: <script> <port> [extra-args...]
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
      ctxSizeFlag = lib.optionalString (model.ctxSize != null) "--ctx-size ${toString model.ctxSize}";
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
          --no-webui ${ctxSizeFlag} ${aliasesFlag} ${paramsStr} "''${@:2}"
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
            -d 0,4096,8192,16384,32768 \
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

  applyVariant =
    variantName: variant: model:
    (builtins.removeAttrs model [ "variants" ])
    // {
      name = "${model.name}-${variantName}";
      inherit (variant) aliases;
      args =
        model.args
        ++ variant.args
        ++ (lib.optionals (variant.mmproj != null) [
          "--mmproj"
          variant.mmproj
        ]);
    }
    // lib.optionalAttrs (variant.ctxSize != null) { inherit (variant) ctxSize; };

  unpackContainedVariants =
    model:
    [ (builtins.removeAttrs model [ "variants" ]) ]
    ++ map (
      variantName:
      let
        variant = lib.getAttr variantName model.variants;
      in
      applyVariant variantName variant model
    ) (builtins.attrNames model.variants);

  unpackedModels = lib.concatMap unpackContainedVariants cfg.models;

  # Generate a single llama-swap model entry backed by a shell application
  mkModelEntry =
    {
      model,
      device,
      isFirstDevice ? false,
      unlisted ? false,
    }:
    let
      script = mkLlamaScript {
        inherit
          model
          device
          ;
      };
      modelKey =
        (if unlisted then "unlisted:" else "") + (if isFirstDevice then "" else "${device}:") + model.name;
    in
    {
      "${modelKey}" = {
        cmd = "${lib.getExe script} \${PORT}";
        ttl = model.ttl;
        inherit unlisted;
        aliases = lib.optionals (isFirstDevice && unlisted == false) model.aliases;
      };
    };

  # Generate model entries for a list of devices, optionally marking them as unlisted
  mkDeviceEntries =
    {
      model,
      devices,
      unlisted ? false,
    }:
    let
      firstDevice = if devices != [ ] then builtins.head devices else null;
    in
    map (
      device:
      let
        isFirstDevice = device == firstDevice;
      in
      mkModelEntry {
        inherit
          model
          device
          isFirstDevice
          unlisted
          ;
      }
    ) devices;

  # Generate all model entries for a single model input across all its devices
  mkModelEntries =
    model:
    mkDeviceEntries {
      inherit model;
      devices = builtins.filter guardDevice model.devices;
    }
    ++ mkDeviceEntries {
      inherit model;
      devices = builtins.filter guardDevice model.unlistedDevices;
      unlisted = true;
    };

  # Collect all script derivations for home-manager packages
  mkScriptDeviceEntries =
    {
      model,
      devices,
    }:
    lib.concatMap (
      device:
      lib.optionals (guardDevice device) ([
        (mkLlamaScript {
          inherit model device;
        })
        (mkLlamaBenchScript {
          inherit model device;
        })
      ])
    ) devices;

  mkScriptEntries =
    model:
    mkScriptDeviceEntries {
      inherit model;
      devices = model.devices;
    }
    ++ mkScriptDeviceEntries {
      inherit model;
      devices = model.unlistedDevices;
    };

  allScripts = lib.concatMap mkScriptEntries unpackedModels;

  # Collect the names of all generated llama-bench-* scripts
  benchScriptNames = lib.concatMap (
    s: lib.optional (lib.hasPrefix "llama-bench_" s.name) s.name
  ) allScripts;

  # All distinct devices that have at least one generated llama-bench_<device>_* script
  benchDevices = lib.unique (
    lib.concatMap (
      n:
      let
        # Strip the "llama-bench_" prefix, then take everything up to the next "_"
        rest = lib.removePrefix "llama-bench_" n;
        parts = lib.splitString "_" rest;
      in
      lib.optional (parts != [ ]) (builtins.head parts)
    ) benchScriptNames
  );

  # Build an aggregate "run every matching llama-bench script" wrapper. The
  # `scripts` arg is the list of script names to include; `name` is the
  # resulting binary's name.
  mkLlamaBenchAggregate =
    {
      name,
      scripts,
    }:
    pkgs.writeShellApplication {
      inherit name;
      runtimeInputs = allScripts;
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

  # The catch-all aggregate that runs every generated bench script
  llamaBenchAll = mkLlamaBenchAggregate {
    name = "llama-bench-all";
    scripts = benchScriptNames;
  };

  # Per-device aggregates: llama-bench_CUDA0, llama-bench_Vulkan0, ...
  llamaBenchPerDevice = map (
    device:
    mkLlamaBenchAggregate {
      name = "llama-bench_${device}";
      scripts = builtins.filter (n: lib.hasPrefix "llama-bench_${device}_" n) benchScriptNames;
    }
  ) benchDevices;

  # Generate all models from the input list
  allModels = lib.mkMerge (lib.concatMap mkModelEntries unpackedModels);

  # Extract the numeric suffix from a device string (e.g. "Vulkan0" -> "0", "ROCm1" -> "1").
  # Devices with the same index share the same physical GPU, so they must be in the same group.
  deviceIndex =
    device:
    let
      m = builtins.match ".*([0-9]+)$" device;
    in
    if m != null then builtins.head m else device;

  # Build groups: one group per physical GPU (by device index) so models on different GPUs
  # can run simultaneously. Within the same GPU, models swap (they share VRAM).
  allGroups =
    let
      mkGpuPairs =
        { model, devices }:
        lib.concatMap (
          device:
          lib.optionals (guardDevice device) ([
            {
              gpu = deviceIndex device;
              key = "${device}:${model.name}";
            }
          ])
        ) devices;
      # Collect (gpuIndex, modelKey) pairs for all eligible model entries
      gpuModelPairs = lib.concatMap (
        model:
        mkGpuPairs {
          inherit model;
          devices = model.devices;
        }
        ++ mkGpuPairs {
          inherit model;
          devices = model.unlistedDevices;
        }
      ) unpackedModels;

      # Group model keys by GPU index
      gpuIndices = lib.unique (map (p: p.gpu) gpuModelPairs);
      membersFor = idx: map (p: p.key) (builtins.filter (p: p.gpu == idx) gpuModelPairs);
    in
    lib.listToAttrs (
      map (idx: {
        name = "gpu${idx}";
        value = {
          swap = true;
          exclusive = false;
          members = membersFor idx;
        };
      }) gpuIndices
    );

  mkModelNames =
    { model, devices }:
    lib.concatMap (device: lib.optionals (guardDevice device) ([ "${device}:${model.name}" ])) devices;

  # Collect all model names/keys exposed by this llama-swap instance for localModels registration
  allModelNames = lib.concatMap (
    model:
    mkModelNames {
      inherit model;
      devices = model.devices;
    }
    ++ mkModelNames {
      inherit model;
      devices = model.unlistedDevices;
    }
    ++ model.aliases
  ) unpackedModels;
in
{
  options.myconfig.ai.llama-swap = with lib; {
    models = mkOption {
      type = types.listOf (
        types.submodule {
          options = {
            name = mkOption {
              type = types.str;
              description = "Model name used as identifier in the llama-swap model key";
            };
            path = mkOption {
              type = types.str;
              description = "Path to the GGUF model file";
            };
            devices = mkOption {
              type = types.listOf types.str;
              default = [ "Vulkan0" ];
              description = "List of devices to run this model on (e.g. 'Vulkan0', 'CUDA0', 'ROCm0')";
            };
            unlistedDevices = mkOption {
              type = types.listOf types.str;
              default = [ ];
              description = "Devices that generate llama-swap entries with unlisted = true (accessible only via direct script)";
            };
            params = mkOption {
              type = types.listOf types.str;
              default = [ ];
              description = "Additional llama-server parameters";
            };
            aliases = mkOption {
              type = types.listOf types.str;
              default = [ ];
              description = "Aliases for this model in llama-swap";
            };
            ttl = mkOption {
              type = types.int;
              default = 300;
              description = "Time-to-live in seconds before the model is unloaded";
            };
            ctxSize = mkOption {
              type = types.nullOr types.int;
              default = null;
              description = "Context size (--ctx-size) for llama-server; null to use the model default";
            };
            variants = mkOption {
              type = types.attrsOf (
                types.submodule {
                  options = {
                    aliases = mkOption {
                      type = types.listOf types.str;
                      default = [ ];
                      description = "Aliases for this model in llama-swap";
                    };
                    ctxSize = mkOption {
                      type = types.nullOr types.int;
                      default = null;
                      description = "Context size (--ctx-size) for llama-server; null to use the model default";
                    };
                    params = mkOption {
                      type = types.listOf types.str;
                      default = [ ];
                      description = "Additional llama-server parameters appended to the parent model params";
                    };
                    mmproj = mkOption {
                      type = types.nullOr types.str;
                      default = null;
                      description = "Path to mmproj file; when set, a :mmproj variant is auto-generated";
                    };
                  };
                }
              );
              default = { };
              description = "Named variants of this model. Each variant generates a ${model.name}-${variant_name} entry with its params merged on top of the parent params";
            };
          };
        }
      );
      default = [ ];
      description = "Declarative model definitions that are expanded into llama-swap model entries per device";
    };
  };
  imports = [
    (
      {
        config,
        options,
        lib,
        ...
      }:

      let
        hmEnabled = lib.hasAttrByPath [ "home-manager" "sharedModules" ] options;
      in
      {
        config = lib.optionalAttrs hmEnabled {
          home-manager.sharedModules = lib.mkIf config.services.llama-swap.enable [
            {
              home.packages = allScripts ++ [ llamaBenchAll ] ++ llamaBenchPerDevice;
              myconfig.persistence.cache-directories = [
                "benchmarks/llama-bench"
              ];
            }
          ];
        };
      }
    )
  ];
  config = lib.mkIf config.services.llama-swap.enable {
    myconfig.ai.localModels = [
      (
        let
          port = config.services.llama-swap.port;
        in
        {
          name = "llama-swap-${toString port}";
          models = allModelNames;
          port = port;
        }
      )
    ];

    services.llama-swap = {
      settings = {
        sendLoadingState = true;
        models = allModels;
        groups = allGroups;
      };
    };

    systemd.services.llama-swap = {
      # https://github.com/nixos/nixpkgs/issues/441531
      environment.XDG_CACHE_HOME = "/var/cache/llama-swap";
      serviceConfig.CacheDirectory = "llama-swap";
    };
  };
}
