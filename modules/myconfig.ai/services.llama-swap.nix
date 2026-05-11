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
      suffix ? "",
      extraArgs ? "",
    }:
    let
      server = llamaServerFor device;
      safeName = lib.replaceStrings [ ":" ] [ "-" ] "${model.name}${suffix}";
      scriptName = "llama-server_${device}_${safeName}";
      envExports = lib.concatStringsSep "\n" (map (e: "export ${e}") (envForDevice device));
      ctxSizeFlag = lib.optionalString (model.ctxSize != null) "--ctx-size ${toString model.ctxSize} ";
    in
    pkgs.writeShellApplication {
      name = scriptName;
      runtimeInputs = [ ];
      text = ''
        ${envExports}
        exec ${server} --port "''${1:-22545}" -m "${model.path}" --gpu-layers 999 -fa on --no-webui ${ctxSizeFlag}${model.params} ${extraArgs} "''${@:2}"
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
      # Matching llama-server script (no suffix, no extraArgs) — used to capture
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
        dir="$HOME/benchmarks/llama-bench-logs"
        mkdir -p "$dir"

        # --- Capture model metadata by briefly starting llama-server and querying /props ---
        capture_metadata() {
          local port="$1"
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
          echo "[metadata] starting llama-server on port $port to capture /props" >&2
          ${lib.getExe serverScript} "$port" --no-warmup >"$server_log" 2>&1 &
          server_pid=$!

          # Wait until /props responds (or the server dies / we time out)
          local waited=0
          while (( waited < 120 )); do
            if ! kill -0 "$server_pid" 2>/dev/null; then
              echo "[metadata] llama-server exited before becoming ready; see $server_log" >&2
              return 1
            fi
            if curl -fsS "http://127.0.0.1:$port/props" -o "$props_json" 2>/dev/null; then
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
          local n_ctx n_ctx_per_seq model_path model_size n_params arch
          n_ctx=$(jq -r '
            (.default_generation_settings.n_ctx
             // .default_generation_settings.params.n_ctx
             // .n_ctx
             // empty)' "$props_json")
          n_ctx_per_seq=$(jq -r '
            (.default_generation_settings.n_ctx_per_seq
             // .n_ctx_per_seq
             // empty)' "$props_json")
          model_path=$(jq -r '(.model_path // .default_generation_settings.model // empty)' "$props_json")
          model_size=$(jq -r '(.model_size // empty)' "$props_json")
          n_params=$(jq -r '(.model_n_params // .n_params // empty)' "$props_json")
          arch=$(jq -r '(.model_arch // empty)' "$props_json")

          # metadata.csv: one row per script, deduped by script name. Written
          # with jq -r @csv so embedded commas/quotes are escaped properly.
          local meta="$dir/metadata.csv"
          local header="timestamp,script,device,model,model_path,n_ctx,n_ctx_per_seq,n_params,model_size,arch"
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
            --arg path     "$model_path" \
            --arg n_ctx    "$n_ctx" \
            --arg n_ctx_ps "$n_ctx_per_seq" \
            --arg n_params "$n_params" \
            --arg size     "$model_size" \
            --arg arch     "$arch" \
            '[$ts,$script,$device,$model,$path,$n_ctx,$n_ctx_ps,$n_params,$size,$arch] | @csv' \
            >> "$meta"

          # Print a structured human-readable summary of the captured metadata
          # to stderr so it shows up in the terminal next to the bench output.
          {
            printf '%s\n' "[metadata] ---- captured metadata ----"
            printf '[metadata]   %-14s %s\n' \
              "timestamp"     "$timestamp" \
              "script"        "${scriptName}" \
              "device"        "${device}" \
              "model"         "${model.name}" \
              "model_path"    "$model_path" \
              "n_ctx"         "$n_ctx" \
              "n_ctx_per_seq" "$n_ctx_per_seq" \
              "n_params"      "$n_params" \
              "model_size"    "$model_size" \
              "arch"          "$arch"
            printf '%s\n' "[metadata] ---------------------------"
            printf '%s\n' "[metadata] wrote row for ${scriptName} to $meta"
          } >&2
        }

        # Try to capture metadata, but never block the benchmark on failures.
        capture_metadata 22799 || echo "[metadata] capture failed; continuing with benchmark" >&2

        bench() (
          set -x
          ${bench} -m "${model.path}" -dev "${device}" -d 0,4096,8192,16384,32768 -p 2048 -n 32 -ub 2048 -mmp 0 -o csv -oe md
        )
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
      '';
    };

  # Generate a single llama-swap model entry backed by a shell application
  mkModelEntry =
    {
      model,
      device,
      isFirstDevice ? false,
      suffix ? "",
      extraArgs ? "",
      unlisted ? false,
    }:
    let
      script = mkLlamaScript {
        inherit
          model
          device
          suffix
          extraArgs
          ;
      };
      modelKey =
        (if unlisted then "unlisted:" else "")
        + (if isFirstDevice then "" else "${device}:")
        + "${model.name}${suffix}";
    in
    {
      "${modelKey}" = {
        cmd = "${lib.getExe script} \${PORT}";
        ttl = model.ttl;
      }
      // lib.optionalAttrs unlisted { unlisted = true; }
      // lib.optionalAttrs (model.aliases != [ ] && suffix == "" && isFirstDevice && unlisted == false) {
        inherit (model) aliases;
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
      eligibleDevices = builtins.filter guardDevice devices;
      firstDevice = if eligibleDevices != [ ] then builtins.head eligibleDevices else null;
    in
    lib.concatMap (
      device:
      lib.optionals (guardDevice device) (
        let
          isFirstDevice = device == firstDevice;
        in
        [
          (mkModelEntry {
            inherit
              model
              device
              isFirstDevice
              unlisted
              ;
          })
        ]
        ++ lib.optionals (model.mmproj != null) [
          (mkModelEntry {
            inherit model device unlisted;
            suffix = ":mmproj";
            extraArgs = ''--mmproj "${model.mmproj}"'';
          })
        ]
      )
    ) devices;

  # Generate all model entries for a single model input across all its devices
  mkModelEntries =
    model:
    mkDeviceEntries {
      inherit model;
      devices = model.devices;
    }
    ++ mkDeviceEntries {
      inherit model;
      devices = model.unlistedDevices;
      unlisted = true;
    };

  # Collect all script derivations for home-manager packages
  mkScriptDeviceEntries =
    { model, devices }:
    lib.concatMap (
      device:
      lib.optionals (guardDevice device) (
        [
          (mkLlamaScript { inherit model device; })
          (mkLlamaBenchScript { inherit model device; })
        ]
        ++ lib.optionals (model.mmproj != null) [
          (mkLlamaScript {
            inherit model device;
            suffix = ":mmproj";
            extraArgs = ''--mmproj "${model.mmproj}"'';
          })
        ]
      )
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

  allScripts = lib.concatMap mkScriptEntries cfg.models;

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
        dir="$HOME/benchmarks/llama-bench-logs"
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
  allModels = lib.mkMerge (lib.concatMap mkModelEntries cfg.models);

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
          lib.optionals (guardDevice device) (
            [
              {
                gpu = deviceIndex device;
                key = "${device}:${model.name}";
              }
            ]
            ++ lib.optional (model.mmproj != null) {
              gpu = deviceIndex device;
              key = "${device}:${model.name}:mmproj";
            }
          )
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
      ) cfg.models;

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
    lib.concatMap (
      device:
      lib.optionals (guardDevice device) (
        [ "${device}:${model.name}" ]
        ++ lib.optional (model.mmproj != null) "${device}:${model.name}:mmproj"
      )
    ) devices;

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
  ) cfg.models;
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
            mmproj = mkOption {
              type = types.nullOr types.str;
              default = null;
              description = "Path to mmproj file; when set, a :mmproj variant is auto-generated";
            };
            params = mkOption {
              type = types.str;
              default = "";
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
                "benchmarks/llama-bench-logs"
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
