# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.microvm — UNATTENDED BATCH JOBS: the versioned job format, the
# host-side job directories and the guest-side `agent-job` runner
# (improvement ticket 4, part A).
#
# Layout (per slot, created by the host launcher's `submit`; the directories
# themselves are pre-created by tmpfiles because virtiofsd needs its share
# source to exist before the VM starts):
#
#   ${runtimeRoot}/jobs/<slot>/            root:root 0755  guest: read-only *
#   ${runtimeRoot}/jobs/<slot>/spec.json   root:root 0444  the job spec (v1)
#   ${runtimeRoot}/jobs/<slot>/prompt.md   root:root 0444  the prompt TEXT
#   ${runtimeRoot}/jobs/<slot>/out/        1000:1000 0755  guest-writable
#   ${runtimeRoot}/jobs/<slot>/out/result.json             the guest's result
#
#   * The share itself is read-WRITE (the guest must write `out/result.json`),
#     but the spec and the prompt are root-owned 0444 inside a root-owned 0755
#     directory. virtiofsd passes ownership through unchanged, so the guest
#     `agent` (uid 1000) can READ them and cannot modify, replace or unlink
#     them — only `out/` is writable. The guest therefore cannot rewrite its
#     own job (e.g. to lift the timeout or change the agent).
#
# Design rules taken from the ticket:
#   * prompts and specs NEVER go through process arguments (the host writes
#     files; the guest reads them) and NEVER into the Nix store (they are
#     runtime-only files under ${runtimeRoot});
#   * the spec is VERSIONED and validated on BOTH sides;
#   * the spec cannot name an executable — the agent is resolved through the
#     authoritative registry (./agents.nix), so a job can only ever run an
#     agent this module declares;
#   * `result.json` is written with tmp-file + rename, so the host never reads
#     a partially written result;
#   * the timeout is enforced TWICE in the guest (per-job `timeout(1)` plus the
#     unit's static `RuntimeMaxSec` ceiling) and once more on the host.
#
# NOTE (deliberately no guest-side power-off): microvm.nix's `microvm@<slot>`
# unit runs with `Restart = "always"`, so a guest that powers itself off after
# the job would be rebooted immediately, re-running or looping. Shutting the VM
# down is therefore the HOST's job (`submit` stops the unit once it has
# collected the result).
{
  config,
  lib,
  pkgs,
  agentRegistry,
  # The effective resource-class table (see default.nix).
  agentResourceClasses,
  ...
}:
let
  cfg = config.myconfig.ai.microvm;
  jobCfg = cfg.job;

  # The slot pool of the effective resource classes (ticket 5 A). The class
  # table comes from default.nix (`_module.args.agentResourceClasses`), which
  # also performs the legacy `slotCount` migration, so every module builds the
  # SAME pool.
  slots = (import ./slots.nix { inherit lib; }).mkSlots agentResourceClasses;

  # --- the ONE definition of every job path (host + guest side) ----------
  paths = rec {
    # Host side.
    root = "${cfg.runtimeRoot}/jobs";
    slotDir = slotName: "${root}/${slotName}";
    hostSpec = slotName: "${slotDir slotName}/${specName}";
    hostPrompt = slotName: "${slotDir slotName}/${promptName}";
    hostOutDir = slotName: "${slotDir slotName}/out";
    hostResult = slotName: "${hostOutDir slotName}/${resultName}";

    # Host-only archive of finished job results, so `status <task>` still knows
    # the outcome after the slot has been released and its job dir cleared.
    # NOT shared into any guest.
    resultsDir = "${cfg.runtimeRoot}/results";
    hostArchivedResult = taskId: "${resultsDir}/${taskId}.json";

    specName = "spec.json";
    promptName = "prompt.md";
    resultName = "result.json";

    # Guest side (identical for every slot — the share hides which slot it is).
    guestTag = "job";
    guestMountPoint = "/run/agent-job";
    guestSpec = "${guestMountPoint}/${specName}";
    guestPrompt = "${guestMountPoint}/${promptName}";
    guestOutDir = "${guestMountPoint}/out";
    guestResult = "${guestOutDir}/${resultName}";

    # Schema version understood by BOTH sides. Bump only together with the
    # validation in the host launcher and in `agent-job` below.
    specVersion = 1;

    # The terminal / transitional job states written to result.json.
    states = [
      "starting"
      "running"
      "completed"
      "failed"
      "timed-out"
      "cancelled"
      "infrastructure-error"
    ];
  };

  # --- guest-side batch runner -------------------------------------------
  # Runs as the unprivileged `agent` user from `agent-job.service` below.
  agent-job = pkgs.writeShellApplication {
    name = "agent-job";
    runtimeInputs = with pkgs; [
      coreutils # timeout, cat, mv, date, id
      jq
    ];
    text = ''
      set -euo pipefail

      readonly SPEC=${lib.escapeShellArg paths.guestSpec}
      readonly PROMPT_FILE=${lib.escapeShellArg paths.guestPrompt}
      readonly OUT_DIR=${lib.escapeShellArg paths.guestOutDir}
      readonly RESULT=${lib.escapeShellArg paths.guestResult}
      readonly WORKSPACE=/workspace
      readonly SPEC_VERSION=${toString paths.specVersion}
      readonly MAX_TIMEOUT=${toString jobCfg.maxTimeoutSeconds}
      # Grace given to the agent after SIGTERM before SIGKILL, inside the
      # guest, so it can still flush partial work to /workspace.
      readonly KILL_GRACE=10

      log() { printf 'agent-job: %s\n' "$*" >&2; }

      task_id="unknown"
      agent="unknown"
      started_at="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

      # Atomic state transition: write a temp file in the SAME (guest-writable)
      # directory, then rename, so the host either sees the old or the new
      # result — never a half-written one.
      write_result() {
          local state="$1" exit_code="$2" timed_out="$3" message="''${4-}"
          local tmp
          tmp="$(mktemp "$OUT_DIR/.result.XXXXXX")"
          jq -n \
              --argjson version "$SPEC_VERSION" \
              --arg taskId "$task_id" \
              --arg agent "$agent" \
              --arg state "$state" \
              --argjson exitCode "$exit_code" \
              --arg startedAt "$started_at" \
              --arg finishedAt "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
              --argjson timedOut "$timed_out" \
              --arg message "$message" \
              '{version:$version, taskId:$taskId, agent:$agent, state:$state,
                exitCode:$exitCode, startedAt:$startedAt, finishedAt:$finishedAt,
                timedOut:$timedOut, message:$message}' > "$tmp"
          mv -f -- "$tmp" "$RESULT"
      }

      # Any problem that is NOT the agent's own failure is reported as
      # `infrastructure-error`, so the host can distinguish "the agent failed"
      # from "the job never ran".
      infra_fail() {
          log "infrastructure error: $*"
          write_result infrastructure-error 70 false "$*"
          exit 70
      }

      # --- inert without a job (ticket 4: "remain inert when no batch job") --
      # The unit also carries ConditionPathExists, so this is belt-and-braces
      # for a manual invocation.
      if [[ ! -e "$SPEC" ]]; then
          log "no job spec at $SPEC; nothing to do"
          exit 0
      fi
      [[ -d "$OUT_DIR" && -w "$OUT_DIR" ]] \
          || { log "job output dir $OUT_DIR is missing or not writable"; exit 70; }

      # --- validate the spec (guest side; the host validated it too) --------
      jq -e . "$SPEC" >/dev/null 2>&1 || infra_fail "spec is not valid JSON"

      version="$(jq -r '.version // empty' "$SPEC")"
      [[ "$version" == "$SPEC_VERSION" ]] \
          || infra_fail "unsupported spec version '$version' (expected $SPEC_VERSION)"

      # A spec must NEVER be able to name a command: the agent is resolved from
      # the generated registry dispatch below. Refuse loudly if a future host
      # version tries to smuggle one in.
      if jq -e 'has("command") or has("exec") or has("executable")' "$SPEC" >/dev/null; then
          infra_fail "spec must not contain an executable path"
      fi

      task_id="$(jq -r '.taskId // empty' "$SPEC")"
      [[ "$task_id" =~ ^[a-zA-Z0-9._-]{1,64}$ ]] \
          || infra_fail "invalid taskId '$task_id'"

      agent="$(jq -r '.agent // empty' "$SPEC")"

      workspace="$(jq -r '.workspace // empty' "$SPEC")"
      [[ "$workspace" == "$WORKSPACE" ]] \
          || infra_fail "workspace must be exactly $WORKSPACE (got '$workspace')"
      [[ -d "$WORKSPACE" && -w "$WORKSPACE" ]] \
          || infra_fail "$WORKSPACE is not a writable directory"

      prompt_path="$(jq -r '.promptFile // empty' "$SPEC")"
      # Exact match, not a prefix check: this rejects traversal
      # ("$JOB_DIR/../x"), symlink games and any path outside the job dir.
      [[ "$prompt_path" == "$PROMPT_FILE" ]] \
          || infra_fail "promptFile must be exactly $PROMPT_FILE (got '$prompt_path')"
      [[ -r "$PROMPT_FILE" ]] || infra_fail "prompt file $PROMPT_FILE is not readable"

      timeout_s="$(jq -r '.timeoutSeconds // empty' "$SPEC")"
      [[ "$timeout_s" =~ ^[0-9]+$ ]] || infra_fail "timeoutSeconds must be an integer"
      (( timeout_s >= 1 )) || infra_fail "timeoutSeconds must be >= 1"
      (( timeout_s <= MAX_TIMEOUT )) \
          || infra_fail "timeoutSeconds $timeout_s exceeds the guest maximum $MAX_TIMEOUT"

      # --- run the agent ----------------------------------------------------
      cd "$WORKSPACE"
      write_result starting 0 false

      prompt="$(cat -- "$PROMPT_FILE")"

      # Per-job hard limit (the unit adds a second, static RuntimeMaxSec
      # ceiling, and the host enforces a third with a grace period).
      run_agent() {
          timeout --kill-after="$KILL_GRACE" -- "$timeout_s" "$@"
      }
      # Same, for CLIs that read their instructions from stdin — the prompt
      # then never becomes an argv element at all.
      run_agent_stdin() {
          timeout --kill-after="$KILL_GRACE" -- "$timeout_s" "$@" < "$PROMPT_FILE"
      }

      log "running agent '$agent' for task '$task_id' (timeout ''${timeout_s}s)"
      write_result running 0 false

      rc=0
      case "$agent" in
      ${agentRegistry.batchDispatchCases}
          *)
              infra_fail "agent '$agent' cannot run unattended (expected: ${agentRegistry.batchNamesAlternation})"
              ;;
      esac || rc=$?

      # timeout(1) reports 124 when it had to signal the child, and 128+9=137
      # when --kill-after had to SIGKILL it.
      timed_out=false
      state=completed
      if (( rc == 124 || rc == 137 )); then
          timed_out=true
          state=timed-out
      elif (( rc != 0 )); then
          state=failed
      fi

      log "agent '$agent' finished: state=$state exit=$rc"
      write_result "$state" "$rc" "$timed_out"

      # Exit 0 for a *reported* agent failure: the authoritative outcome is
      # result.json, and a failed unit would only add noise (and, with
      # microvm.nix's Restart=always on the VM, confusion). Infrastructure
      # errors above DO exit non-zero.
      exit 0
    '';
    meta = with lib; {
      description = "Guest-side unattended batch runner for myconfig.ai.microvm sandboxes";
      platforms = platforms.linux;
    };
  };

  # --- guest-side NixOS module fragment ---------------------------------
  # Merged into every slot's guest config by guest.nix, next to the workspace /
  # hostkey shares.
  guestModule = {
    environment.systemPackages = [ agent-job ];

    systemd.services.agent-job = {
      description = "Unattended agent batch job (myconfig.ai.microvm)";
      wantedBy = [ "multi-user.target" ];
      # Only run when the host actually placed a job in the share. Without a
      # spec the unit is skipped entirely, so an interactive slot is unaffected.
      unitConfig = {
        ConditionPathExists = paths.guestSpec;
        # Resolves to the generated .mount units for both shares, so the job
        # never starts before the workspace and the job data are available.
        RequiresMountsFor = "/workspace ${paths.guestMountPoint}";
      };
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      # The agent binaries plus the runner's own tools. Explicit, so the unit
      # does not depend on the ambient systemd PATH.
      path =
        agentRegistry.packages
        ++ (with pkgs; [
          bash
          coreutils
          git
          gnugrep
          gnused
          jq
          ripgrep
        ]);
      serviceConfig = {
        Type = "oneshot";
        ExecStart = lib.getExe agent-job;
        # §6/ticket 4: the job runs as the SAME unprivileged guest user as the
        # interactive session, in the workspace, with no way to gain privileges.
        User = "agent";
        Group = "users";
        WorkingDirectory = "/workspace";
        NoNewPrivileges = true;
        PrivateDevices = true;
        PrivateTmp = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        RestrictSUIDSGID = true;
        # STATIC ceiling on top of the per-job `timeout(1)`: even a job spec
        # that somehow passed validation cannot run longer than this.
        RuntimeMaxSec = jobCfg.maxTimeoutSeconds + jobCfg.gracePeriodSeconds;
        # Give the agent the same SIGTERM grace the per-job timeout uses, so a
        # host-side `cancel` still lets it flush work to /workspace.
        TimeoutStopSec = 30;
      };
    };
  };
in
{
  options.myconfig.ai.microvm.job = with lib; {
    defaultTimeoutSeconds = mkOption {
      type = types.ints.positive;
      default = 3600;
      description = ''
        Default hard runtime limit for an unattended job
        (`agent-microvm submit` without `--timeout`).
      '';
    };

    maxTimeoutSeconds = mkOption {
      type = types.ints.positive;
      default = 86400;
      description = ''
        Upper bound for a job's `--timeout`. Enforced by the host launcher,
        re-validated by the guest runner, and used for the guest unit's static
        `RuntimeMaxSec` ceiling.
      '';
    };

    gracePeriodSeconds = mkOption {
      type = types.ints.positive;
      default = 120;
      description = ''
        Extra time the HOST waits beyond a job's own timeout before it
        force-stops the VM, so the guest can finish writing `result.json`
        after its in-guest `timeout(1)` fired.
      '';
    };
  };

  config = lib.mkMerge [
    # Path/format definitions + the guest module fragment, exported for
    # guest.nix (shares + service) and launcher.nix (submit/status/recover).
    {
      _module.args.agentJobs = paths // {
        inherit guestModule;
        runner = agent-job;
      };
    }

    (lib.mkIf cfg.enable {
      assertions = [
        {
          assertion = cfg.job.defaultTimeoutSeconds <= cfg.job.maxTimeoutSeconds;
          message = "myconfig.ai.microvm.job.defaultTimeoutSeconds must be <= job.maxTimeoutSeconds.";
        }
      ];

      # virtiofsd refuses to start when a share source is missing, so every
      # slot's job directory must exist before any VM starts — including slots
      # that never ran a job. `out/` is owned by the guest agent uid/gid (§11)
      # because only the guest writes result.json.
      systemd.tmpfiles.rules = [
        "d ${paths.root} 0755 root root - -"
        "d ${paths.resultsDir} 0755 root root - -"
      ]
      ++ lib.concatMap (slot: [
        "d ${paths.slotDir slot.name} 0755 root root - -"
        "d ${paths.hostOutDir slot.name} 0755 ${toString cfg.guestAgentUid} ${toString cfg.guestAgentGid} - -"
      ]) slots;
    })
  ];
}
