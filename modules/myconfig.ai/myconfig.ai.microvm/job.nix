# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.microvm — UNATTENDED BATCH JOBS: the versioned job format, the
# host-side job directories, the TRUSTED guest-side job CONTROLLER, the
# UNTRUSTED guest-side WORKER and the host-side result VERIFIER.
#
# ---------------------------------------------------------------------------
# TRUST SPLIT (the whole point of this file)
# ---------------------------------------------------------------------------
# The coding agent and every process the repository can start are UNTRUSTED.
# They must not be able to create, replace, delete, rename or shadow the
# authoritative job result, because the HOST acts on that result (it decides
# success/failure, stops the VM and reports an exit code to the operator).
#
# Therefore a batch job runs as TWO guest identities:
#
#   * `agent-job-controller.service` — TRUSTED, runs as guest root. It is the
#     only writer of the authoritative result. It validates the immutable job
#     spec, starts the worker as an unprivileged user, enforces the timeout and
#     cancellation, collects the worker's exit status from systemd and writes
#     the result. It NEVER executes a repository- or spec-provided command: the
#     worker's executable and argv come from the Nix-generated registry
#     (./agents.nix) only.
#
#   * `agent-job-worker@<agent>.service` — UNTRUSTED, runs as the guest `agent`
#     user (uid `guestAgentUid`, the same identity as the interactive session).
#     It runs the coding agent, and therefore transitively whatever the
#     repository asks for. Its stdout/stderr/artifacts are untrusted output.
#
# Layout (per slot; the directories are pre-created by tmpfiles because
# virtiofsd needs its share source to exist before the VM starts):
#
#   ${runtimeRoot}/jobs/<slot>/                  root:root 0755
#     input/                                     root:root 0755
#       spec.json                                root:root 0400  (1)
#       prompt.md                                root:root 0444  (2)
#       cancel.json                              root:root 0400  (3)
#     controller/                                root:root 0700  (4)
#       state.json                               root:root 0600   trusted progress
#       result.json                              root:root 0600   AUTHORITATIVE
#     worker/                                    1000:1000 0755  (5)
#       artifacts/                                                untrusted
#     worker-logs/                               root:root 0755  (6)
#       stdout.log stderr.log                    root:root 0644   untrusted content
#
#   (1) 0400 root-only: the spec carries the allocation token, which the worker
#       must not be able to read (it would otherwise be able to mint a result
#       that passes the host's identity checks if it ever gained write
#       access). The token
#       therefore also never travels through a process ARGUMENT VECTOR on
#       either side: /proc/<pid>/cmdline is world-readable (0444) while
#       /proc/<pid>/environ is 0400, so every helper that needs the token is
#       handed it in its ENVIRONMENT (and the worker unit additionally hides
#       other processes' /proc entries with ProtectProc=invisible).
#   (2) 0444: the worker must READ the prompt; nobody may modify it.
#   (3) the host's cancellation request, bound to the allocation token.
#   (4) 0700 root-only: NOT writable and NOT readable by the worker. virtiofsd
#       passes ownership through unchanged, so this is the EFFECTIVE permission
#       inside the guest. The worker also cannot rename/replace this directory,
#       because its parent (`/run/agent-job`) is root-owned 0755, and the guest
#       unit additionally lists it as `InaccessiblePaths`.
#   (5) the only worker-writable part of the share.
#   (6) the worker's stdout/stderr. systemd (PID 1, root) opens these with
#       `append:` and FOLLOWS symlinks, so they must not live anywhere the
#       worker uid can create, rename or replace a path: `worker-logs/` is
#       root-owned and sits directly under the root-owned 0755 share root, so
#       only root can touch the directory or the files in it. The worker may
#       READ its own logs; their CONTENT is untrusted all the same.
#
# Design rules taken from the ticket:
#   * prompts and specs NEVER go through process arguments (the host writes
#     files; the guest reads them) and NEVER into the Nix store (they are
#     runtime-only files under ${runtimeRoot});
#   * the spec is VERSIONED and validated on BOTH sides;
#   * the spec cannot name an executable — the agent is resolved through the
#     authoritative registry (./agents.nix), so a job can only ever run an
#     agent this module declares;
#   * every allocation carries a 256-bit random ALLOCATION TOKEN. It is written
#     into the host allocation marker, into the guest's immutable input and into
#     the authoritative result, and the host rejects any result whose version,
#     task id, token, slot or agent does not match the ACTIVE allocation. This
#     is what makes a stale or cross-allocation result harmless;
#   * `result.json` is written with tmp-file + rename. That gives CONSISTENCY
#     (the host never reads a half-written file) — it does NOT establish
#     authenticity. Authenticity comes from ownership (only guest root can
#     write `controller/`) plus the allocation token;
#   * the timeout is enforced by the CONTROLLER (which stops the worker's whole
#     cgroup), by the worker unit's static `TimeoutStartSec` ceiling, and once
#     more by the host;
#   * the host treats the result as untrusted input regardless: it is parsed and
#     identity-checked by ONE verifier (`agent-job-verify-result`), and anything
#     malformed becomes an INFRASTRUCTURE ERROR, never a success.
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
  # The ONE definition of the per-session tree (./session.nix): the layout table
  # this file derives its per-slot root, its subdirectory NAMES and its directory
  # MODES from, so the trust boundary is written down exactly once. The job data
  # lives IN the session tree, i.e. in the ONE writable share.
  agentSession,
  # The ONE resolved capability set (see default.nix, lightweight plan phase 5).
  # A host that does not select `batch` gets NONE of the guest units, guest
  # packages, host result archive or unit-ordering statements below — the
  # decision lives here (and in ../session.nix's per-capability layout table),
  # never in the consumers.
  agentCapabilities,
  ...
}:
let
  cfg = config.myconfig.ai.microvm;
  jobCfg = cfg.job;
  session = agentSession;

  # --- the ONE definition of every job path / mode / schema fact ----------
  paths = rec {
    # ---- host side ----------------------------------------------------
    # The per-slot job data IS the session directory (./session.nix), i.e. part
    # of the ONE writable share.
    root = session.root;
    slotDir = slotName: "${root}/${slotName}";

    # Subdirectory NAMES come from the session layout table (./session.nix),
    # the single source of truth for the tree's shape.
    inputSubdir = session.subdirs.input;
    controllerSubdir = session.subdirs.controller;
    workerSubdir = session.subdirs.worker;
    # The worker's log files live NEXT TO its writable directory, not inside it:
    # systemd opens them as root and follows symlinks, so the whole path must be
    # root-controlled (see (6) in the header).
    workerLogsSubdir = session.subdirs.workerLogs;
    artifactsSubdir = "artifacts";

    specName = "spec.json";
    promptName = "prompt.md";
    cancelName = "cancel.json";
    resultName = "result.json";
    controllerStateName = "state.json";
    workerStdoutName = "stdout.log";
    workerStderrName = "stderr.log";

    hostInputDir = s: "${slotDir s}/${inputSubdir}";
    hostControllerDir = s: "${slotDir s}/${controllerSubdir}";
    hostWorkerDir = s: "${slotDir s}/${workerSubdir}";
    hostWorkerLogsDir = s: "${slotDir s}/${workerLogsSubdir}";
    hostSpec = s: "${hostInputDir s}/${specName}";
    hostPrompt = s: "${hostInputDir s}/${promptName}";
    hostCancel = s: "${hostInputDir s}/${cancelName}";
    hostResult = s: "${hostControllerDir s}/${resultName}";
    hostControllerState = s: "${hostControllerDir s}/${controllerStateName}";

    # Host-only archive of finished job results, so `status <task>` still knows
    # the outcome after the slot has been released and its job dir cleared.
    # NOT shared into any guest.
    resultsDir = "${cfg.runtimeRoot}/results";
    hostArchivedResult = taskId: "${resultsDir}/${taskId}.json";

    # ---- permission facts (host tmpfiles + guest assertions agree) -----
    # From the session layout table (./session.nix): ONE place decides who owns
    # what in this tree.
    # `modeOf` reads the FULL table, not the capability-filtered one, so these
    # four modes exist even on a host that creates none of the directories. That
    # is intentional (a directory's mode is a policy fact of the layout, not of
    # this host's selection) and harmless: everything derived from them lives
    # inside the `batch`-only fragments below, which are `lib.optionalAttrs`-ed
    # away wholesale on a host without the capability. The coupling is therefore
    # "these constants are inert without `batch`", NOT "this file is
    # capability-independent".
    inputDirMode = session.modeOf session.subdirs.input;
    controllerDirMode = session.modeOf session.subdirs.controller;
    workerDirMode = session.modeOf session.subdirs.worker;
    # root-owned: the worker must not be able to rename or replace the directory
    # whose files systemd opens as root.
    workerLogsDirMode = session.modeOf session.subdirs.workerLogs;
    workerLogMode = "0644";
    specMode = "0400";
    promptMode = "0444";
    cancelMode = "0400";
    resultMode = "0600";
    # The UNPRIVILEGED guest identity that runs the coding agent. The
    # controller asserts this is never 0.
    workerUid = cfg.guestAgentUid;
    # NOTE (deliberate mismatch): this is the numeric GID the HOST chowns the
    # worker-writable directory to, NOT the worker's primary group. The guest
    # `agent` user is an `isNormalUser`, so its primary group is `users`
    # (gid 100); `worker/` therefore ends up group-owned by a gid the worker is
    # not a member of. That is harmless — access is granted by the OWNER bits
    # (uid matches `workerUid`) and the group bits never grant more than the
    # owner bits — and it keeps the host-side chown symmetrical with the
    # workspace clone (`chown 1000:1000`). Do not "fix" this by widening group
    # access.
    workerGid = cfg.guestAgentGid;
    workerUser = "agent";
    # `isNormalUser` puts the guest agent into the `users` group.
    workerGroup = "users";

    # ---- guest side (identical for every slot — the share hides the slot) --
    # The job data is reached through the ONE writable session mount; the
    # subdirectory names — and therefore every path the guest controller/worker
    # validate — are the layout table's.
    guestMountPoint = session.guestMountPoint;
    guestInputDir = "${guestMountPoint}/${inputSubdir}";
    guestControllerDir = "${guestMountPoint}/${controllerSubdir}";
    guestWorkerDir = "${guestMountPoint}/${workerSubdir}";
    guestSpec = "${guestInputDir}/${specName}";
    guestPrompt = "${guestInputDir}/${promptName}";
    guestCancel = "${guestInputDir}/${cancelName}";
    guestResult = "${guestControllerDir}/${resultName}";
    guestControllerState = "${guestControllerDir}/${controllerStateName}";
    guestWorkerLogsDir = "${guestMountPoint}/${workerLogsSubdir}";
    guestWorkerStdout = "${guestWorkerLogsDir}/${workerStdoutName}";
    guestWorkerStderr = "${guestWorkerLogsDir}/${workerStderrName}";
    guestWorkerArtifacts = "${guestWorkerDir}/${artifactsSubdir}";

    controllerUnit = "agent-job-controller.service";
    workerUnitTemplate = "agent-job-worker@";
    # The instance is the AGENT NAME, which the registry constrains to
    # [a-z][a-z0-9-]* — never a task id or any other caller-supplied string
    # (a hostile task name must not be able to reach a unit name).
    workerUnit = agent: "agent-job-worker@${agent}.service";

    # Schema version understood by BOTH sides. Bumped from 1 to 2 for the
    # controller/worker split: results gained `allocationToken`, `slot` and
    # `controllerVersion`, and moved from `out/result.json` to
    # `controller/result.json`. There is NO compatibility mode — a v1 result is
    # rejected (fail closed).
    specVersion = 2;
    # Version of the CONTROLLER protocol (the writer of the result). Bumped
    # independently of the spec when the controller's own semantics change.
    controllerVersion = 1;

    # The terminal states the controller may write into result.json. A
    # non-terminal state in result.json is a protocol violation (progress goes
    # into controller/state.json instead).
    terminalStates = [
      "completed"
      "failed"
      "timed-out"
      "cancelled"
      "infrastructure-error"
    ];
    # The controller's non-authoritative progress phases (state.json).
    phases = [
      "validating"
      "starting-worker"
      "running"
      "timing-out"
      "cancelling"
      "finished"
    ];
  };

  # Seconds the controller gives the worker cgroup between SIGTERM and SIGKILL,
  # so the agent can still flush partial work to /workspace. Also the worker
  # unit's TimeoutStopSec, so systemd's own stop path uses the same grace.
  workerKillGraceSeconds = 10;

  # Static ceiling for the worker unit. `Type=oneshot` ignores RuntimeMaxSec,
  # so the ceiling is expressed as TimeoutStartSec (which kills the whole
  # cgroup on expiry with Result=timeout).
  workerCeilingSeconds = jobCfg.maxTimeoutSeconds + jobCfg.gracePeriodSeconds;

  # The worker's unit PATH: the agent binaries plus a small toolchain. Rendered
  # exactly the way NixOS renders `systemd.services.<n>.path`.
  workerPackages =
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

  # ======================================================================
  # (1) guest-side PATH/PERMISSION ASSERTIONS (trusted, runs as root)
  # ======================================================================
  # Fails the controller before anything is started when the job share does not
  # have the EFFECTIVE permissions the trust split depends on. Mode 0600 on
  # result.json is worthless if a parent directory can be replaced, so every
  # parent component up to the boundary is checked too.
  #
  # Kept as a SEPARATE, argument-driven program on purpose: the check suite
  # runs it against deliberately broken fixtures, so this logic is actually
  # exercised rather than merely read.
  agent-job-assert-paths = pkgs.writeShellApplication {
    name = "agent-job-assert-paths";
    runtimeInputs = with pkgs; [ coreutils ];
    text = ''
      set -euo pipefail

      root=${lib.escapeShellArg paths.guestMountPoint}
      worker_uid=${toString paths.workerUid}
      # Walk parent directories up to and including this one. Defaults to `/`
      # (the guest case); the check suite passes its fixture's parent.
      boundary=/

      while [[ $# -gt 0 ]]; do
          case "$1" in
              --root)       root="''${2-}"; shift 2 ;;
              --worker-uid) worker_uid="''${2-}"; shift 2 ;;
              --boundary)   boundary="''${2-}"; shift 2 ;;
              *) printf 'agent-job-assert-paths: unknown argument %s\n' "$1" >&2; exit 64 ;;
          esac
      done

      bad() {
          printf 'agent-job-assert-paths: %s\n' "$*" >&2
          exit 1
      }

      [[ "$worker_uid" =~ ^[0-9]+$ ]] || bad "--worker-uid must be numeric"
      (( worker_uid != 0 )) || bad "the worker uid must be unprivileged (never 0)"

      # `stat -c` on the path itself (never dereferencing a symlink).
      owner_of() { stat -c %u -- "$1"; }
      mode_of()  { printf '%d' "0$(stat -c %a -- "$1")"; }

      # A directory that only root may modify: root-owned, no group/other write.
      assert_root_dir() {
          local d="$1" what="$2" owner mode
          [[ ! -L "$d" ]] || bad "$what is a symlink: $d"
          [[ -d "$d" ]] || bad "$what is missing or not a directory: $d"
          owner="$(owner_of "$d")"
          (( owner == 0 )) || bad "$what must be owned by uid 0, is owned by $owner: $d"
          mode="$(mode_of "$d")"
          (( (mode & 0022) == 0 )) \
              || bad "$what is group/other-writable (mode $(stat -c %a -- "$d")): $d"
      }

      # The controller channel: additionally UNREADABLE and UNSEARCHABLE for
      # everyone but root, so the worker cannot read the documents that carry
      # the allocation token. (The token is kept out of process argv on both
      # sides for the same reason — see the note on (1) in the header.)
      assert_controller_dir() {
          local d="$1" mode
          assert_root_dir "$d" "the controller directory"
          mode="$(mode_of "$d")"
          (( (mode & 0077) == 0 )) \
              || bad "the controller directory grants group/other access (mode $(stat -c %a -- "$d")): $d"
      }

      # A root-owned file nobody else may write. `max_mode` masks the bits that
      # must be clear (e.g. 0177 for a root-only 0400 file).
      assert_root_file() {
          local f="$1" what="$2" forbidden="$3" owner mode
          [[ ! -L "$f" ]] || bad "$what is a symlink: $f"
          [[ -f "$f" ]] || bad "$what is missing or not a regular file: $f"
          owner="$(owner_of "$f")"
          (( owner == 0 )) || bad "$what must be owned by uid 0, is owned by $owner: $f"
          mode="$(mode_of "$f")"
          (( (mode & forbidden) == 0 )) \
              || bad "$what has too permissive a mode ($(stat -c %a -- "$f")): $f"
      }

      # --- the share root and every parent up to the boundary --------------
      # A worker that could rename or replace any of these could shadow the
      # authoritative result no matter what mode the file itself has.
      assert_root_dir "$root" "the job share root"
      boundary="$(realpath -m -- "$boundary")"
      p="$(realpath -m -- "$root")"
      while :; do
          assert_root_dir "$p" "job-share parent directory"
          [[ "$p" == "$boundary" ]] && break
          [[ "$p" == "/" ]] && break
          p="$(dirname -- "$p")"
      done

      # --- the immutable input --------------------------------------------
      assert_root_dir "$root/${paths.inputSubdir}" "the job input directory"
      # 0177: root-only. The spec carries the allocation token.
      assert_root_file "$root/${paths.inputSubdir}/${paths.specName}" "the job spec" 0177
      # 0222: readable by all, writable by none.
      assert_root_file "$root/${paths.inputSubdir}/${paths.promptName}" "the prompt file" 0222
      if [[ -e "$root/${paths.inputSubdir}/${paths.cancelName}" || -L "$root/${paths.inputSubdir}/${paths.cancelName}" ]]; then
          assert_root_file "$root/${paths.inputSubdir}/${paths.cancelName}" "the cancellation request" 0177
      fi

      # --- the authoritative result channel --------------------------------
      assert_controller_dir "$root/${paths.controllerSubdir}"
      for f in ${paths.resultName} ${paths.controllerStateName}; do
          target="$root/${paths.controllerSubdir}/$f"
          # A symlink here would let a writer of the target path escape the
          # controller directory; absent is fine (nothing written yet).
          [[ ! -L "$target" ]] || bad "controller file is a symlink: $target"
          if [[ -e "$target" ]]; then
              assert_root_file "$target" "controller file" 0177
          fi
      done

      # --- the worker's own (untrusted, writable) area ----------------------
      w="$root/${paths.workerSubdir}"
      [[ ! -L "$w" ]] || bad "the worker directory is a symlink: $w"
      [[ -d "$w" ]] || bad "the worker directory is missing: $w"
      owner="$(owner_of "$w")"
      (( owner == worker_uid )) \
          || bad "the worker directory must be owned by uid $worker_uid, is owned by $owner: $w"

      # --- the worker's LOG directory (root-owned on purpose) ---------------
      # systemd opens stdout.log/stderr.log as root, following symlinks, so the
      # directory must not be one the worker uid can write, rename or replace.
      # Absent is fine (the controller creates it before it starts the worker).
      l="$root/${paths.workerLogsSubdir}"
      if [[ -e "$l" || -L "$l" ]]; then
          assert_root_dir "$l" "the worker log directory"
          for f in ${paths.workerStdoutName} ${paths.workerStderrName}; do
              target="$l/$f"
              [[ ! -L "$target" ]] || bad "worker log file is a symlink: $target"
              if [[ -e "$target" ]]; then
                  # 0022: root-owned, writable by nobody else (0644 is expected).
                  assert_root_file "$target" "worker log file" 0022
              fi
          done
      fi

      exit 0
    '';
    meta = with lib; {
      description = "Assert the effective permissions of the myconfig.ai.microvm job share";
      platforms = platforms.linux;
    };
  };

  # ======================================================================
  # (2) guest-side WORKER (UNTRUSTED, runs as the guest `agent` user)
  # ======================================================================
  # Started ONLY by the controller, through the predeclared templated unit
  # `agent-job-worker@<agent>.service`. It receives the already-validated agent
  # NAME as its single argument (the unit instance) and resolves the executable
  # and argv from the generated registry dispatch — never from the spec, which
  # it cannot even read.
  #
  # Everything this process writes (stdout, stderr, artifacts, /workspace) is
  # UNTRUSTED output. It has no way to influence the authoritative result:
  # `controller/` is root-owned 0700 and additionally masked by the unit's
  # `InaccessiblePaths`.
  # `prompt` is read by the GENERATED dispatch below only for the registry
  # entries that take the prompt TEXT as an argv token (`%PROMPT%`). A host
  # whose `enabledAgents` selects only stdin-driven agents — e.g.
  # `[ "codex" ]`, which reads the prompt file on stdin — therefore generates a
  # script in which the variable is genuinely unread, and `writeShellApplication`'s
  # shellcheck gate fails the BUILD with SC2034. The read is kept (rather than
  # made conditional) so the two invocation shapes stay symmetrical and enabling
  # a `%PROMPT%` agent needs no change here; the suppression is emitted ONLY on
  # the hosts where it is true, so the generated script is unchanged everywhere
  # else.
  promptUnusedSuppression = lib.optionalString (!agentRegistry.batchUsesPromptText) (
    lib.concatStringsSep "\n      " [
      "# NOTE: on this host every selected batch agent is stdin-driven, so the"
      "# generated dispatch below never reads `prompt`."
      "# shellcheck disable=SC2034"
      ""
    ]
  );

  agent-job-worker = pkgs.writeShellApplication {
    name = "agent-job-worker";
    runtimeInputs = with pkgs; [ coreutils ];
    text = ''
      set -euo pipefail

      readonly PROMPT_FILE=${lib.escapeShellArg paths.guestPrompt}

      log() { printf 'agent-job-worker: %s\n' "$*" >&2; }

      [[ $# -eq 1 ]] || { log "usage: agent-job-worker <agent>"; exit 64; }
      agent="$1"

      [[ -r "$PROMPT_FILE" ]] || { log "prompt file $PROMPT_FILE is not readable"; exit 70; }
      # The prompt TEXT, for the registry entries that take it as an argument.
      ${promptUnusedSuppression}prompt="$(cat -- "$PROMPT_FILE")"

      # The two invocation shapes the generated dispatch below calls. There is
      # deliberately NO timeout(1) here: the deadline belongs to the trusted
      # controller (which stops this unit's whole cgroup) plus the unit's own
      # static TimeoutStartSec ceiling. A timeout enforced by the untrusted
      # worker would be worthless as evidence anyway.
      run_agent() { "$@"; }
      run_agent_stdin() { "$@" < "$PROMPT_FILE"; }

      log "running agent '$agent' in $PWD"
      case "$agent" in
      ${agentRegistry.batchDispatchCases}
          *)
              log "agent '$agent' cannot run unattended (expected: ${agentRegistry.batchNamesAlternation})"
              exit 64
              ;;
      esac
    '';
    meta = with lib; {
      description = "Untrusted guest-side batch worker for myconfig.ai.microvm sandboxes";
      platforms = platforms.linux;
    };
  };

  # ======================================================================
  # (3) guest-side CONTROLLER (TRUSTED, runs as guest root)
  # ======================================================================
  agent-job-controller = pkgs.writeShellApplication {
    name = "agent-job-controller";
    runtimeInputs = with pkgs; [
      coreutils # date, install, mktemp, mv, stat, uname
      jq
      systemd # systemctl
    ];
    text = ''
      set -euo pipefail

      readonly SPEC=${lib.escapeShellArg paths.guestSpec}
      readonly PROMPT_FILE=${lib.escapeShellArg paths.guestPrompt}
      readonly CANCEL_FILE=${lib.escapeShellArg paths.guestCancel}
      readonly CTRL_DIR=${lib.escapeShellArg paths.guestControllerDir}
      readonly RESULT=${lib.escapeShellArg paths.guestResult}
      readonly STATE_FILE=${lib.escapeShellArg paths.guestControllerState}
      readonly WORKER_DIR=${lib.escapeShellArg paths.guestWorkerDir}
      readonly WORKER_LOG_DIR=${lib.escapeShellArg paths.guestWorkerLogsDir}
      readonly WORKER_STDOUT=${lib.escapeShellArg paths.guestWorkerStdout}
      readonly WORKER_STDERR=${lib.escapeShellArg paths.guestWorkerStderr}
      readonly WORKER_ARTIFACTS=${lib.escapeShellArg paths.guestWorkerArtifacts}
      readonly WORKSPACE=/workspace
      readonly SPEC_VERSION=${toString paths.specVersion}
      readonly CONTROLLER_VERSION=${toString paths.controllerVersion}
      readonly MAX_TIMEOUT=${toString jobCfg.maxTimeoutSeconds}
      readonly WORKER_UID=${toString paths.workerUid}
      readonly WORKER_GID=${toString paths.workerGid}
      readonly WORKER_KILL_GRACE=${toString workerKillGraceSeconds}
      readonly POLL_INTERVAL=2
      # `systemctl start --no-block` only ENQUEUES the job, so the unit can
      # legitimately still read `inactive` for a moment (or longer, if it is
      # waiting for one of its dependencies). Only after this window does a
      # still-inactive worker count as "it never ran". Measured on the same
      # wall clock as the deadline, but INDEPENDENT of it: a worker that never
      # started is an infrastructure error, not a timeout, even when the job's
      # own timeout is shorter than this grace.
      readonly WORKER_STARTUP_GRACE=60
      readonly ASSERT_PATHS=${lib.getExe agent-job-assert-paths}
      # Every key a v${toString paths.specVersion} spec may carry. Anything else
      # is rejected: an unknown field means host and guest disagree about the
      # protocol, and guessing is how privilege ends up smuggled in.
      readonly SPEC_KEYS='["version","taskId","allocationToken","slot","agent","workspace","promptFile","timeoutSeconds","resourceClass","persistAgentState"]'

      log() { printf 'agent-job-controller: %s\n' "$*" >&2; }

      # Structured guest-side lifecycle events. The controller unit's
      # StandardError is `journal+console`, so they land on the guest console,
      # which microvm.nix captures into the HOST journal
      # (`journalctl -u microvm@<slot>`), so host and guest transitions can be
      # correlated. Never contains the prompt, a key, the allocation token or
      # any env var.
      emit_event() {
          local event="$1" state="''${2-}" exit_code="''${3-}"
          jq -nc --arg ts "$(date -u +%Y-%m-%dT%H:%M:%SZ)" --arg event "$event" \
              --arg task "$task_id" --arg agent "$agent" --arg state "$state" \
              --arg exit_code "$exit_code" --arg host "$(uname -n)" \
              '{ts:$ts, event:$event, task:$task, agent:$agent, slot:$host,
                state:$state, exit_code:$exit_code}
               | with_entries(select(.value != ""))' >&2
      }

      task_id="unknown"
      agent="unknown"
      allocation_token=""
      slot="$(uname -n)"
      timeout_s=0
      worker_unit=""
      started_at="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
      # How the CONTROLLER itself ended the worker, if it did. This — never the
      # worker's own claim — is what makes a result `timed-out` or `cancelled`.
      controller_verdict=""

      # Write a file into the controller directory atomically: mktemp in the
      # SAME directory, write the complete document, tighten the mode, rename.
      # The temp file is removed on every error path.
      write_controller_file() {
          local dest="$1" tmp
          tmp="$(mktemp "$CTRL_DIR/.tmp.XXXXXX")" || {
              log "could not create a temp file in $CTRL_DIR"
              return 1
          }
          if cat > "$tmp" \
              && chmod ${paths.resultMode} -- "$tmp" \
              && mv -f -- "$tmp" "$dest"; then
              return 0
          fi
          # Never leave a partial document behind for the host to trip over.
          rm -f -- "$tmp"
          log "could not write $dest"
          return 1
      }

      # Non-authoritative, TRUSTED progress. The host may mirror it into its
      # event log but must never treat it as a terminal outcome.
      write_state() {
          local phase="$1" message="''${2-}"
          # ALLOC_TOKEN goes through the ENVIRONMENT, never through argv:
          # /proc/<pid>/cmdline is world-readable (0444), so `--arg
          # allocationToken <token>` would publish the active allocation token
          # to the untrusted worker (which shares the guest PID namespace) for
          # as long as this jq lives. /proc/<pid>/environ is 0400.
          ALLOC_TOKEN="$allocation_token" jq -n \
              --argjson version "$SPEC_VERSION" \
              --argjson controllerVersion "$CONTROLLER_VERSION" \
              --arg taskId "$task_id" \
              --arg slot "$slot" \
              --arg agent "$agent" \
              --arg phase "$phase" \
              --arg startedAt "$started_at" \
              --arg updatedAt "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
              --arg workerUnit "$worker_unit" \
              --arg message "$message" \
              '{version:$version, controllerVersion:$controllerVersion,
                taskId:$taskId, allocationToken:$ENV.ALLOC_TOKEN, slot:$slot,
                agent:$agent, phase:$phase, startedAt:$startedAt,
                updatedAt:$updatedAt, workerUnit:$workerUnit, message:$message}' \
              | write_controller_file "$STATE_FILE" || true
      }

      # THE authoritative result. Only ever a TERMINAL state, only ever written
      # here, only ever derived from what the controller itself observed.
      write_result() {
          local state="$1" exit_code="$2" timed_out="$3" message="''${4-}"
          # The token is passed in the ENVIRONMENT (see write_state).
          ALLOC_TOKEN="$allocation_token" jq -n \
              --argjson version "$SPEC_VERSION" \
              --argjson controllerVersion "$CONTROLLER_VERSION" \
              --arg taskId "$task_id" \
              --arg slot "$slot" \
              --arg agent "$agent" \
              --arg state "$state" \
              --argjson exitCode "$exit_code" \
              --arg startedAt "$started_at" \
              --arg finishedAt "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
              --argjson timedOut "$timed_out" \
              --arg message "$message" \
              '{version:$version, controllerVersion:$controllerVersion,
                taskId:$taskId, allocationToken:$ENV.ALLOC_TOKEN, slot:$slot,
                agent:$agent, state:$state, exitCode:$exitCode,
                startedAt:$startedAt, finishedAt:$finishedAt,
                timedOut:$timedOut, message:$message}' \
              | write_controller_file "$RESULT"
      }

      # Any problem that is NOT the agent's own failure is reported as
      # `infrastructure-error`, so the host can distinguish "the agent failed"
      # from "the job never ran".
      infra_fail() {
          log "infrastructure error: $*"
          write_result infrastructure-error 70 false "$*" || true
          exit 70
      }

      # A problem so early/severe that the result channel itself cannot be
      # trusted: do NOT write anything, fail loudly. The host then finds no
      # valid result and reports an infrastructure error itself.
      hard_fail() {
          log "REFUSING TO RUN: $*"
          exit 70
      }

      # --- inert without a job ---------------------------------------------
      # The unit also carries ConditionPathExists, so this is belt-and-braces
      # for a manual invocation.
      if [[ ! -e "$SPEC" ]]; then
          log "no job spec at $SPEC; nothing to do"
          exit 0
      fi

      # --- (a) the trust boundary must be intact BEFORE anything runs -------
      "$ASSERT_PATHS" || hard_fail "the job share does not have the required ownership/permissions"

      # --- (b) validate the immutable spec ---------------------------------
      jq -e . "$SPEC" >/dev/null 2>&1 || infra_fail "spec is not valid JSON"
      jq -e 'type == "object"' "$SPEC" >/dev/null 2>&1 || infra_fail "spec is not a JSON object"

      # --- (b1) IDENTITY first ----------------------------------------------
      # Everything the host needs to attribute a result (and therefore to
      # believe a reported infrastructure error at all) is parsed before any
      # other check, so a rejection further down still lands in a result
      # document the host can match to this allocation.
      version="$(jq -r '.version // empty' "$SPEC")"
      [[ "$version" == "$SPEC_VERSION" ]] \
          || infra_fail "unsupported spec version '$version' (expected $SPEC_VERSION)"

      task_id="$(jq -r '.taskId // empty' "$SPEC")"
      [[ "$task_id" =~ ^[a-zA-Z0-9._-]{1,64}$ ]] \
          || { task_id="unknown"; infra_fail "invalid taskId"; }

      allocation_token="$(jq -r '.allocationToken // empty' "$SPEC")"
      [[ "$allocation_token" =~ ^[0-9a-f]{32,128}$ ]] \
          || { allocation_token=""; infra_fail "missing or malformed allocationToken"; }

      spec_slot="$(jq -r '.slot // empty' "$SPEC")"
      [[ "$spec_slot" =~ ^[a-zA-Z0-9._-]{1,64}$ ]] \
          || infra_fail "invalid slot in spec"
      # The guest hostname IS the slot name (guest.nix), so a spec that claims a
      # different slot means host and guest disagree about which VM this is.
      [[ "$spec_slot" == "$slot" ]] \
          || infra_fail "spec slot '$spec_slot' does not match this guest ($slot)"

      agent="$(jq -r '.agent // empty' "$SPEC")"
      # The agent must be one this build declares as batch-capable. The
      # executable and argv are resolved from the registry INSIDE the worker,
      # never from the spec.
      case "$agent" in
      ${lib.concatMapStringsSep "\n" (n: "          ${n}) ;;") agentRegistry.batchNames}
          *)
              agent="''${agent//[^a-zA-Z0-9._-]/?}"
              infra_fail "agent '$agent' cannot run unattended (expected: ${agentRegistry.batchNamesAlternation})"
              ;;
      esac
      worker_unit="agent-job-worker@$agent.service"

      # --- (b2) the rest of the spec ----------------------------------------
      # Strict: no unknown fields. This also subsumes the older explicit
      # "spec must not contain an executable path" guard for
      # command/exec/executable, which stays as a NAMED rejection so the intent
      # is greppable and testable.
      if jq -e 'has("command") or has("exec") or has("executable")' "$SPEC" >/dev/null; then
          infra_fail "spec must not contain an executable path"
      fi
      unknown="$(jq -r --argjson allowed "$SPEC_KEYS" \
          '[keys[] | select(. as $k | $allowed | index($k) | not)] | join(",")' "$SPEC")"
      [[ -z "$unknown" ]] || infra_fail "spec carries unknown field(s): $unknown"

      workspace="$(jq -r '.workspace // empty' "$SPEC")"
      [[ "$workspace" == "$WORKSPACE" ]] \
          || infra_fail "workspace must be exactly $WORKSPACE"
      [[ "$workspace" == /* ]] || infra_fail "workspace must be an absolute path"
      [[ -d "$WORKSPACE" && ! -L "$WORKSPACE" ]] \
          || infra_fail "$WORKSPACE is not a directory"
      # The controller deliberately does NOT need write access to /workspace —
      # the WORKER does. Assert the worker can write there instead of testing
      # `-w` as root (which is always true and therefore proves nothing).
      ws_owner="$(stat -c %u -- "$WORKSPACE")"
      [[ "$ws_owner" == "$WORKER_UID" ]] \
          || infra_fail "$WORKSPACE must be owned by the worker uid $WORKER_UID (is $ws_owner)"

      prompt_path="$(jq -r '.promptFile // empty' "$SPEC")"
      # Exact match, not a prefix check: this rejects traversal
      # ("$JOB_DIR/../x"), symlink games and any path outside the job input dir.
      [[ "$prompt_path" == "$PROMPT_FILE" ]] \
          || infra_fail "promptFile must be exactly $PROMPT_FILE"
      [[ -r "$PROMPT_FILE" ]] || infra_fail "prompt file $PROMPT_FILE is not readable"

      timeout_s="$(jq -r '.timeoutSeconds // empty' "$SPEC")"
      [[ "$timeout_s" =~ ^[0-9]+$ ]] || infra_fail "timeoutSeconds must be a non-negative integer"
      (( timeout_s >= 1 )) || infra_fail "timeoutSeconds must be >= 1"
      (( timeout_s <= MAX_TIMEOUT )) \
          || infra_fail "timeoutSeconds exceeds the guest maximum $MAX_TIMEOUT"

      rclass="$(jq -r '.resourceClass // empty' "$SPEC")"
      [[ "$rclass" =~ ^[a-zA-Z0-9._-]{1,32}$ ]] || infra_fail "invalid resourceClass"

      jq -e '.persistAgentState | type == "boolean"' "$SPEC" >/dev/null \
          || infra_fail "persistAgentState must be a boolean"

      write_state validating

      # --- (c) prepare the worker's own (untrusted) area --------------------
      [[ -d "$WORKER_DIR" && ! -L "$WORKER_DIR" ]] \
          || infra_fail "the worker directory $WORKER_DIR is missing"
      # systemd (PID 1, root) opens the log files with `append:` BEFORE the
      # worker starts, and it FOLLOWS symlinks. They therefore live in a
      # ROOT-OWNED directory next to (never inside) the worker-writable one:
      # `worker/` is agent-owned, so anything running as the worker uid could
      # otherwise re-plant a symlink between this check and `systemctl start`
      # and redirect a root-opened append fd. With `worker-logs/` root-owned
      # under the root-owned 0755 share root there is no such window, and the
      # worker cannot truncate or replace its own logs mid-run either.
      if [[ -L "$WORKER_LOG_DIR" || ( -e "$WORKER_LOG_DIR" && ! -d "$WORKER_LOG_DIR" ) ]]; then
          log "replacing non-directory worker log path $WORKER_LOG_DIR"
          rm -f -- "$WORKER_LOG_DIR"
      fi
      # `install -d` also RESETS mode and ownership of an existing directory.
      install -d -m ${paths.workerLogsDirMode} -o 0 -g 0 -- "$WORKER_LOG_DIR" \
          || infra_fail "could not create the worker log directory $WORKER_LOG_DIR"
      log_dir_owner="$(stat -c %u -- "$WORKER_LOG_DIR")"
      [[ "$log_dir_owner" == "0" ]] \
          || infra_fail "the worker log directory must be root-owned (is uid $log_dir_owner)"
      # A fresh, root-owned file per job: `install` replaces whatever was there
      # (including a symlink) instead of writing through it.
      for f in "$WORKER_STDOUT" "$WORKER_STDERR"; do
          install -m ${paths.workerLogMode} -o 0 -g 0 /dev/null "$f" \
              || infra_fail "could not create the worker log file $f"
      done
      if [[ -L "$WORKER_ARTIFACTS" || ( -e "$WORKER_ARTIFACTS" && ! -d "$WORKER_ARTIFACTS" ) ]]; then
          rm -f -- "$WORKER_ARTIFACTS"
      fi
      [[ -d "$WORKER_ARTIFACTS" ]] \
          || install -d -m 0755 -o "$WORKER_UID" -g "$WORKER_GID" "$WORKER_ARTIFACTS"

      # --- (d) is this allocation already cancelled? -----------------------
      # A cancellation only ever applies to the allocation whose token it
      # carries, so a stale cancel file from an earlier task cannot stop this
      # one (and vice versa).
      cancel_requested() {
          local tok
          [[ -f "$CANCEL_FILE" && ! -L "$CANCEL_FILE" ]] || return 1
          tok="$(jq -r '.allocationToken // empty' "$CANCEL_FILE" 2>/dev/null)" || return 1
          [[ "$tok" == "$allocation_token" ]]
      }

      if cancel_requested; then
          log "cancellation already requested for this allocation; not starting the worker"
          write_state cancelling
          write_result cancelled 130 false "cancelled before the worker started"
          emit_event cancellation cancelled 130
          exit 0
      fi

      # --- (e) start the UNTRUSTED worker under its own identity -----------
      # A predeclared TEMPLATED unit, so every property (uid, cgroup limits,
      # sandboxing, the static timeout ceiling) is a build-time fact in
      # job.nix rather than something assembled at runtime. The instance name
      # is the registry-validated agent name — never a task id.
      systemctl reset-failed "$worker_unit" >/dev/null 2>&1 || true
      write_state starting-worker
      log "starting worker $worker_unit for task '$task_id' (timeout ''${timeout_s}s)"
      systemctl start --no-block "$worker_unit" \
          || infra_fail "could not start the worker unit $worker_unit"

      # A systemd property of the worker unit, or the empty string when it
      # cannot be read (the caller must never treat that as success).
      unit_prop() {
          systemctl show -P "$1" "$worker_unit" 2>/dev/null || true
      }

      # Stop the worker's WHOLE cgroup: SIGTERM, bounded grace, then SIGKILL of
      # whatever is left. Never "kill the initial pid" — a repository process
      # that double-forked must die too.
      stop_worker() {
          local waited=0
          systemctl stop --no-block "$worker_unit" >/dev/null 2>&1 || true
          while (( waited < WORKER_KILL_GRACE + 5 )); do
              case "$(unit_prop ActiveState)" in
                  inactive|failed|"") return 0 ;;
              esac
              sleep 1
              waited=$(( waited + 1 ))
          done
          log "worker $worker_unit still active after ''${waited}s; SIGKILLing its cgroup"
          systemctl kill --kill-whom=all --signal=SIGKILL "$worker_unit" >/dev/null 2>&1 || true
          systemctl stop "$worker_unit" >/dev/null 2>&1 || true
      }

      write_state running
      emit_event agent-started running

      # --- (f) supervise: deadline + cancellation + exit collection --------
      # The deadline is measured against the WALL CLOCK (bash's own `SECONDS`,
      # reset here), never by counting poll iterations: every iteration also
      # spends one or two `systemctl show` round-trips, so an iteration counter
      # under-counts real time by a few percent. At the 24h ceiling that is
      # tens of minutes — enough for the HOST deadline to fire first and take
      # the verdict away from the controller, which owns the timeout.
      SECONDS=0
      active_state=""
      # Set once the worker unit has actually been seen running, so a queued
      # start is never mistaken for a finished job.
      worker_seen=0
      while :; do
          active_state="$(unit_prop ActiveState)"
          case "$active_state" in
              activating|active|deactivating|reloading) worker_seen=1 ;;
          esac
          # Type=oneshot + RemainAfterExit: a finished worker is `active`
          # (SubState=exited) or `failed`, and its exit status stays readable
          # until the controller stops the unit.
          if [[ "$active_state" == "failed" ]]; then
              break
          fi
          if [[ "$active_state" == "active" && "$(unit_prop SubState)" == "exited" ]]; then
              break
          fi
          # Gone without ever reporting a status: treat as an infrastructure
          # error below, never as success. Only once the startup window is over
          # (or the worker was seen running and then vanished).
          if [[ -z "$active_state" || "$active_state" == "inactive" ]] \
              && (( worker_seen || SECONDS >= WORKER_STARTUP_GRACE )); then
              break
          fi
          if cancel_requested; then
              log "cancellation requested for this allocation; stopping the worker"
              controller_verdict=cancelled
              write_state cancelling
              stop_worker
              break
          fi
          # Only a worker that was actually seen running can TIME OUT; one that
          # never started falls through to the startup-grace branch above and
          # becomes an infrastructure error instead.
          if (( worker_seen && SECONDS >= timeout_s )); then
              log "worker exceeded its ''${timeout_s}s deadline (''${SECONDS}s elapsed); stopping its cgroup"
              controller_verdict=timed-out
              write_state timing-out
              stop_worker
              break
          fi
          sleep "$POLL_INTERVAL"
      done

      # --- (g) derive the outcome from what WE observed ---------------------
      unit_result="$(unit_prop Result)"
      exec_code="$(unit_prop ExecMainCode)"
      exec_status="$(unit_prop ExecMainStatus)"
      active_state="$(unit_prop ActiveState)"
      log "worker $worker_unit: ActiveState=$active_state Result=$unit_result ExecMainCode=$exec_code ExecMainStatus=$exec_status"

      state=""
      rc=70
      timed_out=false
      message=""
      case "$controller_verdict" in
          timed-out)
              state=timed-out; rc=124; timed_out=true
              message="the controller stopped the worker after ''${timeout_s}s"
              ;;
          cancelled)
              state=cancelled; rc=130
              message="cancelled by the operator"
              ;;
          *)
              case "$unit_result" in
                  timeout)
                      # The unit's own static ceiling fired.
                      state=timed-out; rc=124; timed_out=true
                      message="the worker unit hit its static runtime ceiling"
                      ;;
                  oom-kill)
                      state=failed; rc=137
                      message="the worker was killed by the cgroup OOM killer"
                      ;;
                  *)
                      case "$exec_code" in
                          1) # exited normally
                              if [[ "$exec_status" =~ ^[0-9]+$ ]]; then
                                  rc="$exec_status"
                                  if (( rc == 0 )); then state=completed; else state=failed; fi
                              else
                                  state=infrastructure-error
                                  message="the worker's exit status could not be read"
                              fi
                              ;;
                          2) # killed by a signal
                              state=failed
                              if [[ "$exec_status" =~ ^[0-9]+$ ]]; then
                                  rc=$(( 128 + exec_status ))
                              else
                                  rc=137
                              fi
                              message="the worker was killed by a signal"
                              ;;
                          *)
                              state=infrastructure-error
                              message="the worker never reported an exit status (result=''${unit_result:-unknown})"
                              ;;
                      esac
                      ;;
              esac
              ;;
      esac

      # Release the unit (RemainAfterExit keeps it around) and make sure no
      # descendant of the worker survives into the next phase.
      stop_worker
      systemctl reset-failed "$worker_unit" >/dev/null 2>&1 || true

      log "task '$task_id' finished: state=$state exit=$rc"
      write_state finished "$state"
      write_result "$state" "$rc" "$timed_out" "$message" \
          || hard_fail "could not write the authoritative result"
      case "$state" in
          timed-out) emit_event timeout "$state" "$rc" ;;
          cancelled) emit_event cancellation "$state" "$rc" ;;
          *)         emit_event agent-finished "$state" "$rc" ;;
      esac

      # Exit 0 for a *reported* agent failure: the authoritative outcome is
      # result.json, and a failed unit would only add noise (and, with
      # microvm.nix's Restart=always on the VM, confusion). Infrastructure
      # errors above DO exit non-zero.
      exit 0
    '';
    meta = with lib; {
      description = "Trusted guest-side batch job controller for myconfig.ai.microvm sandboxes";
      platforms = platforms.linux;
    };
  };

  # ======================================================================
  # (4) HOST-side result VERIFIER (the ONE result parser)
  # ======================================================================
  # The host must treat the result as untrusted input even though only the
  # guest controller can write it: ownership separation is a control, not a
  # proof, and a bug on either side must fail CLOSED.
  #
  # The EXPECTED allocation token is read from the environment variable
  # AGENT_JOB_EXPECTED_TOKEN, never from argv: /proc/<pid>/cmdline is
  # world-readable 0444 (while /proc/<pid>/environ is 0400), so a `--token`
  # argument would publish the active allocation token to every local process
  # for the lifetime of each check — and the host polls this verifier every few
  # seconds for the whole runtime of a job.
  #
  # Exit codes:
  #   0   the document is valid AND belongs to the active allocation; the
  #       canonical (compact) JSON is printed on stdout
  #   1   nothing to read yet (the file does not exist)
  #   2   REJECTED — the reason is printed on stderr; callers must treat this
  #       as an infrastructure/protocol error, never as a result
  #   64  usage error (a caller-side bug, NEVER evidence about the guest)
  resultSchema = pkgs.writeText "agent-job-result-schema.jq" ''
    # Validate a controller-written job document against the ACTIVE allocation.
    # Emits "ok" or "reject: <first reason>".
    def isoTs: type == "string"
      and test("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$");
    def isInt: type == "number" and (. == floor);

    def terminalStates: ${builtins.toJSON paths.terminalStates};
    def phases: ${builtins.toJSON paths.phases};
    def commonKeys: ["version","controllerVersion","taskId","allocationToken",
                     "slot","agent","message"];
    def resultKeys: commonKeys + ["state","exitCode","startedAt","finishedAt","timedOut"];
    def stateKeys:  commonKeys + ["phase","startedAt","updatedAt","workerUnit"];

    def identityErrors:
      [ if (.version | isInt | not) or .version != $expVersion
        then "schema version mismatch (expected \($expVersion))" else empty end
      , if (.controllerVersion | isInt | not) or .controllerVersion != $expController
        then "controller version mismatch (expected \($expController))" else empty end
      , if (.taskId | type) != "string" or .taskId != $expTask
        then "task id does not belong to the active allocation" else empty end
      , if (.allocationToken | type) != "string" or .allocationToken != $ENV.EXPECTED_TOKEN
        then "allocation token does not belong to the active allocation" else empty end
      , if (.slot | type) != "string" or .slot != $expSlot
        then "slot does not belong to the active allocation" else empty end
      , if (.agent | type) != "string" or .agent != $expAgent
        then "agent does not belong to the active allocation" else empty end
      ];

    def resultErrors:
      # NOTE: bind the value BEFORE `index()` — `terminalStates | index(.)` would
      # search the array for ITSELF (jq treats an array argument as a
      # subsequence search) and therefore always match.
      [ (.state as $s
         | if ($s | type) != "string" then "state is not a string"
           elif (terminalStates | index($s)) == null
           then "state is not a terminal state" else empty end)
      , if (.exitCode | isInt | not) then "exitCode is not an integer"
        elif .exitCode < 0 or .exitCode > 255 then "exitCode out of range"
        else empty end
      , if (.timedOut | type) != "boolean" then "timedOut is not a boolean" else empty end
      , if (.startedAt | isoTs | not) then "startedAt is not an ISO-8601 UTC timestamp" else empty end
      , if (.finishedAt | isoTs | not) then "finishedAt is not an ISO-8601 UTC timestamp" else empty end
      , if .state == "timed-out" and .timedOut != true
        then "state/timedOut disagree" else empty end
      , if .state != "timed-out" and .timedOut == true
        then "state/timedOut disagree" else empty end
      , if .state == "completed" and .exitCode != 0
        then "a completed job must report exitCode 0" else empty end
      , if .state != "completed" and .exitCode == 0
        then "a non-completed job must not report exitCode 0" else empty end
      ];

    def stateErrors:
      [ (.phase as $p
         | if ($p | type) != "string" then "phase is not a string"
           elif (phases | index($p)) == null
           then "phase is not a known controller phase" else empty end)
      , if (.startedAt | isoTs | not) then "startedAt is not an ISO-8601 UTC timestamp" else empty end
      , if (.updatedAt | isoTs | not) then "updatedAt is not an ISO-8601 UTC timestamp" else empty end
      ];

    def unknownKeys(allowed): [keys[] | select(. as $k | allowed | index($k) | not)];
    def missingKeys(required): . as $doc | [required[] | select(. as $k | ($doc | has($k)) | not)];

    def errors:
      if type != "object" then ["document is not a JSON object"]
      else
        (if $kind == "result" then resultKeys else stateKeys end) as $allowed
        # Structural problems first: identity comparisons on a document with
        # missing or extra fields would only produce confusing reasons.
        | [ (unknownKeys($allowed) | if length > 0 then "unknown field(s): \(join(","))" else empty end)
          , (missingKeys($allowed - ["message","workerUnit"])
             | if length > 0 then "missing required field(s): \(join(","))" else empty end)
          ] as $structural
        | if ($structural | length) > 0 then $structural
          else
            identityErrors
            + (if $kind == "result" then resultErrors else stateErrors end)
          end
      end;

    errors as $e
    | if ($e | length) == 0 then "ok" else "reject: " + $e[0] end
  '';

  agent-job-verify-result = pkgs.writeShellApplication {
    name = "agent-job-verify-result";
    runtimeInputs = with pkgs; [
      coreutils
      jq
    ];
    text = ''
      set -euo pipefail

      readonly SCHEMA=${resultSchema}
      readonly EXP_VERSION=${toString paths.specVersion}
      readonly EXP_CONTROLLER=${toString paths.controllerVersion}
      # A result document is a handful of scalars; anything larger is junk we
      # refuse to parse.
      readonly MAX_BYTES=65536

      readonly PROG="agent-job-verify-result"
      reject() { printf '%s: reject: %s\n' "$PROG" "$*" >&2; exit 2; }
      usage_error() { printf '%s: %s\n' "$PROG" "$*" >&2; exit 64; }

      path=""
      kind="result"
      task=""
      # The one input that must NOT be an argument (see the header).
      token="''${AGENT_JOB_EXPECTED_TOKEN-}"
      slot=""
      agent=""
      while [[ $# -gt 0 ]]; do
          case "$1" in
              --result) path="''${2-}"; shift 2 ;;
              --kind)   kind="''${2-}"; shift 2 ;;
              --task)   task="''${2-}"; shift 2 ;;
              --slot)   slot="''${2-}"; shift 2 ;;
              --agent)  agent="''${2-}"; shift 2 ;;
              --token)
                  usage_error "the expected allocation token must be passed in the environment (AGENT_JOB_EXPECTED_TOKEN), never as a --token argument (/proc/<pid>/cmdline is world-readable)"
                  ;;
              *) usage_error "unknown argument '$1'" ;;
          esac
      done

      [[ -n "$path" ]] || usage_error "--result <path> is required"
      [[ "$path" == /* ]] || usage_error "--result must be an absolute path"
      case "$kind" in
          result|state) ;;
          *) usage_error "--kind must be 'result' or 'state'" ;;
      esac
      # The EXPECTED identity comes from the host's own allocation marker, so
      # constrain it here too: a malformed expectation would silently weaken
      # every comparison below.
      [[ "$task"  =~ ^[a-zA-Z0-9._-]{1,64}$ ]] || usage_error "--task is missing or malformed"
      [[ "$token" =~ ^[0-9a-f]{32,128}$ ]] \
          || usage_error "AGENT_JOB_EXPECTED_TOKEN is missing or malformed"
      [[ "$slot"  =~ ^[a-zA-Z0-9._-]{1,64}$ ]] || usage_error "--slot is missing or malformed"
      [[ "$agent" =~ ^[a-z][a-z0-9-]{0,32}$ ]] || usage_error "--agent is missing or malformed"

      # --- file-system level: WHO could have written this? -----------------
      # A symlink would mean the path we open is not the path we authorised.
      [[ ! -L "$path" ]] || reject "the result path is a symlink: $path"
      if [[ ! -e "$path" ]]; then
          exit 1
      fi
      [[ -f "$path" ]] || reject "the result path is not a regular file: $path"

      owner="$(stat -c %u -- "$path")"
      [[ "$owner" == 0 ]] || reject "the result is owned by uid $owner, not by the guest controller (uid 0)"
      mode="$(printf '%d' "0$(stat -c %a -- "$path")")"
      (( (mode & 0022) == 0 )) \
          || reject "the result is group/other-writable (mode $(stat -c %a -- "$path"))"

      parent="''${path%/*}"
      [[ ! -L "$parent" ]] || reject "the controller directory is a symlink: $parent"
      [[ -d "$parent" ]] || reject "the controller directory is missing: $parent"
      powner="$(stat -c %u -- "$parent")"
      [[ "$powner" == 0 ]] || reject "the controller directory is owned by uid $powner, not 0"
      pmode="$(printf '%d' "0$(stat -c %a -- "$parent")")"
      (( (pmode & 0022) == 0 )) \
          || reject "the controller directory is group/other-writable (mode $(stat -c %a -- "$parent"))"

      size="$(stat -c %s -- "$path")"
      (( size > 0 )) || reject "the result file is empty"
      (( size <= MAX_BYTES )) || reject "the result file is larger than $MAX_BYTES bytes ($size)"

      # --- content level: strict parse + identity + schema ------------------
      # Read the document EXACTLY ONCE. Everything below validates this byte
      # string, and the very same byte string is what the caller archives:
      # re-opening the path between the checks and the output would leave a
      # window in which the archived bytes were never validated.
      doc="$(cat -- "$path")" || reject "the result could not be read"
      [[ -n "$doc" ]] || reject "the result file is empty"
      jq -e . <<< "$doc" >/dev/null 2>&1 || reject "the result is not valid JSON"
      # The expected TOKEN is passed to jq in the environment for the same
      # reason the launcher passes it to us that way (world-readable argv).
      verdict="$(EXPECTED_TOKEN="$token" jq -r \
          --argjson expVersion "$EXP_VERSION" \
          --argjson expController "$EXP_CONTROLLER" \
          --arg expTask "$task" \
          --arg expSlot "$slot" \
          --arg expAgent "$agent" \
          --arg kind "$kind" \
          -f "$SCHEMA" <<< "$doc")" \
          || reject "the result could not be validated"
      case "$verdict" in
          ok) ;;
          reject:*) reject "''${verdict#reject: }" ;;
          *) reject "the validator produced no verdict" ;;
      esac

      # Canonical, compact form of the bytes we just validated — the caller
      # archives THIS, and never re-reads the file.
      jq -c . <<< "$doc"
    '';
    meta = with lib; {
      description = "Validate a myconfig.ai.microvm batch result against the active allocation";
      platforms = platforms.linux;
    };
  };

  # --- guest-side NixOS module fragment ---------------------------------
  # Merged into every slot's guest config by guest.nix, next to the workspace /
  # hostkey shares. Takes the SLOT because the resource limits below are derived
  # from the slot's resource class (ticket 5 C).
  #
  # EMPTY without the `batch` capability (lightweight plan phase 5): an
  # interactive-only guest then contains no controller unit, no worker template
  # and none of the three job-protocol programs — they are removed from the
  # closure, not merely left unstarted.
  mkGuestModule = slot: lib.optionalAttrs agentCapabilities.batch (mkBatchGuestModule slot);

  # The non-login batch WORKER never sources /etc/profile, so guest.nix has to
  # give it the SAME model-endpoint environment the interactive login shell gets
  # (see its `modelEndpointEnv`). Rendered HERE so "is there a worker unit at
  # all?" is decided in exactly one place.
  mkWorkerEnvironmentModule =
    env:
    lib.optionalAttrs agentCapabilities.batch {
      systemd.services."${paths.workerUnitTemplate}".environment = env;
    };

  mkBatchGuestModule = slot: {
    environment.systemPackages = [
      agent-job-controller
      agent-job-worker
      agent-job-assert-paths
    ];

    # ---- TRUSTED controller (guest root) --------------------------------
    systemd.services.agent-job-controller = {
      description = "Trusted unattended agent batch job controller (myconfig.ai.microvm)";
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
      after = [
        "network-online.target"
        # It talks to PID 1 over D-Bus to start/stop/inspect the worker unit.
        "dbus.service"
      ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = lib.getExe agent-job-controller;
        # Deliberately root: it must start the worker under a DIFFERENT uid and
        # own a directory the worker cannot touch. It never executes anything
        # the spec, the repository or the agent supplied.
        NoNewPrivileges = true;
        PrivateDevices = true;
        PrivateTmp = true;
        # The controller has no business in the worker's home.
        ProtectHome = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        RestrictSUIDSGID = true;
        # The immutable input is read-only even for the controller, and the only
        # part of the share it may write is its OWN directory (plus the worker's
        # log/artifact area it has to prepare). `-` tolerates a slot whose share
        # predates this layout.
        ReadOnlyPaths = [ "-${paths.guestInputDir}" ];
        ReadWritePaths = [
          "-${paths.guestControllerDir}"
          "-${paths.guestWorkerDir}"
          "-${paths.guestWorkerLogsDir}"
        ];
        # The controller's own lifecycle events must be correlatable with the
        # host's, and the guest journal dies with the (ephemeral) guest. Sending
        # them to the CONSOLE too gets them into the host journal, because
        # microvm.nix captures the serial console of `microvm@<slot>.service`.
        StandardOutput = "journal+console";
        StandardError = "journal+console";
        # NOTE: no ProtectSystem= here. It would need a verified list of
        # writable paths for the D-Bus socket and systemd's runtime dirs, which
        # cannot be validated without booting on real KVM; the controller is a
        # small, fixed, non-repository-driven script instead.
        #
        # Static ceiling for the CONTROLLER itself: the worker's own ceiling
        # plus room for the stop grace and the result write. `Type=oneshot`
        # IGNORES RuntimeMaxSec and defaults TimeoutStartSec to infinity, so the
        # ceiling has to be TimeoutStartSec — otherwise a wedged systemctl /
        # D-Bus call would leave this unit activating forever.
        TimeoutStartSec = workerCeilingSeconds + jobCfg.gracePeriodSeconds;
        TimeoutStopSec = 30;
      };
    };

    # ---- UNTRUSTED worker (the guest agent user) ------------------------
    # Predeclared TEMPLATE, started ONLY by the controller with the
    # registry-validated agent name as the instance. Not `wantedBy` anything,
    # so it never starts on its own.
    systemd.services."${paths.workerUnitTemplate}" = {
      description = "Unattended agent batch worker (UNTRUSTED; agent %i)";
      unitConfig = {
        RequiresMountsFor = "/workspace ${paths.guestMountPoint}";
      };
      # The agent binaries plus the worker's own tools. Explicit, so the unit
      # does not depend on the ambient systemd PATH.
      path = workerPackages;
      serviceConfig = {
        # oneshot + RemainAfterExit: the unit stays loaded after the worker
        # exits, so the CONTROLLER can still read ExecMainCode/ExecMainStatus
        # (its only source of truth for the outcome).
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${lib.getExe agent-job-worker} %i";
        # §6/ticket 4: the agent runs as the SAME unprivileged guest user as the
        # interactive session, in the workspace, with no way to gain privileges.
        User = paths.workerUser;
        Group = paths.workerGroup;
        WorkingDirectory = "/workspace";
        # Untrusted output, in the worker's own area.
        StandardOutput = "append:${paths.guestWorkerStdout}";
        StandardError = "append:${paths.guestWorkerStderr}";
        NoNewPrivileges = true;
        PrivateDevices = true;
        PrivateTmp = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        RestrictSUIDSGID = true;
        # /usr, /boot, /efi and /etc read-only. The agent writes to /workspace,
        # its home and /tmp, none of which this affects.
        ProtectSystem = "full";
        # SECOND layer under the 0700 ownership of the controller directory: the
        # worker's mount namespace does not even contain it.
        InaccessiblePaths = [ "-${paths.guestControllerDir}" ];
        # THIRD layer under "the worker must not learn the allocation token":
        # /proc/<pid>/cmdline of ANY process is world-readable (0444) and the
        # worker shares the guest PID namespace, so hide every process the
        # worker does not own — which is every process of the trusted, root-run
        # controller. Neither side puts the token in an argv any more (see
        # write_state/write_result), but a future helper that did must not
        # immediately hand it to the worker.
        #
        # `ProcSubset=pid` is deliberately NOT set: it would also hide
        # /proc/cpuinfo, /proc/meminfo and friends, which the node-based coding
        # agents read (e.g. os.cpus()), and it adds nothing here — the token
        # could only ever appear in a ROOT-owned process's cmdline, which
        # ProtectProc=invisible already hides.
        ProtectProc = "invisible";
        # NOTE: `${paths.guestWorkerLogsDir}` is deliberately NOT listed here.
        # Its files are root:root ${paths.workerLogMode} in a root-owned
        # directory, so DAC already denies the worker every write; marking the
        # path read-only in the unit's mount namespace would additionally risk
        # the `append:` open itself, which systemd performs for this unit.
        ReadOnlyPaths = [ "-${paths.guestInputDir}" ];
        ReadWritePaths = [
          "-${paths.guestWorkerDir}"
          "-/workspace"
        ];
        # STATIC ceiling on top of the controller's own deadline: even a worker
        # the controller somehow stopped supervising cannot run longer than
        # this. (`Type=oneshot` ignores RuntimeMaxSec, hence TimeoutStartSec.)
        TimeoutStartSec = workerCeilingSeconds;
        # Grace between SIGTERM and SIGKILL for the WHOLE cgroup, so the agent
        # can still flush partial work to /workspace.
        TimeoutStopSec = workerKillGraceSeconds;
        # A repository process that double-forked must die with the job: the
        # controller stops a CGROUP, not a pid.
        KillMode = "control-group";
        KillSignal = "SIGTERM";
        # One worker per allocation; never restarted behind the controller's
        # back.
        Restart = "no";

        # --- ticket 5 C: guest-side resource limits, sized by the CLASS ------
        # A runaway agent (fork bomb, memory hog, endless build) must not take
        # the whole guest down before the host's timeout fires. Everything is
        # derived from the slot's class, so a `small` slot really is small.
        #
        # Memory: leave headroom for the guest kernel, the tmpfs root and
        # sshd/systemd, so hitting the limit kills the AGENT (cgroup OOM) rather
        # than wedging the whole VM.
        MemoryMax = "${
          toString (lib.max (slot.memoryMiB - jobCfg.guestMemoryHeadroomMiB) (slot.memoryMiB / 2))
        }M";
        # CPUQuota is expressed in "percent of ONE cpu", so a 4-vCPU class maps
        # to 400% — i.e. the job may use its slot's cpus, not more.
        CPUQuota = "${toString (slot.vcpu * 100)}%";
        # Bound process/thread explosions (default is ~15% of the kernel pid max).
        TasksMax = jobCfg.tasksMax;
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
        re-validated by the guest controller, and used for the guest worker
        unit's static `TimeoutStartSec` ceiling.
      '';
    };

    tasksMax = mkOption {
      type = types.ints.positive;
      default = 4096;
      description = ''
        `TasksMax` for the guest batch worker: an upper bound on processes and
        threads, so a fork bomb inside the sandbox cannot exhaust the guest's
        pid space. Generous enough for compilers and test runners.
      '';
    };

    guestMemoryHeadroomMiB = mkOption {
      type = types.ints.positive;
      default = 512;
      description = ''
        RAM (MiB) subtracted from a slot's class memory to obtain the batch
        worker's `MemoryMax`. The headroom is what the guest kernel, the tmpfs
        root, systemd and sshd need, so that an out-of-memory AGENT is killed by
        its cgroup instead of wedging the whole guest. Never more than half the
        class memory is subtracted.
      '';
    };

    gracePeriodSeconds = mkOption {
      type = types.ints.positive;
      default = 120;
      description = ''
        Extra time the HOST waits beyond a job's own timeout before it
        force-stops the VM, so the guest controller can finish writing its
        authoritative `controller/result.json` after the worker was stopped.
      '';
    };
  };

  config = lib.mkMerge [
    # Path/format definitions + the guest module fragment, exported for
    # guest.nix (shares + services) and launcher.nix (submit/status/recover).
    {
      _module.args.agentJobs = paths // {
        inherit mkGuestModule mkWorkerEnvironmentModule;
        controller = agent-job-controller;
        worker = agent-job-worker;
        assertPaths = agent-job-assert-paths;
        resultVerifier = agent-job-verify-result;
        inherit workerKillGraceSeconds workerCeilingSeconds;
      };
    }

    (lib.mkIf cfg.enable {
      assertions = [
        {
          assertion = cfg.job.defaultTimeoutSeconds <= cfg.job.maxTimeoutSeconds;
          message = "myconfig.ai.microvm.job.defaultTimeoutSeconds must be <= job.maxTimeoutSeconds.";
        }
        {
          # The whole trust split collapses if the worker is root.
          assertion = paths.workerUid != 0;
          message = "myconfig.ai.microvm: the batch worker uid (guestAgentUid) must never be 0.";
        }
      ];

    })

    # The host-side RESULT ARCHIVE exists only for the `batch` capability: it is
    # where `submit` keeps a finished job's verdict after the slot is released,
    # and an interactive-only host never produces one.
    (lib.mkIf (cfg.enable && agentCapabilities.batch) {
      # The per-slot job directories live in the session tree, which
      # ./session.nix creates from the ONE layout table (including the two
      # bind-mount points and the MODES that ARE the trust boundary), so
      # emitting them here as well would only duplicate the same rules. The
      # host-only RESULT ARCHIVE is never part of any share and stays here.
      systemd.tmpfiles.rules = [
        # 0700: an archived result carries the allocation token of the run it
        # belongs to, so it is root-only — not world-readable.
        "d ${paths.resultsDir} 0700 root root - -"
        # Migration: archives written before the mode was tightened are 0644.
        "z ${paths.resultsDir}/*.json 0600 root root - -"
      ];
    })
  ];
}
