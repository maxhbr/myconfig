# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.microvm — host launcher, slot allocator, task/repo validation,
# standalone-clone creation and bind-mount lifecycle (plan §20–§28, §34–§35).
#
# This phase builds the `agent-microvm` host command
# (run/stop/status/ssh/console/destroy/list/workspace-remove) as a
# `pkgs.writeShellApplication` (so shellcheck runs at build time) and installs
# it into the host environment ONLY when the feature is enabled. Everything is
# gated behind `lib.mkIf cfg.enable`, so a disabled feature (the secure
# default) produces zero config side effects.
#
#   §20  A Nix package `agent-microvm` with strict bash (`set -euo pipefail`,
#        no `eval`, all paths quoted), `flock`-based slot allocation.
#   §21  Global allocator lock (/run/agent-microvms/allocator.lock) + per-slot
#        locks; select an unused slot; no double-allocation; clean up on
#        exit/interrupt via `trap` (keeping the workspace clone).
#   §22  Strict task-name validation ([a-zA-Z0-9._-], no `/`, `..`,
#        whitespace, metachars, control chars, absolute paths, leading `-`,
#        over-long). Used only as a dir/metadata label, NEVER in Nix.
#   §23  Source-repo validation via `git rev-parse --show-toplevel` +
#        `realpath`; rejects non-repos, `/`, host home, the runtime/workspace
#        roots, symlink escapes and existing microVM workspaces. $PWD is not
#        trusted (the given --repository is canonicalised).
#   §24  Standalone clone under `workspaceRoot/<task>` via
#        `git clone --no-local`; verifies git-dir + git-common-dir resolve
#        INSIDE the workspace.
#   §26  Bind-mount lifecycle: mkdir `stateRoot/<slot>/workspace`,
#        `mount --bind`, `findmnt` verify; cleanup unmounts, removes slot
#        transient files, releases the lock, but never deletes the clone.
#        A separate, guarded `workspace-remove` op deletes the clone.
#   §27  VM lifecycle strictly via `microvm@<slot>.service` (systemctl), with
#        a bounded clean-shutdown timeout — no untracked background CH.
#   §34  `status` reports slot / service state / IP / MAC / task / workspace /
#        bind-mount status / agent / start time / SSH-guest readiness / lock
#        owner — never secrets.
#   §35  stop vs destroy vs workspace-remove: stop/destroy keep the workspace,
#        git and patches; workspace-remove is separate and guarded.
#
# The slot table is generated from the SAME data as slots.nix (the shared
# single source of truth), so the host launcher and the guest VMs always agree
# on names / IPs / MACs / TAPs.
{
  config,
  lib,
  pkgs,
  myconfig,
  # The ONE authoritative supported-agent registry instance, built in
  # default.nix (`_module.args.agentRegistry`). `--agent` validation and the
  # help output below are GENERATED from it, so the host launcher can never
  # drift from the guest's dispatch table. See ./agents.nix.
  agentRegistry,
  # The ONE definition of the per-slot SSH host-key paths, from hostkeys.nix
  # (`_module.args.agentHostKeys`). Supplies the known_hosts file the launcher
  # verifies guests against.
  agentHostKeys,
  # The ONE definition of the batch-job format/paths (job.nix). `submit` writes
  # the spec + prompt there and reads the guest's result from it.
  agentJobs,
  ...
}:
let
  cfg = config.myconfig.ai.microvm;

  slots = (import ./slots.nix { inherit lib; }).mkSlots cfg.slotCount;

  # Render the deterministic slot table as bash arrays. Using the shared slot
  # helper guarantees the launcher sees exactly the names/IPs/MACs/TAPs that
  # guest.nix builds and network.nix wires up.
  bashList = field: lib.concatMapStringsSep " " (s: lib.escapeShellArg (toString (s.${field}))) slots;

  agent-microvm = pkgs.writeShellApplication {
    name = "agent-microvm";
    runtimeInputs = with pkgs; [
      coreutils
      util-linux # flock, mount, umount, findmnt
      git
      jq
      openssh
      systemd # systemctl, systemd-escape
      gnugrep
      gnused
    ];
    text = ''
      set -euo pipefail

      # ==== deterministic slot table (generated from slots.nix) ============
      readonly SLOT_NAMES=(${bashList "name"})
      readonly SLOT_IPS=(${bashList "ip"})
      readonly SLOT_MACS=(${bashList "mac"})
      readonly SLOT_TAPS=(${bashList "tap"})
      # RESERVED per-slot AF_VSOCK context ids (ticket 3 B). Reported by
      # `status` so the operator can see the slot's control-channel identity;
      # not yet used as a transport (see slots.nix `cid`).
      readonly SLOT_CIDS=(${bashList "cid"})

      # ==== configuration (from the Nix module options) ====================
      readonly WORKSPACE_ROOT=${lib.escapeShellArg cfg.workspaceRoot}
      readonly RUNTIME_ROOT=${lib.escapeShellArg cfg.runtimeRoot}
      readonly STATE_ROOT=${lib.escapeShellArg cfg.stateRoot}
      readonly SSH_ENABLED=${lib.escapeShellArg (if cfg.enableSsh then "1" else "0")}
      readonly SSH_USER="agent"
      # ---- §18 / ticket 3 B: AUTHENTICATED control channel ---------------
      # Every slot has a STABLE ed25519 host identity, provisioned on the host
      # by `agent-microvm-hostkeys.service` and handed to exactly that slot
      # through a read-only virtiofs share (hostkeys.nix / guest.nix). Their
      # public keys are aggregated, keyed by slot IP, in this world-readable
      # known_hosts file, so every ssh invocation below can run with
      # StrictHostKeyChecking=yes instead of the previous
      # `StrictHostKeyChecking=no` + /dev/null known-hosts (which accepted ANY
      # key, i.e. an unauthenticated channel).
      readonly KNOWN_HOSTS=${lib.escapeShellArg agentHostKeys.knownHosts}
      # Strict verification against exactly that file. Kept in ONE place so
      # readiness probing, `--attach` and `ssh` cannot drift apart.
      SSH_VERIFY_OPTS=(
          -o StrictHostKeyChecking=yes
          -o UserKnownHostsFile="$KNOWN_HOSTS"
          -o GlobalKnownHostsFile=/dev/null
          -o HashKnownHosts=no
      )
      readonly SSH_VERIFY_OPTS
      # Stable dest of the agenix-decrypted dedicated private key. MUST match
      # `myconfig.secrets."dedicated-agent-vm-key".dest` in secrets.nix. The
      # priv repo provisions its `source`; agenix then decrypts it here
      # (root:root 0400). Default AGENT_MICROVM_SSH_KEY to it when the caller
      # set none AND it is readable — so `run --attach` / `ssh` under `sudo`
      # (which strips a user-set AGENT_MICROVM_SSH_KEY via env_reset) still
      # find the dedicated key. A NON-root operator cannot read this 0400
      # key, so the check below leaves AGENT_MICROVM_SSH_KEY unset and ssh
      # falls back to the operator's own ~/.ssh identity — which the guest
      # authorises when passwordlessControl is on (see guest.nix), so
      # non-root `agent-microvm ssh` works without this key. If the secret is
      # unprovisioned the file is absent and we likewise leave it unset.
      readonly SSH_KEY_DEST="/run/agenix/dedicated-agent-vm-key"
      if [[ -z "''${AGENT_MICROVM_SSH_KEY:-}" && -r "$SSH_KEY_DEST" ]]; then
          AGENT_MICROVM_SSH_KEY="$SSH_KEY_DEST"
          export AGENT_MICROVM_SSH_KEY
      fi
      # §11 UID/GID ownership: the guest `agent` user is uid/gid 1000
      # (guest.nix `users.users.agent`). The workspace clone is chowned to
      # these numeric ids so it appears agent-owned inside the guest via
      # virtiofs (which passes ownership through unchanged). Keep in sync with
      # guest.nix's `uid = 1000`.
      readonly GUEST_AGENT_UID=1000
      readonly GUEST_AGENT_GID=1000
      # ---- batch jobs (ticket 4) -----------------------------------------
      readonly JOBS_ROOT=${lib.escapeShellArg agentJobs.root}
      readonly RESULTS_DIR=${lib.escapeShellArg agentJobs.resultsDir}
      readonly JOB_SPEC_NAME=${lib.escapeShellArg agentJobs.specName}
      readonly JOB_PROMPT_NAME=${lib.escapeShellArg agentJobs.promptName}
      readonly JOB_RESULT_NAME=${lib.escapeShellArg agentJobs.resultName}
      readonly JOB_SPEC_VERSION=${toString agentJobs.specVersion}
      # Guest-side prompt path, as it must appear in spec.json (the guest
      # validates it by EXACT match against its own mount point).
      readonly GUEST_PROMPT=${lib.escapeShellArg agentJobs.guestPrompt}
      readonly GUEST_WORKSPACE=/workspace
      readonly JOB_DEFAULT_TIMEOUT=${toString cfg.job.defaultTimeoutSeconds}
      readonly JOB_MAX_TIMEOUT=${toString cfg.job.maxTimeoutSeconds}
      readonly JOB_GRACE=${toString cfg.job.gracePeriodSeconds}
      readonly RUN_DIR="/run/agent-microvms"
      readonly ALLOC_LOCK="$RUN_DIR/allocator.lock"
      readonly SLOTS_DIR="$RUNTIME_ROOT/slots"
      # Bounded clean-shutdown window (seconds) before we force-kill the unit.
      readonly SHUTDOWN_TIMEOUT=30
      # Bounded guest-readiness window (seconds) when waiting for SSH.
      readonly READY_TIMEOUT=90

      PROG="agent-microvm"

      die() {
          printf '%s: error: %s\n' "$PROG" "$*" >&2
          exit 1
      }
      log() { printf '%s: %s\n' "$PROG" "$*" >&2; }

      require_root() {
          if [[ "$(id -u)" -ne 0 ]]; then
              die "'$1' must run as root (mounts + systemctl); re-run with sudo"
          fi
      }

      # ---- slot table helpers --------------------------------------------
      slot_index() {
          local want="$1" i
          for i in "''${!SLOT_NAMES[@]}"; do
              if [[ "''${SLOT_NAMES[$i]}" == "$want" ]]; then
                  printf '%s' "$i"
                  return 0
              fi
          done
          return 1
      }
      is_slot_name() { slot_index "$1" >/dev/null 2>&1; }
      slot_ip()  { local i; i="$(slot_index "$1")" || return 1; printf '%s' "''${SLOT_IPS[$i]}"; }
      slot_mac() { local i; i="$(slot_index "$1")" || return 1; printf '%s' "''${SLOT_MACS[$i]}"; }
      slot_tap() { local i; i="$(slot_index "$1")" || return 1; printf '%s' "''${SLOT_TAPS[$i]}"; }
      slot_cid() { local i; i="$(slot_index "$1")" || return 1; printf '%s' "''${SLOT_CIDS[$i]}"; }

      session_file() { printf '%s' "$SLOTS_DIR/$1/session.json"; }
      mount_point()  { printf '%s' "$STATE_ROOT/$1/workspace"; }
      job_dir()      { printf '%s' "$JOBS_ROOT/$1"; }
      job_spec()     { printf '%s' "$JOBS_ROOT/$1/$JOB_SPEC_NAME"; }
      job_prompt()   { printf '%s' "$JOBS_ROOT/$1/$JOB_PROMPT_NAME"; }
      job_result()   { printf '%s' "$JOBS_ROOT/$1/out/$JOB_RESULT_NAME"; }

      # ---- allocation-marker helpers (§21 + ticket 4 ownership) ----------
      # A marker records WHO owns the slot: the task, the launcher pid together
      # with that pid's start time (so a recycled pid cannot masquerade as the
      # owner), the VM unit and a random allocation token. Destructive
      # operations that act on a slot they did not allocate (cancel, recover)
      # compare the TOKEN, never just the slot name.
      marker_field() {
          local slot="$1" field="$2" f
          f="$(session_file "$slot")"
          [[ -e "$f" ]] || return 1
          jq -r --arg f "$field" '.[$f] // ""' "$f" 2>/dev/null || return 1
      }

      new_token() { cat /proc/sys/kernel/random/uuid; }

      # Start time (clock ticks since boot) of a pid, from /proc/<pid>/stat.
      # The comm field can contain spaces and parentheses, so cut everything up
      # to the LAST ')' first; starttime is then the 20th remaining field.
      proc_start_time() {
          local pid="$1" rest
          [[ "$pid" =~ ^[0-9]+$ ]] || return 1
          rest="$(sed -e 's/^.*) //' "/proc/$pid/stat" 2>/dev/null)" || return 1
          [[ -n "$rest" ]] || return 1
          printf '%s' "$rest" | awk '{print $20}'
      }

      # True when the recorded launcher process is STILL the one that allocated
      # the slot (same pid AND same start time).
      owner_alive() {
          local pid="$1" start="$2" now
          [[ -n "$pid" && -n "$start" ]] || return 1
          [[ -d "/proc/$pid" ]] || return 1
          now="$(proc_start_time "$pid")" || return 1
          [[ "$now" == "$start" ]]
      }

      service_active() { systemctl is-active --quiet "microvm@$1.service"; }

      # A slot is free when it has no live session marker and its unit is
      # inactive. Both checks run while the global allocator lock is held, so
      # the decision cannot race another `run` (§21: no double-allocation).
      #
      # NOTE (stale slots): treating *any* session marker as "busy" is a
      # deliberate trade-off. After a hard kill/power loss the EXIT trap does
      # not run and the marker persists even though the unit is inactive, so
      # the slot stays reserved. We do NOT auto-reclaim inactive+marked slots
      # here: a concurrent `run` writes the `allocating` marker *before*
      # start_vm, so reclaiming-on-inactive would race and double-allocate.
      # Recover a stale slot manually with `agent-microvm destroy <slot>`
      # (`status` flags such slots as `stale: yes`).
      slot_is_free() {
          local name="$1"
          [[ -e "$(session_file "$name")" ]] && return 1
          service_active "$name" && return 1
          return 0
      }

      # ---- shared allocation (§21) ---------------------------------------
      # Selects a free slot under the GLOBAL allocator lock, takes the per-slot
      # lock, writes the atomic `allocating` marker and releases the global
      # lock. Results are returned in GLOBALS (never via stdout): the fds 9/8
      # holding the locks must belong to THIS shell, and a command
      # substitution would place them in a subshell that closes them again.
      #
      # The caller MUST have armed its cleanup trap before calling this.
      ALLOC_SLOT=""
      ALLOC_TOKEN=""
      allocate_slot() {
          local name
          ALLOC_SLOT=""
          ALLOC_TOKEN="$(new_token)"
          mkdir -p -- "$RUN_DIR" "$SLOTS_DIR"
          exec 9>"$ALLOC_LOCK"
          flock 9
          for name in "''${SLOT_NAMES[@]}"; do
              if slot_is_free "$name"; then ALLOC_SLOT="$name"; break; fi
          done
          if [[ -z "$ALLOC_SLOT" ]]; then
              flock -u 9
              die "no free slot (all ''${#SLOT_NAMES[@]} in use)"
          fi
          # Per-slot lock, held for the remainder of this process (§21).
          exec 8>"$RUN_DIR/$ALLOC_SLOT.lock"
          if ! flock -n 8; then
              flock -u 9
              die "slot $ALLOC_SLOT is locked by another launcher"
          fi
          mkdir -p -- "$SLOTS_DIR/$ALLOC_SLOT"
          # Atomic allocation marker written while the allocator lock is held,
          # so a concurrent run/submit can no longer pick this slot. It already
          # carries the ownership token, so even this early state can be
          # attributed to a launcher.
          jq -n --arg slot "$ALLOC_SLOT" --arg token "$ALLOC_TOKEN" \
              '{slot:$slot, state:"allocating", token:$token}' \
              > "$(session_file "$ALLOC_SLOT")"
          flock -u 9
          exec 9>&-
      }

      # Full session record for status/list/cancel/recover (§34 + ticket 4).
      # Contains NO secrets: task, slot, workspace, unit, mode, agent, timeout,
      # the owning launcher's pid + pid start time, and the allocation token.
      write_session_marker() {
          local slot="$1" token="$2" mode="$3" task="$4" repo="$5" clone="$6" \
                agent="$7" branch="$8" timeout_s="$9"
          local tmp
          tmp="$(mktemp "$SLOTS_DIR/$slot/.session.XXXXXX")"
          jq -n \
              --arg slot "$slot" --arg task "$task" --arg repo "$repo" \
              --arg workspace "$clone" --arg mount "$(mount_point "$slot")" \
              --arg agent "$agent" --arg branch "$branch" \
              --arg ip "$(slot_ip "$slot")" --arg mac "$(slot_mac "$slot")" \
              --arg start "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
              --arg owner "$(id -un)($(id -u))" \
              --arg token "$token" --arg mode "$mode" \
              --arg unit "microvm@$slot.service" \
              --arg pid "$$" --arg pid_start "$(proc_start_time "$$")" \
              --arg timeout "$timeout_s" \
              '{slot:$slot, state:"running", task:$task, repository:$repo,
                workspace:$workspace, mount:$mount, agent:$agent, branch:$branch,
                ip:$ip, mac:$mac, start:$start, lock_owner:$owner,
                token:$token, mode:$mode, unit:$unit, pid:$pid,
                pid_start:$pid_start, timeout:$timeout}' > "$tmp"
          mv -f -- "$tmp" "$(session_file "$slot")"
      }

      # ---- §22 strict task-name validation -------------------------------
      validate_task_name() {
          local name="$1"
          [[ -n "$name" ]] || die "task name must not be empty"
          (( ''${#name} <= 64 )) || die "task name too long (max 64 chars)"
          [[ "$name" != -* ]] || die "task name must not start with '-'"
          [[ "$name" == *".."* ]] && die "task name must not contain '..'"
          [[ "$name" =~ ^[a-zA-Z0-9._-]+$ ]] \
              || die "task name may contain only [a-zA-Z0-9._-]: '$name'"
          [[ "$name" != "." && "$name" != ".." ]] || die "invalid task name"
      }

      # ---- §23 source-repo validation ------------------------------------
      # Echoes the canonical git toplevel on success. $PWD is NOT trusted: the
      # caller-provided path is canonicalised with realpath (resolving every
      # symlink) before any check, so a symlink cannot smuggle an escape.
      validate_repository() {
          local input="$1" real top
          [[ -n "$input" ]] || die "--repository must not be empty"
          [[ -e "$input" ]] || die "repository path does not exist: $input"
          real="$(realpath -e -- "$input")" \
              || die "cannot canonicalise repository path: $input"
          # Scoped safe.directory override: the launcher runs as root but the
          # repo is typically owned by uid 1000; git >= 2.35.2 would otherwise
          # fail the dubious-ownership check even for root. Scoped to exactly
          # this path — NEVER safe.directory='*'.
          top="$(git -c safe.directory="$real" -C "$real" rev-parse --show-toplevel 2>/dev/null)" \
              || die "not a git repository: $real"
          top="$(realpath -e -- "$top")" \
              || die "cannot canonicalise git toplevel: $top"
          [[ "$top" == /* ]]  || die "git toplevel is not absolute: $top"
          [[ "$top" != "/" ]] || die "refusing to use '/' as a repository"
          [[ "$top" != "''${HOME:-}" ]] \
              || die "refusing to use the host home directory as a repository"
          [[ "$top" != "$RUNTIME_ROOT" && "$top" != "$RUNTIME_ROOT"/* ]] \
              || die "refusing a repository inside the agent runtime root"
          [[ "$top" != "$WORKSPACE_ROOT" && "$top" != "$WORKSPACE_ROOT"/* ]] \
              || die "refusing a repository that is itself an agent workspace"
          [[ "$top" != "$STATE_ROOT"/* ]] \
              || die "refusing a repository inside the microVM state root"
          printf '%s' "$top"
      }

      # ---- branch-name validation (guards `git checkout -b`) -------------
      # A caller-supplied --branch could begin with '-' and be mistaken for a
      # git option; there is no shell injection (the value is always quoted),
      # but reject the ambiguous forms so `checkout -b` cannot be steered.
      validate_branch_name() {
          local br="$1"
          [[ "$br" != -* ]] || die "branch name must not start with '-': $br"
          [[ "$br" != *".."* ]] || die "branch name must not contain '..': $br"
          [[ "$br" != *[[:space:]]* ]] || die "branch name must not contain whitespace: $br"
          [[ "$br" != *:* ]] || die "branch name must not contain ':': $br"
      }

      # ---- agent-name validation (§29 fixed agent set) -------------------
      # The agent name crosses the SSH boundary into the (untrusted) guest;
      # constrain it to the known agent set so it can never carry guest-side
      # metacharacters. This is defence-in-depth, not a host-side control.
      # The accepted set is GENERATED from ./agents.nix — the same registry
      # that builds the guest packages and the guest `agent-run` dispatch.
      validate_agent_name() {
          case "$1" in
              ${agentRegistry.namesCasePattern}) return 0 ;;
              *) die "unknown --agent '$1' (expected: ${agentRegistry.namesAlternation})" ;;
          esac
      }

      # Batch mode additionally requires the agent to declare a non-interactive
      # invocation (`batchArgs` in the registry), so `submit` cannot start an
      # agent that would sit waiting for a TTY forever. Also generated.
      validate_batch_agent_name() {
          case "$1" in
              ${agentRegistry.batchNamesCasePattern}) return 0 ;;
              *) die "--agent '$1' cannot run unattended (expected: ${agentRegistry.batchNamesAlternation})" ;;
          esac
      }

      # A job timeout must be a plain positive integer within the module's
      # bounds; the guest re-validates it against the same maximum.
      validate_timeout() {
          local t="$1"
          [[ "$t" =~ ^[0-9]+$ ]] || die "--timeout must be a positive integer (got '$t')"
          (( t >= 1 )) || die "--timeout must be >= 1"
          (( t <= JOB_MAX_TIMEOUT )) \
              || die "--timeout $t exceeds the configured maximum $JOB_MAX_TIMEOUT"
      }

      # ---- §24 standalone clone creation ---------------------------------
      verify_clone() {
          local clone="$1" cr gd gcd
          cr="$(realpath -e -- "$clone")" || die "clone vanished: $clone"
          gd="$(git -C "$cr" rev-parse --absolute-git-dir)" \
              || die "clone has no git dir: $cr"
          gd="$(realpath -e -- "$gd")"
          gcd="$(git -C "$cr" rev-parse --path-format=absolute --git-common-dir)" \
              || die "clone has no git common dir: $cr"
          gcd="$(realpath -e -- "$gcd")"
          [[ "$gd"  == "$cr"/* ]] || die "git-dir escapes the workspace: $gd"
          [[ "$gcd" == "$cr"/* ]] || die "git-common-dir escapes the workspace: $gcd"
      }

      # Creates the standalone clone at $WORKSPACE_ROOT/<task>. Runs in the
      # CALLER's shell (NOT a command substitution) so a `die` here exits
      # cmd_run directly and its EXIT trap fires with `slot` still in scope,
      # tearing the just-allocated slot down. The clone path is deterministic
      # ($WORKSPACE_ROOT/<task>), so the caller derives it itself rather than
      # capturing it from this function's stdout. Running create_clone in a
      # `$(...)` subshell instead would fire the EXIT trap inside that
      # subshell, where the enclosing `slot`/`committed` locals are NOT in
      # scope: cleanup would no-op AND the parent trap would never run, so a
      # failure (e.g. "workspace already exists") both tripped `set -u`
      # ("slot: unbound variable") and leaked the allocated slot forever.
      create_clone() {
          local repo="$1" task="$2" branch="$3"
          local clone="$WORKSPACE_ROOT/$task"
          [[ ! -e "$clone" ]] \
              || die "workspace already exists: $clone (pick another --name or 'workspace-remove')"
          mkdir -p -- "$WORKSPACE_ROOT"
          # --no-local forbids hardlinks/alternates: a fully independent copy
          # of the objects, so the original repo is never shared into the VM.
          # The scoped safe.directory covers reading the user-owned SOURCE
          # repo as root (dubious-ownership check); the fresh clone itself is
          # root-owned at this point, so it needs no override. All later git
          # calls in create_clone/verify_clone (rev-parse, checkout -b) also
          # run on the root-owned clone BEFORE the chown to 1000:1000 below.
          git -c safe.directory="$repo" clone --no-local -- "$repo" "$clone" \
              || die "git clone --no-local failed"
          verify_clone "$clone"
          if [[ -n "$branch" ]]; then
              git -C "$clone" checkout -b "$branch" >/dev/null 2>&1 \
                  || log "warning: could not create branch '$branch' (continuing)"
          fi
          # --- §11 UID/GID ownership --------------------------------------
          # The clone (and any .git objects/refs just written by the branch
          # checkout above) is created as root. virtiofsd passes file
          # ownership through unchanged (no --translate-uid/gid; see the
          # workspace share in guest.nix), so the numeric owner of this host
          # tree is exactly what the guest sees. The guest `agent` user is
          # uid/gid 1000, so chown the WHOLE tree to 1000:1000 here: inside
          # the guest, /workspace then appears owned by `agent` and is
          # read-write, satisfying `agent-run`'s `test -w /workspace` check.
          #
          # Host-side implication: uid/gid 1000 is the primary UNPRIVILEGED
          # interactive user on f13 (the human who inspects/exports the
          # result, per plan §25) — NOT a privileged id — so no guest id maps
          # to a privileged host id (plan §11), and the host user can still
          # manage / diff / import the clone directly. Done AFTER the branch
          # checkout so freshly written git metadata is owned by the agent too.
          chown -R "$GUEST_AGENT_UID:$GUEST_AGENT_GID" -- "$clone" \
              || die "failed to chown workspace clone to $GUEST_AGENT_UID:$GUEST_AGENT_GID: $clone"
      }

      # ---- §26 bind-mount lifecycle --------------------------------------
      setup_bind_mount() {
          local slot="$1" clone="$2" mp
          mp="$(mount_point "$slot")"
          mkdir -p -- "$mp"
          if findmnt -n -- "$mp" >/dev/null 2>&1; then
              umount -- "$mp" || die "could not unmount stale bind at $mp"
          fi
          mount --bind -- "$clone" "$mp" || die "bind mount failed: $clone -> $mp"
          findmnt -n -- "$mp" >/dev/null 2>&1 \
              || die "bind mount verification failed: $mp"
      }

      teardown_bind_mount() {
          local mp
          mp="$(mount_point "$1")"
          if findmnt -n -- "$mp" >/dev/null 2>&1; then
              umount -- "$mp" 2>/dev/null || umount -l -- "$mp" 2>/dev/null || true
          fi
      }

      # ---- §27 VM lifecycle (systemd only) -------------------------------
      start_vm() {
          # Refresh the declarative VM's `current` runner symlink so the guest
          # we boot always matches the CURRENTLY-BOOTED host system generation
          # rather than a stale runner left over from an earlier build. For
          # fully-declarative microVMs, microvm.nix only re-links
          # <stateRoot>/<slot>/current from `install-microvm-<slot>.service`,
          # which runs on host activation. A slot started after a host rebuild
          # whose install step has not re-run would otherwise boot the OLD
          # guest config (e.g. missing the home-manager dotfile provisioning),
          # which looks like "the sandbox is not provisioned". The unit is an
          # idempotent host oneshot that only re-links a symlink; ignore any
          # failure so a refresh problem never blocks launch. Safe here because
          # the freshly-allocated slot is not yet running.
          systemctl restart "install-microvm-$1.service" 2>/dev/null || true
          # Make sure the slot's SSH host identity exists BEFORE the VM (and
          # thus its virtiofsd for the read-only hostkey share) starts: the
          # provisioning unit is an idempotent RemainAfterExit oneshot, so this
          # is a no-op once it has run. It is also wantedBy multi-user.target,
          # so on a booted host this only covers the "key dir deleted by hand"
          # / "slotCount just increased" cases. Failure is fatal: without the
          # key sshd cannot start in the guest.
          if [[ "$SSH_ENABLED" == "1" ]]; then
              systemctl start agent-microvm-hostkeys.service \
                  || die "failed to provision per-slot SSH host keys (agent-microvm-hostkeys.service)"
          fi
          systemctl start "microvm@$1.service" \
              || die "failed to start microvm@$1.service"
      }

      stop_vm() {
          local slot="$1" waited=0
          systemctl stop "microvm@$slot.service" 2>/dev/null || true
          while service_active "$slot"; do
              if (( waited >= SHUTDOWN_TIMEOUT )); then
                  log "microvm@$slot did not stop within ''${SHUTDOWN_TIMEOUT}s, killing"
                  systemctl kill --signal=SIGKILL "microvm@$slot.service" 2>/dev/null || true
                  break
              fi
              sleep 1
              waited=$(( waited + 1 ))
          done
      }

      # ---- batch-job data (ticket 4) -------------------------------------
      # Writes the versioned spec + the prompt into the slot's job directory
      # (mounted read-write into the guest at $GUEST_JOB_DIR, but with these
      # files root-owned 0444 inside a root-owned 0755 dir, so the guest can
      # only READ them). The prompt is COPIED, never passed as an argument, and
      # neither file ever enters the Nix store.
      prepare_job() {
          local slot="$1" task="$2" agent="$3" prompt_src="$4" timeout_s="$5"
          local dir spec
          dir="$(job_dir "$slot")"
          spec="$(job_spec "$slot")"
          install -d -m 0755 -o root -g root -- "$dir"
          # out/ is written by the guest `agent` user (uid/gid 1000, §11).
          install -d -m 0755 -o "$GUEST_AGENT_UID" -g "$GUEST_AGENT_GID" -- "$dir/out"
          # A stale result from an earlier job would be mistaken for this one.
          rm -f -- "$(job_result "$slot")"
          install -m 0444 -o root -g root -- "$prompt_src" "$(job_prompt "$slot")" \
              || die "could not install the prompt file into $dir"
          jq -n \
              --argjson version "$JOB_SPEC_VERSION" \
              --arg taskId "$task" --arg agent "$agent" \
              --arg workspace "$GUEST_WORKSPACE" --arg promptFile "$GUEST_PROMPT" \
              --argjson timeoutSeconds "$timeout_s" \
              '{version:$version, taskId:$taskId, agent:$agent,
                workspace:$workspace, promptFile:$promptFile,
                timeoutSeconds:$timeoutSeconds}' > "$spec.tmp" \
              || die "could not render the job spec"
          chmod 0444 -- "$spec.tmp"
          chown root:root -- "$spec.tmp"
          # Rename last: the guest unit is conditional on spec.json existing, so
          # it must only appear once it is complete.
          mv -f -- "$spec.tmp" "$spec"
      }

      # Removes the runtime job data of a slot (spec, prompt, result). Called on
      # teardown and before an interactive run, so no slot ever inherits another
      # task's job. The workspace clone is NEVER touched here (§35).
      clear_job() {
          local slot="$1" dir
          dir="$(job_dir "$slot")"
          rm -f -- "$dir/$JOB_SPEC_NAME" "$dir/$JOB_SPEC_NAME.tmp" \
                   "$dir/$JOB_PROMPT_NAME" "$dir/out/$JOB_RESULT_NAME"
      }

      job_state() {
          local slot="$1" f
          f="$(job_result "$slot")"
          [[ -e "$f" ]] || return 1
          jq -r '.state // ""' "$f" 2>/dev/null || return 1
      }

      # ---- §21 slot cleanup / interrupt handling -------------------------
      # Tear a slot down WITHOUT deleting the workspace clone (§26/§35): stop
      # the VM, unmount the bind, remove the slot transient state. Locks are
      # released implicitly when this process exits and closes their fds.
      cleanup_slot() {
          local slot="$1"
          [[ -n "$slot" ]] || return 0
          stop_vm "$slot"
          teardown_bind_mount "$slot"
          # Runtime job data (spec/prompt/result) is transient per task: drop it
          # with the slot, exactly like the session marker. The workspace clone
          # is deliberately kept (§26/§35).
          clear_job "$slot"
          rm -rf -- "''${SLOTS_DIR:?}/$slot"
      }

      # Tear a slot down ONLY while it still belongs to `token` (ticket 4
      # "allocation safety"): used by operations that act on a slot they did not
      # allocate themselves, so a `cancel` can never stop a slot that has since
      # been re-allocated to a different task.
      cleanup_slot_owned() {
          local slot="$1" token="$2" cur
          cur="$(marker_field "$slot" token || true)"
          [[ "$cur" == "$token" ]] \
              || die "slot $slot no longer belongs to that task (allocation token changed); refusing to touch it"
          cleanup_slot "$slot"
      }

      # ---- readiness (§34) -----------------------------------------------
      # Fails closed: without the known_hosts file every ssh below would fail
      # verification, so say so once, clearly, instead of timing out.
      require_known_hosts() {
          [[ -r "$KNOWN_HOSTS" ]] || die \
              "missing host-key database $KNOWN_HOSTS; run: systemctl start agent-microvm-hostkeys.service"
      }

      guest_ssh_ready() {
          local ip="$1"
          [[ "$SSH_ENABLED" == "1" ]] || return 1
          [[ -r "$KNOWN_HOSTS" ]] || return 1
          ssh -o BatchMode=yes "''${SSH_VERIFY_OPTS[@]}" -o ConnectTimeout=3 \
              ''${AGENT_MICROVM_SSH_KEY:+-i "$AGENT_MICROVM_SSH_KEY"} \
              "$SSH_USER@$ip" true >/dev/null 2>&1
      }

      wait_ready() {
          local ip="$1" waited=0
          [[ "$SSH_ENABLED" == "1" ]] || { log "SSH disabled; not waiting for guest readiness"; return 0; }
          while ! guest_ssh_ready "$ip"; do
              if (( waited >= READY_TIMEOUT )); then
                  log "guest at $ip not reachable via SSH within ''${READY_TIMEOUT}s"
                  return 1
              fi
              sleep 3
              waited=$(( waited + 3 ))
          done
          return 0
      }

      # ---- resolve a slot|task argument to a slot name -------------------
      resolve_slot() {
          local arg="$1" f task
          if is_slot_name "$arg"; then
              printf '%s' "$arg"
              return 0
          fi
          for f in "$SLOTS_DIR"/*/session.json; do
              [[ -e "$f" ]] || continue
              task="$(jq -r '.task // empty' "$f" 2>/dev/null || true)"
              if [[ "$task" == "$arg" ]]; then
                  jq -r '.slot' "$f"
                  return 0
              fi
          done
          return 1
      }

      # ==== subcommands ====================================================

      usage() {
          cat >&2 <<EOF
      Usage: $PROG <command> [options]

      Commands:
        run --name <task> --repository <path> [--agent <name>] [--branch <br>] [--attach]
                              Allocate a free slot, create a standalone clone,
                              bind-mount it at the slot's /workspace source and
                              start the microVM. With --attach, SSH in running
                              'agent-run <agent>' and tear the VM down on exit
                              (the workspace clone is always kept).
        stop <slot|task>      Stop the VM, unmount the bind, drop slot transient
                              state. Keeps workspace/git/patches.
        destroy <slot|task>   Like stop, plus clear ephemeral slot runtime.
                              Keeps workspace/git/patches.
        status [slot|task]    Show slot/service/IP/MAC/task/workspace/mount/
                              agent/start-time/SSH-readiness/lock-owner.
        list                  One-line status for every slot.
        ssh <slot|task> [--] [cmd...]   SSH into the guest 'agent' user.
        console <slot|task>   Attach to the VM serial console (journal).
        submit --name <task> --repository <path> --agent <name>
               --prompt-file <path> [--timeout <sec>] [--branch <br>]
                              UNATTENDED batch run: allocate a slot, clone the
                              repo, write a versioned job spec + the prompt into
                              the slot's job dir, boot the VM, wait for the
                              guest's structured result, then tear the VM down
                              (the workspace clone is always kept). Exit code:
                              0 completed, 1 agent failed, 124 timed out,
                              70 infrastructure error.
        cancel <task>         Cancel a running job/session by task name. Refuses
                              unless the slot still carries that task's
                              allocation token. Keeps the workspace.
        recover [--dry-run]   Reconcile slots with systemd: stop orphaned units,
                              unmount stale mounts, drop stale markers and job
                              data. Always keeps workspace clones. --dry-run
                              only prints what it would do.
        workspace-remove <task> [--force]
                              Delete the standalone clone. Separate + guarded:
                              refuses on uncommitted changes / unexported
                              commits, and on a slot still using the clone,
                              without --force. With --force it also stops any
                              slot still holding the clone before removing it.

      Supported agents (--agent), generated from the module's agent registry:
        ${lib.concatStringsSep "\n  " agentRegistry.names}
      EOF
          exit 2
      }

      cmd_run() {
          require_root run
          local task="" repo="" agent="" branch="" attach=0
          while [[ $# -gt 0 ]]; do
              case "$1" in
                  --name)       task="''${2-}"; shift 2 ;;
                  --repository) repo="''${2-}"; shift 2 ;;
                  --agent)      agent="''${2-}"; shift 2 ;;
                  --branch)     branch="''${2-}"; shift 2 ;;
                  --attach)     attach=1; shift ;;
                  --name=*)       task="''${1#*=}"; shift ;;
                  --repository=*) repo="''${1#*=}"; shift ;;
                  --agent=*)      agent="''${1#*=}"; shift ;;
                  --branch=*)     branch="''${1#*=}"; shift ;;
                  *) die "run: unknown argument '$1'" ;;
              esac
          done
          [[ -n "$task" ]] || die "run: --name <task> is required"
          [[ -n "$repo" ]] || die "run: --repository <path> is required"
          validate_task_name "$task"
          [[ -z "$agent" ]] || validate_agent_name "$agent"
          local top
          top="$(validate_repository "$repo")"
          [[ -z "$branch" ]] && branch="agent/$task"
          # The default `agent/<task>` is safe (task is regex-validated); a
          # caller-supplied branch is validated so it cannot look like a flag.
          validate_branch_name "$branch"

          mkdir -p -- "$RUN_DIR" "$SLOTS_DIR" "$WORKSPACE_ROOT"

          # Arm the cleanup trap BEFORE creating any slot state, so a signal
          # in the allocation window still tears the slot down (keeping the
          # workspace clone; §43). cleanup_slot is a no-op for state that does
          # not exist yet, and for the still-empty `slot`.
          local slot="" token="" committed=0
          # shellcheck disable=SC2317  # invoked via trap
          # ''${committed:-0} / ''${slot:-}: create_clone (and other helpers) run
          # in command-substitution subshells that inherit this EXIT trap; the
          # enclosing function's locals are not in scope while the trap fires
          # there, so a failure path (e.g. "workspace already exists") would
          # trip `set -u` without the defaults (§21/§43).
          on_exit() { (( ''${committed:-0} )) || cleanup_slot "''${slot:-}"; }
          trap on_exit EXIT
          trap 'exit 130' INT TERM

          allocate_slot
          slot="$ALLOC_SLOT"
          token="$ALLOC_TOKEN"

          local clone ip
          ip="$(slot_ip "$slot")"
          log "allocated slot $slot ($ip)"
          # Deterministic clone path; create_clone runs in THIS shell (see its
          # header) so a failure exits cmd_run and its EXIT trap cleans up.
          clone="$WORKSPACE_ROOT/$task"
          create_clone "$top" "$task" "$branch"
          setup_bind_mount "$slot" "$clone"
          # An interactive slot must not accidentally pick up a stale batch job.
          clear_job "$slot"
          start_vm "$slot"

          # Persist the full session record for status/list (§34). No secrets.
          local run_mode="detached"
          (( attach )) && run_mode="attached"
          write_session_marker "$slot" "$token" "$run_mode" "$task" "$top" \
              "$clone" "$agent" "$branch" ""

          if (( attach )); then
              [[ "$SSH_ENABLED" == "1" ]] || die "--attach requires enableSsh = true"
              [[ -n "$agent" ]] || die "--attach requires --agent <name>"
              # On readiness failure the EXIT trap runs cleanup_slot: the VM is
              # stopped and the bind unmounted, but the workspace clone is kept.
              require_known_hosts
              wait_ready "$ip" || die "guest not ready; tearing down slot $slot (workspace kept at $clone)"
              log "attaching to $slot; running 'agent-run $agent' in /workspace"
              ssh "''${SSH_VERIFY_OPTS[@]}" \
                  ''${AGENT_MICROVM_SSH_KEY:+-i "$AGENT_MICROVM_SSH_KEY"} \
                  -t "$SSH_USER@$ip" -- agent-run "$agent" || true
              # Foreground session finished: tear the VM down, keep the clone.
              log "session ended; tearing down $slot (workspace kept at $clone)"
              cleanup_slot "$slot"
              committed=1
              trap - EXIT INT TERM
              return 0
          fi

          # Detached mode: leave the VM running under systemd; the session
          # marker keeps the slot allocated. The per-slot flock is released
          # when this process exits (allocation state lives in session.json).
          committed=1
          trap - EXIT INT TERM
          cat >&2 <<EOF
      $PROG: slot $slot is running.
        workspace: $clone
        guest IP:  $ip
        connect:   $PROG ssh $slot
        status:    $PROG status $slot
        stop:      $PROG stop $slot
        inspect changes:
          git -C "$clone" diff
          git -C "$clone" format-patch "origin/HEAD..$branch"
      EOF
      }

      # ==== batch submission (ticket 4) ====================================
      # Mirrors cmd_run's allocation/trap structure, then waits for the GUEST's
      # structured result instead of attaching a terminal.
      cmd_submit() {
          require_root submit
          local task="" repo="" agent="" branch="" prompt="" timeout_s=""
          while [[ $# -gt 0 ]]; do
              case "$1" in
                  --name)        task="''${2-}"; shift 2 ;;
                  --repository)  repo="''${2-}"; shift 2 ;;
                  --agent)       agent="''${2-}"; shift 2 ;;
                  --branch)      branch="''${2-}"; shift 2 ;;
                  --prompt-file) prompt="''${2-}"; shift 2 ;;
                  --timeout)     timeout_s="''${2-}"; shift 2 ;;
                  --name=*)        task="''${1#*=}"; shift ;;
                  --repository=*)  repo="''${1#*=}"; shift ;;
                  --agent=*)       agent="''${1#*=}"; shift ;;
                  --branch=*)      branch="''${1#*=}"; shift ;;
                  --prompt-file=*) prompt="''${1#*=}"; shift ;;
                  --timeout=*)     timeout_s="''${1#*=}"; shift ;;
                  *) die "submit: unknown argument '$1'" ;;
              esac
          done
          [[ -n "$task" ]]   || die "submit: --name <task> is required"
          [[ -n "$repo" ]]   || die "submit: --repository <path> is required"
          [[ -n "$agent" ]]  || die "submit: --agent <name> is required"
          [[ -n "$prompt" ]] || die "submit: --prompt-file <path> is required"
          validate_task_name "$task"
          validate_batch_agent_name "$agent"
          [[ -z "$timeout_s" ]] && timeout_s="$JOB_DEFAULT_TIMEOUT"
          validate_timeout "$timeout_s"
          # The prompt must be a readable regular FILE (not a fifo/device, not a
          # directory); it is copied into the job dir, never referenced.
          local prompt_real
          prompt_real="$(realpath -e -- "$prompt")" \
              || die "submit: --prompt-file does not exist: $prompt"
          [[ -f "$prompt_real" && -r "$prompt_real" ]] \
              || die "submit: --prompt-file must be a readable regular file: $prompt"
          [[ -s "$prompt_real" ]] || die "submit: --prompt-file is empty: $prompt"
          local top
          top="$(validate_repository "$repo")"
          [[ -z "$branch" ]] && branch="agent/$task"
          validate_branch_name "$branch"

          mkdir -p -- "$RUN_DIR" "$SLOTS_DIR" "$WORKSPACE_ROOT" "$RESULTS_DIR"

          local slot="" token="" committed=0
          # shellcheck disable=SC2317  # invoked via trap
          on_exit() { (( ''${committed:-0} )) || cleanup_slot "''${slot:-}"; }
          trap on_exit EXIT
          trap 'exit 130' INT TERM

          allocate_slot
          slot="$ALLOC_SLOT"
          token="$ALLOC_TOKEN"

          local clone ip
          ip="$(slot_ip "$slot")"
          log "allocated slot $slot ($ip) for batch task '$task'"
          clone="$WORKSPACE_ROOT/$task"
          create_clone "$top" "$task" "$branch"
          setup_bind_mount "$slot" "$clone"
          prepare_job "$slot" "$task" "$agent" "$prompt_real" "$timeout_s"
          write_session_marker "$slot" "$token" "batch" "$task" "$top" \
              "$clone" "$agent" "$branch" "$timeout_s"
          start_vm "$slot"

          # --- wait for the structured result -------------------------------
          # HOST timeout = the job's own timeout + a grace period, so the guest
          # (whose `timeout(1)` fires first) still gets to write result.json.
          local deadline=$(( timeout_s + JOB_GRACE )) waited=0 state="" rc=70
          log "waiting up to ''${deadline}s for the job result"
          while :; do
              state="$(job_state "$slot" || true)"
              case "$state" in
                  completed|failed|timed-out|infrastructure-error) break ;;
              esac
              if (( waited >= deadline )); then
                  log "job did not finish within ''${deadline}s; stopping the VM"
                  state="timed-out"
                  break
              fi
              # A guest that died without writing a result must not hang us for
              # the full timeout window.
              if (( waited > 0 )) && ! service_active "$slot"; then
                  log "microvm@$slot stopped without a terminal result"
                  state="''${state:-infrastructure-error}"
                  [[ "$state" == "starting" || "$state" == "running" ]] \
                      && state="infrastructure-error"
                  break
              fi
              sleep 3
              waited=$(( waited + 3 ))
          done

          # Archive the result (host-only) BEFORE the job dir is cleared, so
          # `status <task>` can still report the outcome.
          local archived="$RESULTS_DIR/$task.json"
          if [[ -e "$(job_result "$slot")" ]]; then
              cp -f -- "$(job_result "$slot")" "$archived.tmp" && mv -f -- "$archived.tmp" "$archived"
          else
              jq -n --argjson version "$JOB_SPEC_VERSION" --arg taskId "$task" \
                  --arg agent "$agent" --arg state "$state" \
                  '{version:$version, taskId:$taskId, agent:$agent, state:$state,
                    exitCode:70, timedOut:($state=="timed-out"), message:"no guest result"}' \
                  > "$archived.tmp" && mv -f -- "$archived.tmp" "$archived"
          fi

          case "$state" in
              completed) rc=0 ;;
              failed)    rc=1 ;;
              timed-out) rc=124 ;;
              *)         rc=70 ;;
          esac

          # Teardown: stop the VM, unmount, drop job data + marker. The
          # workspace clone is ALWAYS kept (§35).
          cleanup_slot "$slot"
          committed=1
          trap - EXIT INT TERM

          cat >&2 <<EOF
      $PROG: batch task '$task' finished with state '$state'.
        workspace: $clone
        result:    $archived
        inspect changes:
          git -C "$clone" diff
          git -C "$clone" format-patch "origin/HEAD..$branch"
      EOF
          jq . "$archived" || true
          return "$rc"
      }

      # ==== cancellation (ticket 4) ========================================
      cmd_cancel() {
          require_root cancel
          [[ $# -ge 1 ]] || die "cancel: <task> required"
          local task="$1"
          validate_task_name "$task"

          # Resolve the slot from the TASK, then remember its token and only act
          # while that token is unchanged — so a slot that has meanwhile been
          # re-allocated to another task is never stopped.
          local slot="" f cur_task
          for f in "$SLOTS_DIR"/*/session.json; do
              [[ -e "$f" ]] || continue
              cur_task="$(jq -r '.task // empty' "$f" 2>/dev/null || true)"
              if [[ "$cur_task" == "$task" ]]; then
                  slot="$(jq -r '.slot' "$f")"
                  break
              fi
          done
          [[ -n "$slot" ]] || die "no running task named '$task'"
          local token
          token="$(marker_field "$slot" token || true)"
          [[ -n "$token" ]] || die "slot $slot has no allocation token; refusing (use 'recover')"

          # Record the cancellation in the archived result, so the outcome of a
          # cancelled job is not silently indistinguishable from a crash.
          mkdir -p -- "$RESULTS_DIR"
          local archived="$RESULTS_DIR/$task.json"
          jq -n --argjson version "$JOB_SPEC_VERSION" --arg taskId "$task" \
              --arg agent "$(marker_field "$slot" agent || true)" \
              --arg finishedAt "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
              '{version:$version, taskId:$taskId, agent:$agent, state:"cancelled",
                exitCode:130, finishedAt:$finishedAt, timedOut:false,
                message:"cancelled by the operator"}' > "$archived.tmp" \
              && mv -f -- "$archived.tmp" "$archived"

          log "cancelling task '$task' on slot $slot (workspace kept)"
          # Requests a clean shutdown first, waits a bounded interval, then
          # force-kills — all inside cleanup_slot/stop_vm — and unmounts + drops
          # the runtime job data. Token-guarded.
          cleanup_slot_owned "$slot" "$token"
          log "cancelled; result recorded at $archived"
      }

      # ==== recovery (ticket 4) ============================================
      # Reconciles every slot's marker with the actual systemd/mount state.
      # NEVER deletes a workspace clone. --dry-run only prints.
      cmd_recover() {
          require_root recover
          local dry=0
          while [[ $# -gt 0 ]]; do
              case "$1" in
                  --dry-run) dry=1; shift ;;
                  *) die "recover: unknown argument '$1'" ;;
              esac
          done
          local slot marker active mounted mode pid pid_start task acted=0
          for slot in "''${SLOT_NAMES[@]}"; do
              marker=0
              [[ -e "$(session_file "$slot")" ]] && marker=1
              active=0
              service_active "$slot" && active=1
              mounted=0
              findmnt -n -- "$(mount_point "$slot")" >/dev/null 2>&1 && mounted=1
              task="$(marker_field "$slot" task 2>/dev/null || true)"
              mode="$(marker_field "$slot" mode 2>/dev/null || true)"
              pid="$(marker_field "$slot" pid 2>/dev/null || true)"
              pid_start="$(marker_field "$slot" pid_start 2>/dev/null || true)"

              local reason=""
              if (( marker && ! active )); then
                  # The EXIT trap never ran (hard kill / power loss): the slot is
                  # reserved but nothing is running.
                  reason="stale marker (unit inactive)"
              elif (( ! marker && active )); then
                  # A VM nobody claims: no task can be harmed by stopping it.
                  reason="orphaned unit (no allocation marker)"
              elif (( marker && active )) \
                   && [[ "$mode" == "attached" || "$mode" == "batch" ]] \
                   && ! owner_alive "$pid" "$pid_start"; then
                  # `attached`/`batch` slots are supervised by a live launcher;
                  # `detached` ones are not, so their dead pid is EXPECTED and
                  # must never trigger recovery.
                  reason="orphaned $mode run (launcher pid $pid is gone)"
              elif (( ! marker && ! active && mounted )); then
                  reason="stale bind mount"
              elif (( ! marker && ! active )) && [[ -e "$(job_spec "$slot")" ]]; then
                  reason="stale job data"
              fi

              if [[ -z "$reason" ]]; then
                  printf '%s: ok%s\n' "$slot" \
                      "$( (( marker )) && printf " (task %s, mode %s)" "''${task:-<none>}" "''${mode:-<none>}" )"
                  continue
              fi

              acted=1
              if (( dry )); then
                  printf '%s: would recover — %s\n' "$slot" "$reason"
                  (( active )) && printf '%s:   would stop %s\n' "$slot" "microvm@$slot.service"
                  (( mounted )) && printf '%s:   would unmount %s\n' "$slot" "$(mount_point "$slot")"
                  [[ -e "$(job_spec "$slot")" ]] \
                      && printf '%s:   would clear job data in %s\n' "$slot" "$(job_dir "$slot")"
                  (( marker )) && printf '%s:   would drop the allocation marker\n' "$slot"
                  printf '%s:   would KEEP the workspace clone\n' "$slot"
                  continue
              fi

              printf '%s: recovering — %s\n' "$slot" "$reason"
              if (( active )); then
                  printf '%s:   stopping %s\n' "$slot" "microvm@$slot.service"
                  stop_vm "$slot"
              fi
              if (( mounted )); then
                  printf '%s:   unmounting %s\n' "$slot" "$(mount_point "$slot")"
                  teardown_bind_mount "$slot"
              fi
              if [[ -e "$(job_spec "$slot")" || -e "$(job_prompt "$slot")" ]]; then
                  printf '%s:   clearing job data in %s\n' "$slot" "$(job_dir "$slot")"
                  clear_job "$slot"
              fi
              if (( marker )); then
                  printf '%s:   dropping the allocation marker\n' "$slot"
                  rm -rf -- "''${SLOTS_DIR:?}/$slot"
              fi
              printf '%s:   keeping the workspace clone\n' "$slot"
          done
          if (( ! acted )); then
              log "nothing to recover"
          fi
      }

      cmd_stop() {
          require_root stop
          [[ $# -ge 1 ]] || die "stop: <slot|task> required"
          local slot
          slot="$(resolve_slot "$1")" || die "no such slot or task: $1"
          log "stopping $slot (workspace kept)"
          cleanup_slot "$slot"
      }

      cmd_destroy() {
          require_root destroy
          [[ $# -ge 1 ]] || die "destroy: <slot|task> required"
          local slot
          slot="$(resolve_slot "$1")" || die "no such slot or task: $1"
          # §35: destroy removes ephemeral runtime + slot transient + bind
          # mount + VM process state, but must NOT delete workspace/git/patches.
          log "destroying $slot ephemeral state (workspace kept)"
          cleanup_slot "$slot"
      }

      cmd_status() {
          local targets=()
          if [[ $# -ge 1 ]]; then
              local s
              s="$(resolve_slot "$1")" || die "no such slot or task: $1"
              targets=("$s")
          else
              targets=("''${SLOT_NAMES[@]}")
          fi
          local slot f state task workspace agent start owner ip mac cid mnt ssh_ready
          local mode timeout_s jstate
          for slot in "''${targets[@]}"; do
              f="$(session_file "$slot")"
              ip="$(slot_ip "$slot")"
              mac="$(slot_mac "$slot")"
              cid="$(slot_cid "$slot")"
              if service_active "$slot"; then state="running"; else state="stopped"; fi
              task=""; workspace=""; agent=""; start=""; owner=""
              mode=""; timeout_s=""
              local sstate="" stale="no"
              if [[ -e "$f" ]]; then
                  task="$(jq -r '.task // ""' "$f")"
                  workspace="$(jq -r '.workspace // ""' "$f")"
                  agent="$(jq -r '.agent // ""' "$f")"
                  start="$(jq -r '.start // ""' "$f")"
                  owner="$(jq -r '.lock_owner // ""' "$f")"
                  sstate="$(jq -r '.state // ""' "$f")"
                  mode="$(jq -r '.mode // ""' "$f")"
                  timeout_s="$(jq -r '.timeout // ""' "$f")"
              fi
              # Job state: the live guest result while the slot runs, else the
              # archived result of the last run of that task.
              jstate="$(job_state "$slot" 2>/dev/null || true)"
              if [[ -z "$jstate" && -n "$task" && -e "$RESULTS_DIR/$task.json" ]]; then
                  jstate="$(jq -r '.state // ""' "$RESULTS_DIR/$task.json" 2>/dev/null || true)"
              fi
              # A slot with a persisted marker but an inactive unit is stale
              # (see slot_is_free NOTE): clear it with 'destroy <slot>'.
              if [[ -e "$f" ]] && ! service_active "$slot"; then stale="yes"; fi
              mnt="unmounted"
              if findmnt -n -- "$(mount_point "$slot")" >/dev/null 2>&1; then mnt="mounted"; fi
              ssh_ready="n/a"
              if [[ "$SSH_ENABLED" == "1" ]]; then
                  if guest_ssh_ready "$ip"; then ssh_ready="ready"; else ssh_ready="not-ready"; fi
              fi
              cat <<EOF
      slot:        $slot
        service:   $state
        ip:        $ip
        mac:       $mac
        vsock cid: $cid
        task:      ''${task:-<none>}
        workspace: ''${workspace:-<none>}
        bind-mount: $mnt
        agent:     ''${agent:-<none>}
        mode:      ''${mode:-<none>}
        job:       ''${jstate:-<none>}
        timeout:   ''${timeout_s:-<n/a>}
        started:   ''${start:-<n/a>}
        ssh:       $ssh_ready
        state:     ''${sstate:-<none>}
        stale:     $stale
        lock owner: ''${owner:-<none>}
      EOF
          done
      }

      cmd_list() {
          local slot state task
          for slot in "''${SLOT_NAMES[@]}"; do
              if service_active "$slot"; then state="running"; else state="stopped"; fi
              task=""
              [[ -e "$(session_file "$slot")" ]] \
                  && task="$(jq -r '.task // ""' "$(session_file "$slot")")"
              printf '%-10s %-8s %-16s %s\n' "$slot" "$state" "$(slot_ip "$slot")" "''${task:-<free>}"
          done
      }

      cmd_ssh() {
          [[ "$SSH_ENABLED" == "1" ]] || die "ssh: enableSsh is false"
          [[ $# -ge 1 ]] || die "ssh: <slot|task> required"
          local slot ip
          slot="$(resolve_slot "$1")" || die "no such slot or task: $1"
          shift
          ip="$(slot_ip "$slot")"
          service_active "$slot" || die "microvm@$slot is not running"
          [[ "''${1-}" == "--" ]] && shift
          # AUTHENTICATED channel (ticket 3 B): the slot's host key is stable
          # and pinned in $KNOWN_HOSTS, so verification is STRICT — a wrong or
          # unknown key aborts the connection instead of being accepted. Two
          # independent layers now protect this session: strict host-key
          # verification here, and per-TAP L2 `isolated` on the bridge
          # (network.nix), which prevents a co-resident guest from ARP-spoofing
          # the gateway or another slot in the first place.
          require_known_hosts
          exec ssh "''${SSH_VERIFY_OPTS[@]}" \
              ''${AGENT_MICROVM_SSH_KEY:+-i "$AGENT_MICROVM_SSH_KEY"} \
              -t "$SSH_USER@$ip" "$@"
      }

      cmd_console() {
          [[ $# -ge 1 ]] || die "console: <slot|task> required"
          local slot
          slot="$(resolve_slot "$1")" || die "no such slot or task: $1"
          # The Cloud Hypervisor serial console is captured by the microVM
          # systemd unit; follow it via the journal (no untracked CH process).
          exec journalctl -f -u "microvm@$slot.service"
      }

      cmd_workspace_remove() {
          require_root workspace-remove
          [[ $# -ge 1 ]] || die "workspace-remove: <task> required"
          local task="$1" force=0
          shift
          while [[ $# -gt 0 ]]; do
              case "$1" in
                  --force) force=1; shift ;;
                  *) die "workspace-remove: unknown argument '$1'" ;;
              esac
          done
          validate_task_name "$task"
          local clone="$WORKSPACE_ROOT/$task"
          [[ -d "$clone" ]] || die "no such workspace: $clone"
          # §35: guard against losing uncommitted work / unexported commits.
          # The clone is owned by 1000:1000 and this runs as root, so each git
          # call needs a scoped safe.directory override (never '*'). The
          # guards FAIL CLOSED: if git itself errors we cannot determine the
          # workspace state, so we refuse instead of treating it as clean.
          if (( ! force )); then
              local dirty rc
              rc=0
              dirty="$(git -c safe.directory="$clone" -C "$clone" status --porcelain)" || rc=$?
              if (( rc != 0 )); then
                  die "cannot determine workspace status (git status failed, exit $rc); refusing (use --force): $clone"
              fi
              if [[ -n "$dirty" ]]; then
                  die "workspace has uncommitted changes; refusing (use --force): $clone"
              fi
              local unpushed
              rc=0
              unpushed="$(git -c safe.directory="$clone" -C "$clone" log --branches --not --remotes --oneline)" || rc=$?
              if (( rc != 0 )); then
                  die "cannot determine unexported commits (git log failed, exit $rc); refusing (use --force): $clone"
              fi
              if [[ -n "$unpushed" ]]; then
                  die "workspace has unexported commits; refusing (use --force): $clone"
              fi
          fi
          # Refuse while any slot still has this clone bind-mounted. For a bind
          # mount `findmnt -no SOURCE` reports DEVICE[/subpath] (e.g.
          # `none[/var/lib/.../workspaces/<task>]`), never the bare path, so
          # strip the bracketed suffix before comparing. Cross-check the
          # recorded session workspace too, so an active slot bound to this
          # clone always blocks even if the mount source parsing changes.
          # When a slot still holds this clone (bind-mounted or recorded as the
          # running slot's workspace), --force tears that slot down first
          # (stop VM + unmount + drop slot transient, exactly like `stop`)
          # instead of refusing. Without --force we still fail closed and tell
          # the user which slot to stop.
          local slot mp src sess_ws matched
          for slot in "''${SLOT_NAMES[@]}"; do
              matched=0
              mp="$(mount_point "$slot")"
              if findmnt -n -- "$mp" >/dev/null 2>&1; then
                  src="$(findmnt -no SOURCE -- "$mp" 2>/dev/null || true)"
                  src="''${src##*[}"
                  src="''${src%]}"
                  [[ "$src" == "$clone" ]] && matched=1
              fi
              if service_active "$slot" && [[ -e "$(session_file "$slot")" ]]; then
                  sess_ws="$(jq -r '.workspace // ""' "$(session_file "$slot")" 2>/dev/null || true)"
                  [[ "$sess_ws" == "$clone" ]] && matched=1
              fi
              if (( matched )); then
                  if (( force )); then
                      log "workspace in use by slot $slot; stopping it first (--force)"
                      cleanup_slot "$slot"
                  else
                      die "workspace is in use by slot $slot; stop it first with: $PROG stop $slot"
                  fi
              fi
          done
          log "removing workspace $clone"
          rm -rf -- "$clone"
      }

      main() {
          [[ $# -ge 1 ]] || usage
          local cmd="$1"; shift
          case "$cmd" in
              run)              cmd_run "$@" ;;
              submit)           cmd_submit "$@" ;;
              cancel)           cmd_cancel "$@" ;;
              recover)          cmd_recover "$@" ;;
              stop)             cmd_stop "$@" ;;
              destroy)          cmd_destroy "$@" ;;
              status)           cmd_status "$@" ;;
              list)             cmd_list "$@" ;;
              ssh)              cmd_ssh "$@" ;;
              console)          cmd_console "$@" ;;
              workspace-remove) cmd_workspace_remove "$@" ;;
              -h|--help|help)   usage ;;
              *) die "unknown command '$cmd' (try '$PROG --help')" ;;
          esac
      }

      main "$@"
    '';

    meta = with lib; {
      description = "Host launcher / slot allocator for myconfig.ai.microvm agent sandboxes";
      maintainers = [ ];
      platforms = platforms.linux;
    };
  };
in
{
  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        # Host-side tool: it mounts, drives systemctl and manages runtime
        # state, so it belongs in the system environment (invoked via sudo).
        environment.systemPackages = [ agent-microvm ];
      }

      # --- optional passwordless operator control (secure default: off) ---
      # `agent-microvm` already REQUIRES root (mounts, systemctl on
      # microvm@<slot>.service, chown to the guest agent uid/gid, runtime
      # state) and is always run via sudo. When opted in, grant the host
      # primary operator a scoped NOPASSWD rule for exactly this one launcher
      # so the interactive workflow (and the workmux per-agent panes) no
      # longer prompt for a password. This is OPERATOR CONVENIENCE ONLY: the
      # untrusted guest can never invoke host sudo, so the guest/agent
      # isolation boundary is unchanged; the operator (already a full sudoer)
      # simply skips the password for this specific command.
      (lib.mkIf cfg.passwordlessControl {
        users.groups.agent-microvm = { };
        # Add the primary operator to the control group. Membership — not a
        # blanket `ALL` rule — is what gates the NOPASSWD rule below.
        users.users.${myconfig.user}.extraGroups = [ "agent-microvm" ];
        security.sudo.extraRules = [
          {
            groups = [ "agent-microvm" ];
            commands = [
              {
                # Match the PATH-resolved command the operator / workmux run
                # (`sudo agent-microvm ...`): sudoers `secure_path` resolves
                # the bare name to the current-system wrapper dir, exactly
                # like the existing shell.common systemctl NOPASSWD rules.
                command = "/run/current-system/sw/bin/agent-microvm";
                # SETENV is required so workmux's
                # `sudo --preserve-env=AGENT_MICROVM_SSH_KEY agent-microvm`
                # is accepted (without it sudo hard-fails on --preserve-env).
                # The target is a single fixed, shellcheck-gated launcher
                # that reads only a small known env set, so scoping SETENV to
                # just this command is a narrow, deliberate grant.
                options = [
                  "NOPASSWD"
                  "SETENV"
                ];
              }
            ];
          }
        ];
      })
    ]
  );
}
