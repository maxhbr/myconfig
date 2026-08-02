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

      # ==== configuration (from the Nix module options) ====================
      readonly WORKSPACE_ROOT=${lib.escapeShellArg cfg.workspaceRoot}
      readonly RUNTIME_ROOT=${lib.escapeShellArg cfg.runtimeRoot}
      readonly STATE_ROOT=${lib.escapeShellArg cfg.stateRoot}
      readonly SSH_ENABLED=${lib.escapeShellArg (if cfg.enableSsh then "1" else "0")}
      readonly SSH_USER="agent"
      # §11 UID/GID ownership: the guest `agent` user is uid/gid 1000
      # (guest.nix `users.users.agent`). The workspace clone is chowned to
      # these numeric ids so it appears agent-owned inside the guest via
      # virtiofs (which passes ownership through unchanged). Keep in sync with
      # guest.nix's `uid = 1000`.
      readonly GUEST_AGENT_UID=1000
      readonly GUEST_AGENT_GID=1000
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

      session_file() { printf '%s' "$SLOTS_DIR/$1/session.json"; }
      mount_point()  { printf '%s' "$STATE_ROOT/$1/workspace"; }

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
      validate_agent_name() {
          case "$1" in
              claude | pi | codex | opencode) return 0 ;;
              *) die "unknown --agent '$1' (expected: claude|pi|codex|opencode)" ;;
          esac
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
          printf '%s' "$clone"
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

      # ---- §21 slot cleanup / interrupt handling -------------------------
      # Tear a slot down WITHOUT deleting the workspace clone (§26/§35): stop
      # the VM, unmount the bind, remove the slot transient state. Locks are
      # released implicitly when this process exits and closes their fds.
      cleanup_slot() {
          local slot="$1"
          [[ -n "$slot" ]] || return 0
          stop_vm "$slot"
          teardown_bind_mount "$slot"
          rm -rf -- "''${SLOTS_DIR:?}/$slot"
      }

      # ---- readiness (§34) -----------------------------------------------
      guest_ssh_ready() {
          local ip="$1"
          [[ "$SSH_ENABLED" == "1" ]] || return 1
          ssh -o BatchMode=yes -o StrictHostKeyChecking=no \
              -o UserKnownHostsFile=/dev/null -o ConnectTimeout=3 \
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
        workspace-remove <task> [--force]
                              Delete the standalone clone. Separate + guarded:
                              refuses on uncommitted changes without --force.
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

          # --- §21 lock-protected allocation -------------------------------
          local slot="" name
          exec 9>"$ALLOC_LOCK"
          flock 9
          for name in "''${SLOT_NAMES[@]}"; do
              if slot_is_free "$name"; then slot="$name"; break; fi
          done
          if [[ -z "$slot" ]]; then
              flock -u 9
              die "no free slot (all ''${#SLOT_NAMES[@]} in use)"
          fi
          # Per-slot lock, held for the remainder of this process (§21).
          exec 8>"$RUN_DIR/$slot.lock"
          if ! flock -n 8; then
              flock -u 9
              die "slot $slot is locked by another launcher"
          fi

          # Arm the cleanup trap BEFORE creating any slot state, so a signal
          # in the marker-writing window still tears the slot down (keeping
          # the workspace clone; §43). cleanup_slot is a no-op for state that
          # does not exist yet.
          local committed=0
          # shellcheck disable=SC2317  # invoked via trap
          on_exit() { (( committed )) || cleanup_slot "$slot"; }
          trap on_exit EXIT
          trap 'exit 130' INT TERM

          mkdir -p -- "$SLOTS_DIR/$slot"
          # Atomic allocation marker written while the allocator lock is held,
          # so a concurrent 'run' can no longer pick this slot.
          jq -n --arg slot "$slot" --arg state "allocating" \
              '{slot:$slot, state:$state}' > "$(session_file "$slot")"
          flock -u 9
          exec 9>&-

          local clone ip mac
          ip="$(slot_ip "$slot")"
          mac="$(slot_mac "$slot")"
          log "allocated slot $slot ($ip)"
          clone="$(create_clone "$top" "$task" "$branch")"
          setup_bind_mount "$slot" "$clone"
          start_vm "$slot"

          # Persist the full session record for status/list (§34). No secrets.
          jq -n \
              --arg slot "$slot" --arg task "$task" --arg repo "$top" \
              --arg workspace "$clone" --arg mount "$(mount_point "$slot")" \
              --arg agent "$agent" --arg branch "$branch" \
              --arg ip "$ip" --arg mac "$mac" \
              --arg start "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
              --arg owner "$(id -un)($(id -u))" \
              '{slot:$slot, state:"running", task:$task, repository:$repo,
                workspace:$workspace, mount:$mount, agent:$agent, branch:$branch,
                ip:$ip, mac:$mac, start:$start, lock_owner:$owner}' \
              > "$(session_file "$slot")"

          if (( attach )); then
              [[ "$SSH_ENABLED" == "1" ]] || die "--attach requires enableSsh = true"
              [[ -n "$agent" ]] || die "--attach requires --agent <name>"
              # On readiness failure the EXIT trap runs cleanup_slot: the VM is
              # stopped and the bind unmounted, but the workspace clone is kept.
              wait_ready "$ip" || die "guest not ready; tearing down slot $slot (workspace kept at $clone)"
              log "attaching to $slot; running 'agent-run $agent' in /workspace"
              ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null \
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
          local slot f state task workspace agent start owner ip mac mnt ssh_ready
          for slot in "''${targets[@]}"; do
              f="$(session_file "$slot")"
              ip="$(slot_ip "$slot")"
              mac="$(slot_mac "$slot")"
              if service_active "$slot"; then state="running"; else state="stopped"; fi
              task=""; workspace=""; agent=""; start=""; owner=""
              local sstate="" stale="no"
              if [[ -e "$f" ]]; then
                  task="$(jq -r '.task // ""' "$f")"
                  workspace="$(jq -r '.workspace // ""' "$f")"
                  agent="$(jq -r '.agent // ""' "$f")"
                  start="$(jq -r '.start // ""' "$f")"
                  owner="$(jq -r '.lock_owner // ""' "$f")"
                  sstate="$(jq -r '.state // ""' "$f")"
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
        task:      ''${task:-<none>}
        workspace: ''${workspace:-<none>}
        bind-mount: $mnt
        agent:     ''${agent:-<none>}
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
          # StrictHostKeyChecking=no + /dev/null known-hosts is intentional:
          # slots are ephemeral guests with regenerated host keys, so
          # pinning would only add churn. Residual risk: the iptables/
          # br_netfilter firewall does NOT filter ARP, so without per-TAP L2
          # isolation (open item A1 in agent-microvm-remaining.md) a hostile
          # co-resident guest could ARP-spoof the gateway or another slot
          # and MITM this unpinned ssh/--attach session (agent prompts and
          # commands only — no secrets transit it, §17).
          exec ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null \
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
          local slot mp src sess_ws
          for slot in "''${SLOT_NAMES[@]}"; do
              mp="$(mount_point "$slot")"
              if findmnt -n -- "$mp" >/dev/null 2>&1; then
                  src="$(findmnt -no SOURCE -- "$mp" 2>/dev/null || true)"
                  src="''${src##*[}"
                  src="''${src%]}"
                  [[ "$src" == "$clone" ]] \
                      && die "workspace is still bind-mounted at slot $slot; stop it first"
              fi
              if service_active "$slot" && [[ -e "$(session_file "$slot")" ]]; then
                  sess_ws="$(jq -r '.workspace // ""' "$(session_file "$slot")" 2>/dev/null || true)"
                  [[ "$sess_ws" == "$clone" ]] \
                      && die "workspace is in use by running slot $slot; stop it first"
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
  config = lib.mkIf cfg.enable {
    # Host-side tool: it mounts, drives systemctl and manages runtime state,
    # so it belongs in the system environment (invoked via sudo).
    environment.systemPackages = [ agent-microvm ];
  };
}
