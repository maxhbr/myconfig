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
#        `git clone --local --no-hardlinks` (never `--shared`/`--reference`,
#        with a `--no-local` fallback when the source is being mutated);
#        verifies git-dir + git-common-dir resolve INSIDE the workspace and
#        that the clone borrows no objects (no `objects/info/alternates`).
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
  # The effective resource-class table (see default.nix).
  agentResourceClasses,
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
  # The ONE definition of the task-scoped agent-state paths (state.nix).
  agentState,
  # The ONE resolved network decision (profile + capabilities + DNS policy),
  # from default.nix (`_module.args.agentNetwork`). The host launcher's
  # endpoint preflight and `doctor` use `caps.litellm` so they only fire when
  # the effective profile actually grants model-API access.
  agentNetwork,
  # The ONE definition of the RUNTIME configuration staging (lightweight plan
  # phase 3), from config-seed.nix (`_module.args.agentConfigSeed`): the
  # per-slot staging directory and the BAKED, allowlist-enforcing stager this
  # launcher calls once per launch.
  agentConfigSeed,
  # The ONE definition of the CONSOLIDATED per-session tree (session.nix,
  # lightweight plan phase 4): the layout table this launcher prepares, verifies
  # (through the generated `agent-microvm-verify-session`) and removes as a
  # WHOLE, plus the two paths it bind-mounts (the workspace clone and the
  # task-scoped agent state).
  agentSession,
  ...
}:
let
  cfg = config.myconfig.ai.microvm;

  # The slot pool of the effective resource classes (ticket 5 A). The class
  # table comes from default.nix (`_module.args.agentResourceClasses`), which
  # also performs the legacy `slotCount` migration, so every module builds the
  # SAME pool.
  slots = (import ./slots.nix { inherit lib; }).mkSlots agentResourceClasses;

  # --- multi-line script FRAGMENTS ---------------------------------------
  # Several blocks of the generated script are defined next to the module data
  # they are rendered from (the config-seed policy, the session layout table)
  # rather than inline in the 3000-line script below. `mkFragment` re-indents
  # such a (Nix-dedented) block to the column it is spliced into and appends the
  # newline the following line's indentation needs, so the splice site keeps its
  # own indentation. It is PURE RENDERING — there is one launcher shape.
  indentFragment = indent: text: lib.replaceStrings [ "\n" ] [ "\n${indent}" ] text;
  mkFragment = indent: text: indentFragment indent (text + "\n");

  # Declarations + helpers, spliced into the helper section of the script.
  configSeedHelpers = mkFragment "      " ''
    # ---- runtime config staging (lightweight plan phase 3) -------------
    # The host copies an ALLOWLIST of configuration paths (the SELECTED
    # agents' registry `configPaths` plus `configSeed.extraPaths`) into a
    # cleaned per-slot directory that the guest sees through a READ-ONLY
    # virtiofs share; a root-owned guest oneshot copies it into the
    # disposable /home/agent before sshd and the job controller start.
    #
    # The POLICY is not here: the allowlist, the credential denylist, the
    # host home, the modes and the budgets are all BAKED into the stager by
    # Nix (config-seed.nix), which also re-validates them and refuses any
    # path that escapes the host home. This launcher can therefore only ask
    # for "stage slot X" — it cannot widen what gets staged, and no
    # caller-supplied path is ever expanded.
    readonly CONFIG_SEED_STAGER=${lib.getExe agentConfigSeed.stager}
    readonly CONFIG_SEED_ROOT=${lib.escapeShellArg agentConfigSeed.root}
    readonly CONFIG_SEED_PAYLOAD_SUBDIR=${lib.escapeShellArg agentConfigSeed.homeSubdir}
    # The manifest lives OUTSIDE every guest share, which means outside the
    # read-only tree too — hence its own root, taken from config-seed.nix
    # rather than derived from the payload path here.
    readonly CONFIG_SEED_MANIFEST_ROOT=${lib.escapeShellArg agentConfigSeed.manifestRoot}
    readonly CONFIG_SEED_MANIFEST_NAME=${lib.escapeShellArg agentConfigSeed.manifestName}
    config_seed_dir() { printf '%s' "$CONFIG_SEED_ROOT/$1"; }

    # Remove everything a previous task staged for this slot. Called before
    # every launch (the stager cleans again, so this is the second layer)
    # and on teardown, so a staged configuration never outlives its session.
    clear_config_seed() {
        local dir
        dir="$(config_seed_dir "$1")"
        # Every name and root comes from config-seed.nix, which OWNS them:
        # hardcoding any of them here would leave a stale file behind the day it
        # is renamed or moved.
        if [[ -d "$dir" ]]; then
            rm -rf -- "''${dir:?}/$CONFIG_SEED_PAYLOAD_SUBDIR"
        fi
        rm -f -- "$CONFIG_SEED_MANIFEST_ROOT/$1/$CONFIG_SEED_MANIFEST_NAME"
    }

    # Stage the CURRENT host configuration for this launch. Fails the launch
    # if it cannot: a guest that silently starts without its instructions is
    # worse than one that does not start.
    stage_config_seed() {
        local slot="$1"
        clear_config_seed "$slot"
        "$CONFIG_SEED_STAGER" "$slot" \
            || die "could not stage the host agent configuration for slot $slot"
    }
  '';

  # One call site each in `run` and `submit`, before the VM is started.
  configSeedStage = mkFragment "          " ''stage_config_seed "$slot"'';

  # --- the per-session tree (lightweight plan phase 4) --------------------
  # `install -d` line for ONE layout entry, generated from the SINGLE source of
  # truth (session.nix's table). `install -d` also RESETS the mode and ownership
  # of an existing directory, which is exactly what is needed: the guest agent
  # owns `worker/` and could have chmodded it during a previous session.
  # The two BIND-MOUNT points are only created when missing — they may still
  # carry a live bind at this point, and chmodding through it would change the
  # workspace clone or the task's persisted state.
  #
  # A failure RETURNS non-zero, it never `die`s: `prepare_session` is also
  # reached from `clear_session`, which runs inside the EXIT trap
  # (`cleanup_slot`). A `die` (i.e. `exit 1`) there would abort the trap before
  # `rm -rf "$SLOTS_DIR/$slot"`, reserving the slot FOREVER — the "second fault
  # on top of the leak" the teardown comments say must never happen. The call
  # sites decide what a failure means.
  sessionInstallLine =
    tree: e:
    let
      path = if e.rel == "" then "\"$dir\"" else "\"$dir/${e.rel}\"";
      install = "install -d -m ${e.mode} -o ${toString e.uid} -g ${toString e.gid} -- ${path}";
      guard = if e.strictMode then "" else "[[ -d ${path} ]] || ";
    in
    "${guard}${install} \\\n    || { log \"ERROR: could not prepare ${tree} ${
      if e.rel == "" then "root" else e.rel
    } of slot $slot\"; return 1; }";

  # The per-slot paths of the two trees, spliced into the configuration section
  # next to the other roots.
  sessionConfig = mkFragment "      " ''
    # ---- the per-session tree (lightweight plan phase 4) ---------------
    # ONE writable virtiofs share per slot (the session tree) plus ONE
    # read-only share (the slot's SSH host identity + the staged host
    # configuration). The trust boundaries are expressed by OWNERSHIP and
    # MODES, which virtiofsd passes through unchanged — see session.nix, which owns
    # the layout table every generated line below comes from.
    readonly SESSION_ROOT=${lib.escapeShellArg agentSession.root}
    readonly SESSION_RO_ROOT=${lib.escapeShellArg agentSession.roRoot}
    readonly SESSION_WORKSPACE_SUBDIR=${lib.escapeShellArg agentSession.subdirs.workspace}
    readonly SESSION_STATE_SUBDIR=${lib.escapeShellArg agentSession.subdirs.state}
    # The MODES of the two directories other parts of this launcher also touch
    # (`prepare_job` on the session root, `setup_agent_state` on the state bind
    # point). They are read from the layout table so no second authority over
    # them exists in this script.
    readonly SESSION_ROOT_MODE=${lib.escapeShellArg (agentSession.modeOf "")}
    readonly SESSION_WORKSPACE_MODE=${lib.escapeShellArg (agentSession.modeOf agentSession.subdirs.workspace)}
    readonly SESSION_STATE_MODE=${lib.escapeShellArg (agentSession.modeOf agentSession.subdirs.state)}
    # The generated PRE-LAUNCH verifier: it re-derives every expected owner and
    # mode from the same table and refuses the launch on any mismatch, symlink
    # or replaceable parent directory. The launcher cannot weaken it (it takes
    # no policy argument, only a slot name).
    readonly SESSION_VERIFIER=${lib.getExe agentSession.verifier}'';

  # WHERE a slot's two bind-mount targets live: both are INSIDE the session
  # tree, and there is exactly ONE definition of each in the generated script,
  # so `status`, `list`, `recover`, `doctor`, `destroy` and every teardown path
  # agree.
  mountPointDef = ''mount_point()  { printf '%s' "$SESSION_ROOT/$1/$SESSION_WORKSPACE_SUBDIR"; }'';
  stateSlotDirDef = ''state_slot_dir() { printf '%s' "$SESSION_ROOT/$1/$SESSION_STATE_SUBDIR"; }'';

  sessionHelpers = mkFragment "      " ''
    # ---- the session tree: preparation / verification / removal ---------
    session_dir()    { printf '%s' "$SESSION_ROOT/$1"; }
    session_ro_dir() { printf '%s' "$SESSION_RO_ROOT/$1"; }

    # Create (or RESET) the per-session tree with the exact ownership and modes
    # the trust split needs, before anything is staged into it.
    prepare_session() {
        local slot="$1" dir
        dir="$(session_dir "$slot")"
        ${lib.concatMapStringsSep "\n    " (sessionInstallLine "the session") agentSession.layout}
        dir="$(session_ro_dir "$slot")"
        ${lib.concatMapStringsSep "\n    " (sessionInstallLine "the read-only session")
          agentSession.roLayout
        }
    }

    # Fail CLOSED before the VM (and therefore virtiofsd) starts: a tree whose
    # ownership/modes are not exactly what the trust split needs must never be
    # handed to a guest.
    verify_session() {
        "$SESSION_VERIFIER" "$1" \
            || die "the session tree of slot $1 failed its pre-launch ownership/mode verification"
    }

    # NOTHING may still be mounted anywhere under a tree we are about to
    # `rm -rf`: the removal would descend THROUGH the mount and destroy the
    # user's clone or a task's persisted state, both of which live outside the
    # tree and must survive it. `rm --one-file-system` is no help here — a bind
    # mount of a same-filesystem directory shares its st_dev. The two binds this
    # module creates are unmounted (and verified) by the caller; this catches
    # every OTHER mount that could be under the tree (an operator's manual
    # mount, a nested bind, a future third share).
    session_subtree_unmounted() {
        local dir="$1" target rc=0
        while read -r target; do
            [[ -n "$target" ]] || continue
            if [[ "$target" == "$dir" || "$target" == "$dir"/* ]]; then
                log "ERROR: a mount is still present under the session tree $dir: $target"
                rc=1
            fi
        done < <(findmnt -rn -o TARGET 2>/dev/null || true)
        return "$rc"
    }

    # Remove the COMPLETE per-session tree and recreate the empty skeleton.
    # Both binds must be VERIFIABLY gone first: deleting through a live bind
    # would destroy the workspace clone or the task's persisted state (which
    # both live outside the tree and must survive it). The skeleton is
    # recreated because virtiofsd refuses to start without its share source.
    #
    # ORDER MATTERS in `cleanup_slot`: the config-seed clear runs BEFORE this,
    # because it `rm -rf`s the payload subdirectory of the READ-ONLY slot
    # directory and the `prepare_session` call at the end of this function is
    # what puts that (empty) subdirectory back. Reversed, the next launch would
    # be refused by `verify_session`, which requires every directory of both
    # layout tables to exist.
    clear_session() {
        local slot="$1" dir
        dir="$(session_dir "$slot")"
        unmount_verified "$(mount_point "$slot")" "$slot" || {
            log "ERROR: refusing to remove the session tree of $slot while its workspace bind mount survives"
            return 1
        }
        unmount_verified "$(state_slot_dir "$slot")" "$slot" || {
            log "ERROR: refusing to remove the session tree of $slot while its agent-state bind mount survives"
            return 1
        }
        session_subtree_unmounted "$dir" || {
            log "ERROR: refusing to remove the session tree of $slot while a mount survives underneath it"
            return 1
        }
        if [[ -d "$dir" ]]; then
            rm -rf -- "''${dir:?}" || {
                log "ERROR: could not remove the session tree $dir"
                return 1
            }
        fi
        if [[ -e "$dir" || -L "$dir" ]]; then
            log "ERROR: the session tree $dir still exists after removing it"
            return 1
        fi
        # virtiofsd refuses to start without its share source, so the empty
        # skeleton must come back. A failure here is REPORTED, never fatal: this
        # runs from the EXIT trap, which has to finish releasing the slot.
        prepare_session "$slot" || {
            log "ERROR: could not recreate the session skeleton of $slot after removing it"
            return 1
        }
    }

    # Bytes of the per-slot job data for `usage`. `du` descends into a bind
    # mount whose source is on the SAME filesystem (they share st_dev, so
    # --one-file-system does not stop it), and under this layout the per-slot
    # job directory CONTAINS the workspace clone and the task's agent state —
    # both already reported on their own `usage` lines. Excluding them keeps
    # the "job data" figure meaning what it says instead of double-counting
    # every byte of the clone.
    session_job_data_bytes() {
        [[ -d "$SESSION_ROOT" ]] || { printf '0'; return 0; }
        du -sb --one-file-system \
            --exclude="*/$SESSION_WORKSPACE_SUBDIR" \
            --exclude="*/$SESSION_STATE_SUBDIR" \
            -- "$SESSION_ROOT" 2>/dev/null | cut -f1
    }

    # NOTE on the FOREIGN per-slot state scan further down: $JOBS_ROOT is the
    # writable session root and $HOSTKEYS_ROOT the read-only one, so the scan
    # covers both trees. $STATE_SLOTS_ROOT and the
    # `$STATE_ROOT/agent-*/workspace` branch find nothing on a host that has
    # only ever run this layout — they are kept DELIBERATELY, because a host
    # migrated from the historical four-share layout still carries residue
    # there, and reporting it is exactly what those branches are for.
  '';

  # One call site each in `run` and `submit`, BEFORE the workspace bind mount
  # and before anything is staged into the tree.
  sessionPrepare = mkFragment "          " ''
    prepare_session "$slot" \
        || die "could not prepare the session tree of slot $slot"
    # The clone is about to be bind-mounted ONTO a directory of the session
    # tree, and a mount point shows the MOUNTED tree's root mode — which the
    # pre-launch verifier rejects if it grants group/other WRITE. That mode
    # comes from `git clone`, i.e. from root's umask and from what the SOURCE
    # repository does (`core.sharedRepository` makes git create group-writable
    # directories), neither of which this launcher controls. Normalise the clone
    # ROOT to the mode the layout table declares for the mount point, so a
    # legitimately shared source repository cannot produce a confusing
    # pre-launch refusal. Only the ROOT: the modes inside the clone are the
    # user's business.
    chmod "$SESSION_WORKSPACE_MODE" -- "$clone" \
        || die "could not normalise the mode of the workspace clone root: $clone"'';
  # ... and one immediately before the VM is started.
  sessionVerify = mkFragment "          " ''verify_session "$slot"'';
  # ... and one in the teardown, so no per-session data outlives the session.
  sessionClear = mkFragment "          " ''clear_session "$slot" || leaked=1'';

  # The MODE of the two directories the rest of this launcher also creates and
  # which the layout table already declares: they resolve to the table's value
  # at RUNTIME, so session.nix stays the only authority over them.
  jobRootModeArg = ''"$SESSION_ROOT_MODE"'';
  stateSlotModeArg = ''"$SESSION_STATE_MODE"'';
  # The state directory is a BIND-MOUNT POINT that `prepare_session` already
  # created with the table's owner/mode. `install -d` would chmod/chown THROUGH
  # a surviving stale bind (i.e. through the previous task's persisted state), so
  # only create it when missing — the same treatment session.nix's table gives
  # every non-`strictMode` entry, and the pre-launch verifier still fails the
  # launch if the owner is wrong.
  stateSlotInstallGuard = ''[[ -d "$mp" ]] || '';

  # `usage`' "job data" line. The per-slot job directory contains the workspace
  # and agent-state BIND MOUNTS, which `du` descends into, so the figure is
  # computed by a helper that excludes them.
  jobDataUsageLine = ''printf '  job data:      %s (%s)\n' "$JOBS_ROOT" "$(human "$(session_job_data_bytes)")"'';

  # ... and one in the teardown, so nothing staged survives the session.
  configSeedClear = mkFragment "          " ''clear_config_seed "$slot" || true'';

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
      curl # endpoint preflight + `doctor` HTTP probes
      iproute2 # ip, bridge — `doctor` bridge / address / TAP checks
      iptables # `doctor` firewall-chain inspection (read-only)
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
      # Resource class of each slot plus its prebuilt sizing (ticket 5 A). The
      # allocator only ever considers slots of the REQUESTED class — it never
      # silently substitutes a smaller one.
      readonly SLOT_CLASSES=(${bashList "class"})
      readonly SLOT_VCPUS=(${bashList "vcpu"})
      readonly SLOT_MEMS=(${bashList "memoryMiB"})
      # Alphabetically ordered class names, generated from the module options.
      readonly RESOURCE_CLASSES=(${
        lib.concatMapStringsSep " " lib.escapeShellArg (lib.attrNames agentResourceClasses)
      })
      readonly DEFAULT_RESOURCE_CLASS=${lib.escapeShellArg (lib.head (lib.attrNames agentResourceClasses))}

      # ==== configuration (from the Nix module options) ====================
      readonly WORKSPACE_ROOT=${lib.escapeShellArg cfg.workspaceRoot}
      readonly RUNTIME_ROOT=${lib.escapeShellArg cfg.runtimeRoot}
      readonly STATE_ROOT=${lib.escapeShellArg cfg.stateRoot}
      # The private bridge + the bridge-only LiteLLM endpoint a guest reaches
      # (network.nix §16, guest.nix forwarder). The host launcher's endpoint
      # preflight and `doctor` probe exactly this address, and the per-task
      # stderr surfacing labels the endpoint in its hint.
      readonly BRIDGE=${lib.escapeShellArg cfg.bridgeName}
      readonly GATEWAY=${lib.escapeShellArg cfg.gatewayAddress}
      readonly LITELLM_PORT=${toString cfg.litellmPort}
      # The private subnet the guest slots live in (network.nix). `doctor`
      # builds the EXACT `iptables -C AGENT_MICROVM_INPUT -s $SUBNET -d
      # $GATEWAY -p tcp --dport $LITELLM_PORT -j ACCEPT` spec from these SAME
      # variables network.nix installs the rule with (its `inputAllowLines`),
      # so the check and the rule it verifies can never drift apart.
      readonly SUBNET=${lib.escapeShellArg cfg.subnet}
      # Whether the effective profile grants model-API access at all. Under
      # `offline` there is no endpoint to probe, so the preflight/doctor skip
      # the LiteLLM checks rather than reporting a spurious failure.
      readonly LITELLM_CAPABLE=${if agentNetwork.caps.litellm then "1" else "0"}
      readonly NETWORK_PROFILE=${lib.escapeShellArg agentNetwork.profile}
      # Bounded connect timeout (seconds) for a single preflight attempt. Short
      # on purpose: a reachable endpoint answers in well under a second; a dead
      # one must fail fast, not hold up boot for the full TCP retransmit cycle.
      readonly PREFLIGHT_TIMEOUT=3
      # A COLD LiteLLM (DB init/migration on the first post-boot request) can
      # take a few seconds to answer even though it is healthy. Retry a bounded
      # number of times so a slow-but-working endpoint is not mistaken for a
      # dead one; a genuinely dead endpoint still fails within
      # PREFLIGHT_RETRIES*PREFLIGHT_TIMEOUT + sleeps, which is negligible vs.
      # booting a doomed VM (the 95-min real-KVM runs this was built for).
      readonly PREFLIGHT_RETRIES=3
      readonly PREFLIGHT_RETRY_DELAY=2
      # Escape hatch for the EXECUTED test harnesses (tests/microvm-batch-*.sh):
      # they run the REAL launcher against a stubbed environment with no real
      # LiteLLM listener, so the production preflight would abort every
      # result-channel scenario before it started. Production callers MUST NOT
      # set this — a skipped preflight is strictly less safe than a real one.
      readonly SKIP_PREFLIGHT="''${AGENT_MICROVM_SKIP_PREFLIGHT:-0}"
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
      # Per-slot host-key directory root; scanned by the foreign-slot report.
      readonly HOSTKEYS_ROOT=${lib.escapeShellArg agentHostKeys.root}
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
      # the `guestAgentUid`/`guestAgentGid` options guest.nix uses.
      readonly GUEST_AGENT_UID=${toString cfg.guestAgentUid}
      readonly GUEST_AGENT_GID=${toString cfg.guestAgentGid}
      # ---- batch jobs (ticket 4; result channel hardened in ticket 7) ------
      # The job share is split into FOUR areas with different owners, and that
      # split IS the security boundary (see job.nix):
      #   input/       root:root — the immutable spec (0400, it carries the
      #                allocation token) and prompt (0444)
      #   controller/  root:root 0700 — the guest CONTROLLER's private channel;
      #                the AUTHORITATIVE result lives here and the untrusted
      #                guest agent can neither write nor read it
      #   worker/      the guest agent — untrusted artifacts, never read as a
      #                result
      #   worker-logs/ root:root 0755 — the worker's stdout/stderr, opened as
      #                root by the guest's systemd; untrusted CONTENT, but no
      #                path the guest agent could redirect
      readonly JOBS_ROOT=${lib.escapeShellArg agentJobs.root}
      readonly RESULTS_DIR=${lib.escapeShellArg agentJobs.resultsDir}
      readonly JOB_INPUT_SUBDIR=${lib.escapeShellArg agentJobs.inputSubdir}
      readonly JOB_CONTROLLER_SUBDIR=${lib.escapeShellArg agentJobs.controllerSubdir}
      readonly JOB_WORKER_SUBDIR=${lib.escapeShellArg agentJobs.workerSubdir}
      readonly JOB_WORKER_LOGS_SUBDIR=${lib.escapeShellArg agentJobs.workerLogsSubdir}
      readonly JOB_WORKER_STDERR_NAME=${lib.escapeShellArg agentJobs.workerStderrName}
      readonly JOB_SPEC_NAME=${lib.escapeShellArg agentJobs.specName}
      readonly JOB_PROMPT_NAME=${lib.escapeShellArg agentJobs.promptName}
      readonly JOB_CANCEL_NAME=${lib.escapeShellArg agentJobs.cancelName}
      readonly JOB_RESULT_NAME=${lib.escapeShellArg agentJobs.resultName}
      readonly JOB_CTRL_STATE_NAME=${lib.escapeShellArg agentJobs.controllerStateName}
      readonly JOB_SPEC_MODE=${lib.escapeShellArg agentJobs.specMode}
      readonly JOB_PROMPT_MODE=${lib.escapeShellArg agentJobs.promptMode}
      readonly JOB_CANCEL_MODE=${lib.escapeShellArg agentJobs.cancelMode}
      readonly JOB_INPUT_DIR_MODE=${lib.escapeShellArg agentJobs.inputDirMode}
      readonly JOB_CONTROLLER_DIR_MODE=${lib.escapeShellArg agentJobs.controllerDirMode}
      readonly JOB_WORKER_DIR_MODE=${lib.escapeShellArg agentJobs.workerDirMode}
      readonly JOB_WORKER_LOGS_DIR_MODE=${lib.escapeShellArg agentJobs.workerLogsDirMode}
      # Mode of an ARCHIVED result: it carries the allocation token of the run
      # it belongs to, so it is root-only — like the archive directory itself.
      readonly JOB_ARCHIVE_MODE=0600
      readonly JOB_ARCHIVE_DIR_MODE=0700
      readonly JOB_SPEC_VERSION=${toString agentJobs.specVersion}
      readonly JOB_CONTROLLER_VERSION=${toString agentJobs.controllerVersion}
      # THE one result parser (job.nix). Every read of a guest-written document
      # goes through it, so there is exactly one place that decides whether a
      # result belongs to the active allocation.
      readonly RESULT_VERIFIER=${lib.getExe agentJobs.resultVerifier}
      # Guest-side prompt path, as it must appear in spec.json (the guest
      # validates it by EXACT match against its own mount point).
      readonly GUEST_PROMPT=${lib.escapeShellArg agentJobs.guestPrompt}
      readonly GUEST_WORKSPACE=/workspace
      readonly JOB_DEFAULT_TIMEOUT=${toString cfg.job.defaultTimeoutSeconds}
      readonly JOB_MAX_TIMEOUT=${toString cfg.job.maxTimeoutSeconds}
      readonly JOB_GRACE=${toString cfg.job.gracePeriodSeconds}
      # Bounded window `cancel` waits for the guest controller's own
      # `cancelled` result before it force-stops the VM.
      readonly CANCEL_WAIT=20
      # ---- task-scoped agent state (ticket 5 B) --------------------------
      readonly STATE_TASKS_ROOT=${lib.escapeShellArg agentState.tasksRoot}
      readonly STATE_SLOTS_ROOT=${lib.escapeShellArg agentState.slotsRoot}
      # The DECLARED per-agent state directories, as "<agent>:<dir>" pairs
      # generated from the registry. Only these are ever exposed to a guest.
      readonly AGENT_STATE_DIRS=(${
        lib.concatMapStringsSep " " lib.escapeShellArg (
          lib.concatMap (a: map (d: "${a.name}:${d}") a.persistentState.directories) (
            lib.attrValues agentRegistry.agents
          )
        )
      })
      ${sessionConfig}# ---- structured lifecycle logs (ticket 6 B) ------------------------
      readonly LOGS_DIR="$RUNTIME_ROOT/logs"
      readonly LOG_MAX_BYTES=${toString cfg.taskLogMaxBytes}
      readonly RUN_DIR="/run/agent-microvms"
      readonly ALLOC_LOCK="$RUN_DIR/allocator.lock"
      readonly SLOTS_DIR="$RUNTIME_ROOT/slots"
      # Bounded clean-shutdown window (seconds) before we force-kill the unit.
      readonly SHUTDOWN_TIMEOUT=30
      # Bounded guest-readiness window (seconds) when waiting for SSH.
      readonly READY_TIMEOUT=90
      # Exponential-backoff bounds (milliseconds) of that wait: the first probe
      # follows almost immediately, so a warm slot no longer pays a fixed
      # multi-second sleep after it is already reachable, while a cold boot
      # backs off to one probe every 2 s instead of hammering sshd.
      readonly READY_POLL_MIN_MS=250
      readonly READY_POLL_MAX_MS=2000

      PROG="agent-microvm"

      die() {
          printf '%s: error: %s\n' "$PROG" "$*" >&2
          exit 1
      }
      log() { printf '%s: %s\n' "$PROG" "$*" >&2; }

      # ==== structured lifecycle events (ticket 6 B) =======================
      # One JSON object per state transition, emitted to
      #   * the operator's stderr (so an interactive run shows its own history),
      #   * the systemd JOURNAL under the `agent-microvm` tag (discoverable with
      #     `journalctl -t agent-microvm`), and
      #   * for batch tasks, a BOUNDED per-task log
      #     <runtimeRoot>/logs/<task>.jsonl (rotated at $LOG_MAX_BYTES, one
      #     generation kept), so a finished job's history survives the slot.
      #
      # Deliberately NEVER logged: api keys, prompts (only the prompt FILE's
      # path and byte size), repository credentials, secret env vars, private
      # key material. `emit_event` takes only scalar, non-secret fields.
      #
      # EV_* are the ambient fields of the current operation; subcommands set
      # them once so every later event carries the same identity.
      EV_TASK=""
      EV_SLOT=""
      EV_AGENT=""
      EV_CLASS=""
      EV_MODE=""
      set_event_context() {
          EV_TASK="''${1-}"; EV_SLOT="''${2-}"; EV_AGENT="''${3-}"
          EV_CLASS="''${4-}"; EV_MODE="''${5-}"
      }

      # emit_event <event> [<state>] [<exit_code>] [<message>]
      emit_event() {
          local event="$1" state="''${2-}" exit_code="''${3-}" message="''${4-}"
          local line
          line="$(jq -nc \
              --arg ts "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
              --arg event "$event" \
              --arg task "$EV_TASK" --arg slot "$EV_SLOT" --arg agent "$EV_AGENT" \
              --arg class "$EV_CLASS" --arg mode "$EV_MODE" \
              --arg state "$state" --arg exit_code "$exit_code" \
              --arg message "$message" \
              '{ts:$ts, event:$event, task:$task, slot:$slot, agent:$agent,
                resource_class:$class, mode:$mode, state:$state,
                exit_code:$exit_code, message:$message}
               | with_entries(select(.value != ""))')"
          printf '%s\n' "$line" >&2
          # Journal: best-effort, never fatal (a launcher must work even when
          # the journal is unavailable).
          logger --tag "$PROG" --priority user.info -- "$line" 2>/dev/null || true
          append_task_log "$line"
      }

      # Bounded per-task log: rotate ONE generation once the file exceeds
      # $LOG_MAX_BYTES, so a long-running or noisy task cannot fill the disk.
      append_task_log() {
          local line="$1" f
          [[ -n "$EV_TASK" ]] || return 0
          [[ -d "$LOGS_DIR" ]] || mkdir -p -- "$LOGS_DIR" || return 0
          f="$LOGS_DIR/$EV_TASK.jsonl"
          if [[ -f "$f" ]]; then
              local size
              size="$(stat -c %s -- "$f" 2>/dev/null || echo 0)"
              if (( size >= LOG_MAX_BYTES )); then
                  mv -f -- "$f" "$f.1" 2>/dev/null || true
              fi
          fi
          printf '%s\n' "$line" >> "$f" 2>/dev/null || true
      }

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
      slot_class() { local i; i="$(slot_index "$1")" || return 1; printf '%s' "''${SLOT_CLASSES[$i]}"; }
      slot_vcpu()  { local i; i="$(slot_index "$1")" || return 1; printf '%s' "''${SLOT_VCPUS[$i]}"; }
      slot_mem()   { local i; i="$(slot_index "$1")" || return 1; printf '%s' "''${SLOT_MEMS[$i]}"; }

      # ---- resource classes (ticket 5 A) ---------------------------------
      validate_resource_class() {
          local want="$1" c
          for c in "''${RESOURCE_CLASSES[@]}"; do
              [[ "$c" == "$want" ]] && return 0
          done
          die "unknown --resource-class '$want' (expected: $(IFS='|'; echo "''${RESOURCE_CLASSES[*]}"))"
      }

      session_file() { printf '%s' "$SLOTS_DIR/$1/session.json"; }
      ${mountPointDef}
      job_dir()      { printf '%s' "$JOBS_ROOT/$1"; }
      job_input_dir()      { printf '%s' "$JOBS_ROOT/$1/$JOB_INPUT_SUBDIR"; }
      job_controller_dir() { printf '%s' "$JOBS_ROOT/$1/$JOB_CONTROLLER_SUBDIR"; }
      job_worker_dir()     { printf '%s' "$JOBS_ROOT/$1/$JOB_WORKER_SUBDIR"; }
      # The worker's stdout/stderr: ROOT-owned, next to (never inside) the
      # worker-writable dir, because the guest's systemd opens them as root.
      job_worker_logs_dir() { printf '%s' "$JOBS_ROOT/$1/$JOB_WORKER_LOGS_SUBDIR"; }
      job_spec()     { printf '%s' "$(job_input_dir "$1")/$JOB_SPEC_NAME"; }
      job_prompt()   { printf '%s' "$(job_input_dir "$1")/$JOB_PROMPT_NAME"; }
      job_cancel()   { printf '%s' "$(job_input_dir "$1")/$JOB_CANCEL_NAME"; }
      # THE authoritative result path: inside the controller-only directory.
      # Nothing else is ever read as a result — in particular nothing under
      # $(job_worker_dir) or /workspace, which the untrusted agent can write.
      job_result()   { printf '%s' "$(job_controller_dir "$1")/$JOB_RESULT_NAME"; }
      job_ctrl_state() { printf '%s' "$(job_controller_dir "$1")/$JOB_CTRL_STATE_NAME"; }

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

      # 256 bits of kernel randomness, hex-encoded. This is the ALLOCATION
      # TOKEN: it identifies one allocation of one slot, is recorded in the
      # session marker, handed to the guest in the (root-only) job spec, and
      # must reappear in the guest's result. It is what makes a stale or
      # cross-allocation result — and a stale cancellation — harmless. Never
      # logged.
      new_token() { od -An -tx1 -N32 /dev/urandom | tr -d ' \n'; }

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
      # Args: <resource-class> <wait-seconds>. Only slots of THAT class are
      # considered (never a different/smaller one). With wait-seconds > 0 the
      # allocation is retried, releasing the global lock between attempts, until
      # the bounded window expires.
      allocate_slot() {
          local want_class="$1" wait_for="$2" name waited=0
          ALLOC_SLOT=""
          ALLOC_TOKEN="$(new_token)"
          mkdir -p -- "$RUN_DIR" "$SLOTS_DIR"
          while :; do
              exec 9>"$ALLOC_LOCK"
              flock 9
              for name in "''${SLOT_NAMES[@]}"; do
                  [[ "$(slot_class "$name")" == "$want_class" ]] || continue
                  if slot_is_free "$name"; then ALLOC_SLOT="$name"; break; fi
              done
              [[ -n "$ALLOC_SLOT" ]] && break
              # Nothing free in the requested class: release the lock so another
              # launcher can finish, then either wait or fail loudly.
              flock -u 9
              exec 9>&-
              if (( waited >= wait_for )); then
                  local waited_note=""
                  (( wait_for > 0 )) && waited_note=" after ''${wait_for}s"
                  die "no free slot in resource class '$want_class' ($(class_slot_count "$want_class") slot(s) total)$waited_note"
              fi
              sleep 5
              waited=$(( waited + 5 ))
          done
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
          # The token is handed to jq in the ENVIRONMENT, never in argv (see
          # write_session_marker).
          ALLOC_TOKEN="$ALLOC_TOKEN" jq -n --arg slot "$ALLOC_SLOT" \
              '{slot:$slot, state:"allocating", token:$ENV.ALLOC_TOKEN}' \
              > "$(session_file "$ALLOC_SLOT")"
          flock -u 9
          exec 9>&-
      }

      class_slot_count() {
          local want="$1" n=0 name
          for name in "''${SLOT_NAMES[@]}"; do
              [[ "$(slot_class "$name")" == "$want" ]] && n=$(( n + 1 ))
          done
          printf '%s' "$n"
      }

      # Full session record for status/list/cancel/recover (§34 + ticket 4).
      # Contains NO secrets: task, slot, workspace, unit, mode, agent, timeout,
      # the owning launcher's pid + pid start time, and the allocation token.
      write_session_marker() {
          local slot="$1" token="$2" mode="$3" task="$4" repo="$5" clone="$6" \
                agent="$7" branch="$8" timeout_s="$9" persist="''${10:-0}"
          local tmp
          tmp="$(mktemp "$SLOTS_DIR/$slot/.session.XXXXXX")"
          # ALLOC_TOKEN goes through the ENVIRONMENT, never through argv:
          # /proc/<pid>/cmdline is world-readable (0444) for every local user,
          # /proc/<pid>/environ is 0400. The token identifies the ACTIVE
          # allocation, so it must not be readable off a running launcher.
          ALLOC_TOKEN="$token" jq -n \
              --arg slot "$slot" --arg task "$task" --arg repo "$repo" \
              --arg workspace "$clone" --arg mount "$(mount_point "$slot")" \
              --arg agent "$agent" --arg branch "$branch" \
              --arg ip "$(slot_ip "$slot")" --arg mac "$(slot_mac "$slot")" \
              --arg start "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
              --arg owner "$(id -un)($(id -u))" \
              --arg mode "$mode" \
              --arg unit "microvm@$slot.service" \
              --arg pid "$$" --arg pid_start "$(proc_start_time "$$")" \
              --arg timeout "$timeout_s" \
              --arg persist "$persist" \
              '{slot:$slot, state:"running", task:$task, repository:$repo,
                workspace:$workspace, mount:$mount, agent:$agent, branch:$branch,
                ip:$ip, mac:$mac, start:$start, lock_owner:$owner,
                token:$ENV.ALLOC_TOKEN, mode:$mode, unit:$unit, pid:$pid,
                pid_start:$pid_start, timeout:$timeout,
                persist_agent_state:$persist}' > "$tmp"
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

      # A bounded allocation wait: 0 (fail immediately) or a positive integer.
      validate_wait() {
          local w="$1"
          [[ "$w" =~ ^[0-9]+$ ]] || die "--wait must be a non-negative integer (got '$w')"
          (( w <= JOB_MAX_TIMEOUT )) || die "--wait $w is unreasonably long (max $JOB_MAX_TIMEOUT)"
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
          # A clone that BORROWS objects (`--shared` / `--reference`, i.e. an
          # `objects/info/alternates` file) would still read from the original
          # repository at runtime and would break the moment the source is
          # gc'ed — the exact opposite of "standalone". `clone --local
          # --no-hardlinks` never writes one; verify it anyway, because this is
          # the invariant that makes the clone disposable.
          [[ ! -e "$gcd/objects/info/alternates" ]] \
              || die "clone borrows objects from another repository (alternates present): $gcd"
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
          # `--local --no-hardlinks` COPIES the object store: no hardlinks into
          # the source repository and — crucially — no `--shared` / `--reference`
          # and therefore no `objects/info/alternates`, so the clone is exactly
          # as independent and disposable as the previous `--no-local` transfer
          # while being roughly an order of magnitude faster on this repo (0.6 s
          # vs 5 s), because it skips pack negotiation and re-compression.
          # `verify_clone` below re-checks the independence invariants (git-dir,
          # git-common-dir, absence of alternates) rather than trusting the flag.
          #
          # Git's local clone reads the source's loose objects and packs
          # directly, so a source repository that is MUTATED concurrently (a
          # running `git gc`, a rebase in another worktree) can yield an
          # inconsistent copy. That surfaces as a failing clone, which is why
          # the fallback below redoes the clone through the ordinary git
          # transport (`--no-local`), which is consistency-checked by the
          # protocol itself.
          #
          # The scoped safe.directory covers reading the user-owned SOURCE
          # repo as root (dubious-ownership check); the fresh clone itself is
          # root-owned at this point, so it needs no override. All later git
          # calls in create_clone/verify_clone (rev-parse, checkout -b) also
          # run on the root-owned clone BEFORE the chown to 1000:1000 below.
          if ! git -c safe.directory="$repo" clone --local --no-hardlinks -- "$repo" "$clone"; then
              log "warning: fast local clone failed (is the source repository being mutated?); retrying via the git transport"
              rm -rf -- "$clone"
              git -c safe.directory="$repo" clone --no-local -- "$repo" "$clone" \
                  || die "git clone failed"
          fi
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
      # Release whatever still holds a slot's shared directories open.
      #
      # microvm.nix runs ONE virtiofsd unit per VM,
      # `microvm-virtiofsd@<slot>.service` (see its nixos-modules/host, which
      # declares the `microvm-virtiofsd@` template with `Restart=always` and
      # `partOf = microvm@%i.service`). Because it is `partOf`, a clean
      # `systemctl stop microvm@<slot>` already takes it down — but a SIGKILLed
      # guest does NOT: `microvm@<slot>` goes to `failed` without propagating a
      # stop, `Restart=always` brings virtiofsd back, and it keeps the workspace
      # bind open. Stopping it explicitly is idempotent and safe when the unit
      # does not exist.
      #
      # CAUTION: `microvm@%i` REQUIRES `microvm-virtiofsd@%i` (pinned
      # microvm.nix, nixos-modules/host/default.nix), so stopping virtiofsd also
      # stops a LIVE VM on that slot. That is correct on every path that reaches
      # here — all of them have already run stop_vm (cleanup_slot, recover) or
      # are re-binding a slot that is not running (setup_bind_mount,
      # setup_agent_state) — but it makes this a teardown-only helper: never
      # call it while a VM is meant to keep running.
      release_mount_holders() {
          systemctl stop "microvm-virtiofsd@$1.service" 2>/dev/null || true
      }

      # Unmount $1 and PROVE it is gone. Returns 0 when nothing is mounted there
      # any more (either it never was, or we unmounted it), 1 when the mount
      # SURVIVED — which the caller must report, never swallow.
      #
      # A lazy unmount is deliberately NOT used as a fallback. `umount -l`
      # detaches the mount from the namespace but keeps it alive until the last
      # reference is dropped, so `findmnt` still lists it and the kernel still
      # holds the clone. Every caller here either re-binds the same mount point
      # (setup_bind_mount) or tells the operator "no stale mount remains"
      # (recover) — both are lies while the mount lives on. So the only accepted
      # outcomes are "gone" and "reported".
      unmount_verified() {
          local mp="$1" slot="''${2-}"
          findmnt -n -- "$mp" >/dev/null 2>&1 || return 0
          if ! umount -- "$mp" 2>/dev/null; then
              # EBUSY is the expected case after a hard-killed guest: virtiofsd
              # still holds the share. Drop that reference and retry ONCE.
              [[ -n "$slot" ]] && release_mount_holders "$slot"
              umount -- "$mp" 2>/dev/null || true
          fi
          if findmnt -n -- "$mp" >/dev/null 2>&1; then
              log "ERROR: $mp is STILL mounted after unmounting it — something is holding it open (a lazy unmount would hide this, so it is not used)"
              # The slot is set as STRUCTURED context (not only mentioned in the
              # free-text message), so a consumer of the lifecycle stream can
              # act on the event without parsing prose. The rest of the context
              # (task/agent/class/mode) is whatever the caller established.
              if [[ -n "$slot" ]]; then EV_SLOT="$slot"; fi
              emit_event mount-leak "" "" "could not unmount $mp"
              return 1
          fi
          return 0
      }

      setup_bind_mount() {
          local slot="$1" clone="$2" mp
          mp="$(mount_point "$slot")"
          mkdir -p -- "$mp"
          unmount_verified "$mp" "$slot" \
              || die "could not unmount stale bind at $mp"
          mount --bind -- "$clone" "$mp" || die "bind mount failed: $clone -> $mp"
          findmnt -n -- "$mp" >/dev/null 2>&1 \
              || die "bind mount verification failed: $mp"
      }

      # Returns non-zero when the mount survived (see unmount_verified). The
      # cleanup path deliberately CONTINUES on failure — dropping the allocation
      # marker matters more than the leaked mount, and leaving the slot reserved
      # forever would be a second fault — but it must never report success.
      teardown_bind_mount() {
          unmount_verified "$(mount_point "$1")" "$1"
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
          # guest config, which looks like "the sandbox is not provisioned". The unit is an
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

      # ---- batch-job data (ticket 4 / hardened in ticket 7) ----------------
      # Lays out the slot's job directory and writes the versioned spec + the
      # prompt into its IMMUTABLE input area. The prompt is COPIED, never passed
      # as an argument, and neither file ever enters the Nix store.
      #
      # The MODES are load-bearing (virtiofsd passes ownership through, so these
      # are the effective permissions inside the guest):
      #   input/       root:root 0755  — spec 0400 (it carries the allocation
      #                token, which the untrusted agent must not learn),
      #                prompt 0444 (the worker must read it)
      #   controller/  root:root 0700  — the guest controller's private channel;
      #                the authoritative result. NOT writable and NOT readable
      #                by the guest agent, and it cannot be renamed either
      #                because its parent is root-owned 0755.
      #   worker/      agent-owned     — untrusted artifacts
      #   worker-logs/ root:root 0755  — the worker's stdout/stderr. ROOT-owned
      #                because the guest's systemd opens them as root (and
      #                follows symlinks), so no path component may be
      #                creatable/renameable by the guest agent.
      prepare_job() {
          local slot="$1" task="$2" agent="$3" prompt_src="$4" timeout_s="$5" \
                token="$6" rclass="$7" persist="$8"
          local dir spec input ctrl worker logs
          dir="$(job_dir "$slot")"
          input="$(job_input_dir "$slot")"
          ctrl="$(job_controller_dir "$slot")"
          worker="$(job_worker_dir "$slot")"
          logs="$(job_worker_logs_dir "$slot")"
          spec="$(job_spec "$slot")"
          install -d -m ${jobRootModeArg} -o root -g root -- "$dir"
          install -d -m "$JOB_INPUT_DIR_MODE" -o root -g root -- "$input"
          install -d -m "$JOB_CONTROLLER_DIR_MODE" -o root -g root -- "$ctrl"
          install -d -m "$JOB_WORKER_DIR_MODE" \
              -o "$GUEST_AGENT_UID" -g "$GUEST_AGENT_GID" -- "$worker"
          install -d -m "$JOB_WORKER_LOGS_DIR_MODE" -o root -g root -- "$logs"
          # Fail closed if the controller directory is not what we just asked
          # for (e.g. pre-existing with wrong ownership): the guest would
          # otherwise get a result channel the agent can write.
          local ctrl_owner ctrl_mode
          ctrl_owner="$(stat -c %u -- "$ctrl")"
          ctrl_mode="$(stat -c %a -- "$ctrl")"
          [[ "$ctrl_owner" == "0" && "$ctrl_mode" == "700" ]] \
              || die "the job controller directory $ctrl is not root:root $JOB_CONTROLLER_DIR_MODE (owner $ctrl_owner, mode $ctrl_mode)"
          # Nothing of an earlier allocation may survive into this one: a stale
          # result, controller state, cancellation request or worker log would
          # otherwise be read as if it belonged to this task. (The allocation
          # token makes a stale result harmless anyway — this is the second
          # layer, not the only one.)
          rm -f -- "$(job_result "$slot")" "$(job_ctrl_state "$slot")" \
                   "$(job_cancel "$slot")" "$spec" "$spec.tmp"
          find "$worker" -mindepth 1 -maxdepth 1 -exec rm -rf -- {} + 2>/dev/null || true
          find "$logs" -mindepth 1 -maxdepth 1 -exec rm -rf -- {} + 2>/dev/null || true
          # Migration (spec v1 -> v2): drop the old guest-writable out/ dir,
          # which used to be the (forgeable) result channel.
          rm -rf -- "$dir/out"
          install -m "$JOB_PROMPT_MODE" -o root -g root -- "$prompt_src" "$(job_prompt "$slot")" \
              || die "could not install the prompt file into $input"
          # ALLOC_TOKEN via the ENVIRONMENT, never argv: /proc/<pid>/cmdline is
          # world-readable (0444), /proc/<pid>/environ is 0400 — the ACTIVE
          # allocation token must not be readable off a running launcher.
          ALLOC_TOKEN="$token" jq -n \
              --argjson version "$JOB_SPEC_VERSION" \
              --arg taskId "$task" \
              --arg slot "$slot" --arg agent "$agent" \
              --arg workspace "$GUEST_WORKSPACE" --arg promptFile "$GUEST_PROMPT" \
              --argjson timeoutSeconds "$timeout_s" \
              --arg resourceClass "$rclass" \
              --argjson persistAgentState "$( (( persist )) && echo true || echo false )" \
              '{version:$version, taskId:$taskId,
                allocationToken:$ENV.ALLOC_TOKEN,
                slot:$slot, agent:$agent, workspace:$workspace,
                promptFile:$promptFile, timeoutSeconds:$timeoutSeconds,
                resourceClass:$resourceClass, persistAgentState:$persistAgentState}' \
              > "$spec.tmp" \
              || die "could not render the job spec"
          # 0400 root:root: the guest CONTROLLER reads it, the guest AGENT
          # cannot — so the allocation token stays out of the untrusted world.
          chmod "$JOB_SPEC_MODE" -- "$spec.tmp"
          chown root:root -- "$spec.tmp"
          # Rename last: the guest controller unit is conditional on spec.json
          # existing, so it must only appear once it is complete.
          mv -f -- "$spec.tmp" "$spec"
      }

      # Asks the GUEST controller to cancel — bound to the allocation token, so
      # a request can never affect a slot that has since been re-allocated to a
      # different task (and a stale request file cannot stop a new job). Written
      # root-only into the immutable input area, so the untrusted agent can
      # neither forge nor remove it.
      request_guest_cancel() {
          local slot="$1" task="$2" token="$3" f dir tmp
          f="$(job_cancel "$slot")"
          dir="$(job_input_dir "$slot")"
          [[ -d "$dir" ]] || return 0
          tmp="$(mktemp "$dir/.cancel.XXXXXX")" || return 0
          # The token is handed to jq in the ENVIRONMENT (see prepare_job).
          if ALLOC_TOKEN="$token" jq -n --argjson version "$JOB_SPEC_VERSION" \
              --arg taskId "$task" \
              --arg requestedAt "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
              '{version:$version, taskId:$taskId,
                allocationToken:$ENV.ALLOC_TOKEN,
                requestedAt:$requestedAt}' > "$tmp"; then
              chmod "$JOB_CANCEL_MODE" -- "$tmp"
              chown root:root -- "$tmp"
              mv -f -- "$tmp" "$f"
          else
              rm -f -- "$tmp"
              log "warning: could not write the cancellation request $f"
          fi
      }

      # Removes the runtime job data of a slot (input, controller channel,
      # worker output). Called on teardown and before an interactive run, so no
      # slot ever inherits another task's job. The workspace clone is NEVER
      # touched here (§35).
      clear_job() {
          local slot="$1" dir worker logs
          dir="$(job_dir "$slot")"
          worker="$(job_worker_dir "$slot")"
          logs="$(job_worker_logs_dir "$slot")"
          rm -f -- "$(job_spec "$slot")" "$(job_spec "$slot").tmp" \
                   "$(job_prompt "$slot")" "$(job_cancel "$slot")" \
                   "$(job_result "$slot")" "$(job_ctrl_state "$slot")"
          if [[ -d "$worker" ]]; then
              find "$worker" -mindepth 1 -maxdepth 1 -exec rm -rf -- {} + 2>/dev/null || true
          fi
          if [[ -d "$logs" ]]; then
              find "$logs" -mindepth 1 -maxdepth 1 -exec rm -rf -- {} + 2>/dev/null || true
          fi
          # Legacy v1 layout, if this slot still has one.
          rm -rf -- "$dir/out"
      }

      # ---- reading a guest-written document (ticket 7) ---------------------
      # The result is UNTRUSTED INPUT even though only the guest controller can
      # write it: ownership separation is a control, not a proof. Every read
      # goes through the ONE verifier, which checks that the path is a regular,
      # root-owned, non-symlink file in a root-owned non-group/other-writable
      # directory, parses it strictly, and requires the schema version,
      # controller version, task id, allocation token, slot and agent of the
      # ACTIVE allocation plus a valid terminal state and exit code.
      #
      # Return codes: 0 = valid (VERIFY_JSON is set), 1 = nothing written yet,
      # 2 = REJECTED (VERIFY_REASON is set) — a protocol/infrastructure error,
      # never a result, 3 = the CALLER is broken (usage error / verifier
      # missing): also fail-closed, but never blamed on the guest.
      VERIFY_JSON=""
      VERIFY_REASON=""
      verify_job_document() {
          local path="$1" kind="$2" task="$3" token="$4" slot="$5" agent="$6"
          local rc=0 err=""
          VERIFY_JSON=""
          VERIFY_REASON=""
          # stdout and stderr are captured SEPARATELY: the validated document
          # must never be contaminated by a diagnostic, and a diagnostic must
          # never be parsed as JSON.
          #
          # The expected allocation token is passed in the ENVIRONMENT, not in
          # argv: /proc/<pid>/cmdline is world-readable (0444) and this runs
          # every few seconds for the whole runtime of a job.
          local errf
          errf="$(mktemp)" || die "could not create a temp file for the verifier's stderr"
          VERIFY_JSON="$(AGENT_JOB_EXPECTED_TOKEN="$token" "$RESULT_VERIFIER" \
              --result "$path" --kind "$kind" --task "$task" \
              --slot "$slot" --agent "$agent" 2>"$errf")" || rc=$?
          err="$(cat -- "$errf")"
          rm -f -- "$errf"
          case "$rc" in
              0) ;;
              1) VERIFY_JSON="" ;;
              2) VERIFY_JSON=""; VERIFY_REASON="$err" ;;
              # 64 = the LAUNCHER passed a malformed --task/--slot/--agent or an
              # unusable token; 127 = the verifier is not installed. Neither is
              # evidence about the guest, so it must not be reported as "the
              # guest sent a bad result".
              *)
                  VERIFY_JSON=""
                  VERIFY_REASON="host-side verifier error (exit $rc): ''${err:-<no output>}"
                  rc=3
                  ;;
          esac
          return "$rc"
      }

      verify_job_result() {
          local slot="$1" task="$2" token="$3" agent="$4"
          verify_job_document "$(job_result "$slot")" result "$task" "$token" "$slot" "$agent"
      }

      # The controller's own PROGRESS phase (never a terminal outcome, and never
      # a reason to stop waiting). Best-effort: it is only used for events.
      job_phase() {
          local slot="$1" task="$2" token="$3" agent="$4"
          verify_job_document "$(job_ctrl_state "$slot")" state "$task" "$token" "$slot" "$agent" \
              || return 1
          jq -r '.phase // ""' <<< "$VERIFY_JSON"
      }

      # A HOST-generated result record, for the cases where no VALID controller
      # result exists (no result at all, a rejected one, a dead VM, an operator
      # cancellation the guest never confirmed). `source` marks it, so an
      # archived record can never be mistaken for one the guest controller
      # actually authenticated.
      host_result_json() {
          local task="$1" token="$2" slot="$3" agent="$4" state="$5" rc="$6" message="$7"
          # The token is handed to jq in the ENVIRONMENT (see prepare_job).
          ALLOC_TOKEN="$token" jq -nc \
              --argjson version "$JOB_SPEC_VERSION" \
              --argjson controllerVersion "$JOB_CONTROLLER_VERSION" \
              --arg taskId "$task" \
              --arg slot "$slot" --arg agent "$agent" --arg state "$state" \
              --argjson exitCode "$rc" \
              --arg finishedAt "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
              --argjson timedOut "$( [[ "$state" == "timed-out" ]] && echo true || echo false )" \
              --arg message "$message" \
              '{version:$version, controllerVersion:$controllerVersion,
                taskId:$taskId, allocationToken:$ENV.ALLOC_TOKEN, slot:$slot,
                agent:$agent, state:$state, exitCode:$exitCode,
                finishedAt:$finishedAt, timedOut:$timedOut, message:$message,
                source:"host"}'
      }

      # Archive a result outside every guest share, so `status <task>` still
      # knows the outcome after the slot was released. Only ever the VALIDATED
      # controller document (tagged `source:"controller"`) or a host-generated
      # record — never a raw guest file.
      # The archived document contains the run's allocation token, so both the
      # directory and the file are ROOT-ONLY (0700/0600) — never world-readable.
      # A write failure is reported: `submit` prints the archive path, so
      # silently not having written it would be a lie.
      archive_result() {
          local task="$1" json="$2" archived tmp
          archived="$RESULTS_DIR/$task.json"
          install -d -m "$JOB_ARCHIVE_DIR_MODE" -o root -g root -- "$RESULTS_DIR" \
              || { log "warning: could not create the result archive $RESULTS_DIR"; return 1; }
          tmp="$(mktemp "$RESULTS_DIR/.$task.XXXXXX")" \
              || { log "warning: could not create a temp file in $RESULTS_DIR"; return 1; }
          if printf '%s\n' "$json" > "$tmp" \
              && chmod "$JOB_ARCHIVE_MODE" -- "$tmp" \
              && mv -f -- "$tmp" "$archived"; then
              return 0
          fi
          rm -f -- "$tmp"
          log "warning: could not archive the result of '$task' at $archived"
          return 1
      }

      archive_controller_result() {
          local task="$1" json="$2"
          archive_result "$task" "$(jq -c '. + {source:"controller"}' <<< "$json")"
      }

      # ---- task-scoped agent state (ticket 5 B) ---------------------------
      ${stateSlotDirDef}
      state_task_dir() { printf '%s' "$STATE_TASKS_ROOT/$1/$2"; }

      agent_state_dirs() {
          local agent="$1" pair
          for pair in ''${AGENT_STATE_DIRS[@]+"''${AGENT_STATE_DIRS[@]}"}; do
              [[ "''${pair%%:*}" == "$agent" ]] || continue
              printf '%s\n' "''${pair#*:}"
          done
      }

      # Opt-in, task-scoped persistence: create ONLY the directories the
      # registry declares for this agent under
      # <runtimeRoot>/state/tasks/<task>/<agent>/, then bind-mount that
      # per-task directory onto the slot's share source. Nothing else is ever
      # exposed — no host home, no ~/.ssh, no sockets, no other task's state.
      setup_agent_state() {
          local slot="$1" task="$2" agent="$3" mp dir dirs=()
          # Refuse BEFORE creating anything: an agent without declared state
          # directories cannot persist, and silently doing nothing would be a
          # lie (the operator asked for persistence).
          mapfile -t dirs < <(agent_state_dirs "$agent")
          (( ''${#dirs[@]} > 0 )) \
              || die "--persist-agent-state: agent '$agent' declares no persistent state directories"
          mp="$(state_slot_dir "$slot")"
          ${stateSlotInstallGuard}install -d -m ${stateSlotModeArg} -o "$GUEST_AGENT_UID" -g "$GUEST_AGENT_GID" -- "$mp"
          local task_dir
          task_dir="$(state_task_dir "$task" "$agent")"
          install -d -m 0755 -o root -g root -- "$STATE_TASKS_ROOT"
          install -d -m 0755 -o "$GUEST_AGENT_UID" -g "$GUEST_AGENT_GID" -- \
              "$STATE_TASKS_ROOT/$task" "$task_dir"
          for dir in "''${dirs[@]}"; do
              [[ -n "$dir" ]] || continue
              install -d -m 0700 -o "$GUEST_AGENT_UID" -g "$GUEST_AGENT_GID" -- "$task_dir/$dir"
          done
          unmount_verified "$mp" "$slot" \
              || die "could not unmount stale agent-state bind at $mp"
          mount --bind -- "$task_dir" "$mp" \
              || die "agent-state bind mount failed: $task_dir -> $mp"
          findmnt -n -- "$mp" >/dev/null 2>&1 \
              || die "agent-state bind mount verification failed: $mp"
          log "persisting agent state for '$task' ($agent): $task_dir"
      }

      # Same contract as teardown_bind_mount: verified, never lazy. The agent
      # state share is held by the SAME per-slot virtiofsd, so a hard-killed
      # guest wedges it the same way.
      teardown_agent_state() {
          unmount_verified "$(state_slot_dir "$1")" "$1"
      }

      # Without persistence the slot's share source must be EMPTY, so the
      # guest-side linker finds nothing and the agent keeps its disposable home
      # (and no state leaks from a previous task on this slot).
      clear_agent_state_slot() {
          local mp
          mp="$(state_slot_dir "$1")"
          # While the bind is still in place, this directory IS the task's
          # persisted state — emptying it would delete that state through the
          # bind. So a surviving mount must abort the clearing, not be ignored.
          teardown_agent_state "$1" || {
              log "ERROR: refusing to clear $mp while its bind mount is still in place (that would delete the task's persisted state)"
              return 1
          }
          [[ -d "$mp" ]] || return 0
          find "$mp" -mindepth 1 -maxdepth 1 -exec rm -rf -- {} + 2>/dev/null || true
      }

      ${sessionHelpers}${configSeedHelpers}# ---- §21 slot cleanup / interrupt handling -------------------------
      # Tear a slot down WITHOUT deleting the workspace clone (§26/§35): stop
      # the VM, unmount the bind, remove the slot transient state. Locks are
      # released implicitly when this process exits and closes their fds.
      cleanup_slot() {
          local slot="$1" leaked=0
          [[ -n "$slot" ]] || return 0
          stop_vm "$slot"
          # A surviving mount is LOGGED and emitted as `mount-leak` by
          # unmount_verified; we still finish the teardown, because leaving the
          # allocation marker behind would reserve the slot forever — a second
          # fault on top of the leak. What we must never do is call this a clean
          # release, hence the warning below.
          teardown_bind_mount "$slot" || leaked=1
          # Runtime job data (spec/prompt/result) is transient per task: drop it
          # with the slot, exactly like the session marker. The workspace clone
          # is deliberately kept (§26/§35).
          clear_job "$slot"
          # Unmount the per-task agent state (the task's DIRECTORY is kept, like
          # the workspace clone) and leave the slot's share source empty.
          clear_agent_state_slot "$slot" || leaked=1
          ${configSeedClear}${sessionClear}rm -rf -- "''${SLOTS_DIR:?}/$slot"
          if (( leaked )); then
              log "WARNING: slot $slot was released with a LEAKED mount; re-run '$PROG recover' once the holder is gone"
              # The teardown CONTINUED (see above) but it did not succeed, and
              # an operator running `stop`/`destroy` must learn that from the
              # EXIT CODE, not only from a log line. The EXIT-trap callers are
              # the ones that must never abort; they say so explicitly with
              # `|| true`.
              return 1
          fi
          return 0
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

      # Poll with EXPONENTIAL BACKOFF instead of a fixed 3 s interval: a warm
      # slot answers within a few hundred milliseconds, and the old fixed sleep
      # made every launch pay up to 3 s of pure waiting after the guest was
      # already up (lightweight plan phase 7). The backoff starts at
      # READY_POLL_MIN_MS, doubles up to READY_POLL_MAX_MS and is bounded by the
      # SAME overall READY_TIMEOUT as before, so the worst case is unchanged.
      # `sleep` accepts fractional seconds (coreutils).
      wait_ready() {
          local ip="$1" waited_ms=0 delay_ms="$READY_POLL_MIN_MS"
          [[ "$SSH_ENABLED" == "1" ]] || { log "SSH disabled; not waiting for guest readiness"; return 0; }
          while ! guest_ssh_ready "$ip"; do
              if (( waited_ms >= READY_TIMEOUT * 1000 )); then
                  log "guest at $ip not reachable via SSH within ''${READY_TIMEOUT}s"
                  return 1
              fi
              sleep "$(printf '%d.%03d' $(( delay_ms / 1000 )) $(( delay_ms % 1000 )))"
              waited_ms=$(( waited_ms + delay_ms ))
              delay_ms=$(( delay_ms * 2 ))
              (( delay_ms <= READY_POLL_MAX_MS )) || delay_ms="$READY_POLL_MAX_MS"
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
        run --name <task> --repository <path> [--agent <name>] [--branch <br>]
            [--resource-class <class>] [--wait <sec>] [--persist-agent-state]
            [--attach] [--no-preflight]
                              Allocate a free slot, create a standalone clone,
                              bind-mount it at the slot's /workspace source and
                              start the microVM. With --attach, SSH in running
                              'agent-run <agent>' and tear the VM down on exit
                              (the workspace clone is always kept). --no-preflight
                              skips the model-endpoint preflight (an interactive
                              escape hatch for debugging; 'submit' has none).
        stop <slot|task>      Stop the VM, unmount the bind, drop slot transient
                              state. Keeps workspace/git/patches.
        destroy <slot|task>   Like stop, plus clear ephemeral slot runtime.
                              Keeps workspace/git/patches.
        status [slot|task]    Show slot/service/IP/MAC/task/workspace/mount/
                              agent/start-time/SSH-readiness/lock-owner.
        doctor               Read-only host-side diagnosis of the model-API path
                              (host LiteLLM, the bridge-only forwarder socket,
                              the bridge + gateway address, the firewall chains
                              and per-slot host keys). Exits non-zero if any
                              component is broken; run it when run/submit
                              fails the endpoint preflight.
        list                  One-line status for every slot.
        ssh <slot|task> [--] [cmd...]   SSH into the guest 'agent' user.
        console <slot|task>   Attach to the VM serial console (journal).
        submit --name <task> --repository <path> --agent <name>
               --prompt-file <path> [--timeout <sec>] [--branch <br>]
               [--resource-class <class>] [--wait <sec>] [--persist-agent-state]
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
        recover [--dry-run] [--prune-foreign]
                              Reconcile slots with systemd: stop orphaned units,
                              unmount stale mounts, drop stale markers and job
                              data. Always keeps workspace clones. --dry-run
                              only prints what it would do. Per-slot state whose
                              slot name is NOT in the current pool (left over
                              from a generation with different slot names) is
                              always REPORTED as a 'foreign:' finding, and only
                              removed with --prune-foreign.
        usage                 Report RETAINED disk usage per task (workspace
                              clone + task-scoped agent state) plus the runtime
                              roots, and how to prune it.
        workspace-remove <task> [--force]
                              Delete the standalone clone. Separate + guarded:
                              refuses on uncommitted changes / unexported
                              commits, and on a slot still using the clone,
                              without --force. With --force it also stops any
                              slot still holding the clone before removing it.

      Supported agents (--agent), generated from the module's agent registry:
        ${lib.concatStringsSep "\n  " agentRegistry.names}

      --persist-agent-state keeps ONLY the agent's declared state directories,
      scoped to the task, under ${agentState.tasksRoot}/<task>/<agent>/.
      Without it the guest home stays disposable. Declared directories:
        ${lib.concatStringsSep "\n  " (
          lib.concatMap (a: map (d: "${a.name}: ~/${d}") a.persistentState.directories) (
            lib.attrValues agentRegistry.agents
          )
        )}

      Resource classes (--resource-class), generated from the module options
      (the allocator NEVER substitutes a different class; --wait <sec> bounds
      how long it waits for a free slot in the requested one):
        ${lib.concatStringsSep "\n  " (
          lib.mapAttrsToList (
            n: c: "${n}: ${toString c.count} slot(s), ${toString c.vcpu} vCPU, ${toString c.memoryMiB} MiB"
          ) agentResourceClasses
        )}
      EOF
          exit 2
      }

      # ==== surface a FAILED batch worker's stderr ==========================
      # A batch job that dies in seconds tells the operator nothing in its
      # AUTHORITATIVE result (the controller only records `failed` + an exit
      # code). The worker's own stderr — captured by the guest's systemd into
      # the host-visible, ROOT-owned `$JOBS_ROOT/<slot>/worker-logs/stderr.log`
      # — is where the real reason lives ("connection refused", "no API key",
      # a stack trace, …). On a FAILED job, surface a BOUNDED tail of it to the
      # operator's stderr so the next step is obvious instead of another
      # 95-min investigation.
      #
      # Trust boundary (do not weaken): the worker is UNTRUSTED, so its stderr
      # is UNTRUSTED output. It is therefore
      #   * clearly labelled `UNTRUSTED WORKER STDERR`,
      #   * bounded to the last WORKER_STDERR_TAIL_BYTES bytes (capped under
      #     `taskLogMaxBytes`), never the whole file,
      #   * written ONLY to the operator's stderr — never into the authoritative
      #     result JSON, never into the structured event stream (which the
      #     controller / host share), and never `eval`d or parsed.
      # `cat` (not `tail -n`) on the byte-bounded tail keeps a half-line at
      # the start honest (it is clearly partial) without running the untrusted
      # text through any interpreter.
      readonly WORKER_STDERR_TAIL_BYTES=8192
      surface_worker_stderr() {
          local slot="$1" task="$2" state="$3"
          # Only a FAILED (or infrastructurally-broken) job is worth surfacing;
          # `completed`/`timed-out`/`cancelled` either succeeded or were
          # explained by their own state.
          case "$state" in
              failed|infrastructure-error) ;;
              *) return 0 ;;
          esac
          local stderr_file
          stderr_file="$(job_worker_logs_dir "$slot")/$JOB_WORKER_STDERR_NAME"
          # Require a NON-EMPTY REGULAR FILE: `[[ -s ]]` alone would also be
          # true for a FIFO or a character/block device, on which the bounded
          # `tail -c` below would BLOCK (a FIFO with no writer) or read device
          # state — a path the UNTRUSTED guest cannot create here (the logs dir
          # is root-owned), but one the launcher must never take regardless.
          [[ -f "$stderr_file" && -s "$stderr_file" ]] || return 0
          local bound=$WORKER_STDERR_TAIL_BYTES
          (( LOG_MAX_BYTES < bound )) && bound=$LOG_MAX_BYTES
          printf '%s: task %q FAILED — tail of the UNTRUSTED worker stderr (last %d bytes, root-owned; NOT the authoritative result):\n' \
              "$PROG" "$task" "$bound" >&2
          # Bound in bytes, then print through `cat -v`, which renders
          # non-printing bytes (ESC sequences, OSC title-set, NUL, …) as visible
          # ^X / M-X — so a hostile worker CANNOT inject terminal-control
          # escapes into the operator's TTY or forge a plausible-looking
          # launcher line. LF and TAB are preserved, so the tail stays readable.
          # `cat` and `tail` are in coreutils; the file is root-owned 0644 so
          # the launcher (root) can read it directly. `cat -v` does NOT
          # interpret the text — every byte is still printed, only the
          # non-printing ones are rendered visible.
          tail -c "$bound" -- "$stderr_file" 2>/dev/null | cat -v >&2 || true
          # Guarantee the labelled block ends on its own line even if the worker
          # stderr did not (it is UNTRUSTED text — never assume it is clean).
          printf '\n%s: end of UNTRUSTED worker stderr for task %q\n' "$PROG" "$task" >&2
      }

      # ==== model-endpoint preflight ==========================================
      # A guest agent that cannot reach the model API dies within seconds and
      # leaves nothing actionable in the authoritative result. The two real-KVM
      # runs that surfaced this both spent ~95 min before anyone noticed the
      # worker exited after 2 s. This preflight makes that failure FAST and
      # LOUD, BEFORE a VM is booted: it opens a bounded HTTP connection to the
      # SAME bridge endpoint a guest would use
      # (http://<gateway>:<litellmPort>/v1/models, forwarded by the
      # `agent-litellm-proxy` socket to the loopback LiteLLM). A successful 2xx
      # means the host backend, the bridge address, the bridge-only socket and
      # the loopback LiteLLM are all wired up; a failure names the most likely
      # component to check.
      #
      # Read-only and side-effect-free. Skipped under `offline` (no endpoint)
      # and when AGENT_MICROVM_SKIP_PREFLIGHT=1 (the EXECUTED test harnesses run
      # the launcher against stubs with no listener — they exercise the result
      # channel, not the endpoint).
      preflight_model_endpoint() {
          [[ "$LITELLM_CAPABLE" == "1" ]] || return 0
          [[ "$SKIP_PREFLIGHT" == "1" ]] && return 0
          local url="http://$GATEWAY:$LITELLM_PORT/v1/models"
          local attempt
          # A COLD LiteLLM (DB init/migration on the first post-boot request)
          # can take a few seconds to answer even though it is healthy. Retry
          # a bounded number of times so a slow-but-working endpoint is not
          # mistaken for a dead one; a genuinely dead endpoint still fails fast
          # (each attempt is bounded to PREFLIGHT_TIMEOUT by -m and
          # --connect-timeout).
          for attempt in $(seq 1 "$PREFLIGHT_RETRIES"); do
              if curl -fsS -m "$PREFLIGHT_TIMEOUT" \
                  --connect-timeout "$PREFLIGHT_TIMEOUT" -o /dev/null "$url" 2>/dev/null; then
                  return 0
              fi
              (( attempt < PREFLIGHT_RETRIES )) && sleep "$PREFLIGHT_RETRY_DELAY"
          done
          cat >&2 <<EOF
      $PROG: PREFLIGHT FAILED: the model endpoint a guest must reach is not
        reachable at $url (bounded to ''${PREFLIGHT_TIMEOUT}s).
        The guest agent would die within seconds of starting. Before booting a
        sandbox, check on THIS host:
          - systemctl is-active agent-litellm-proxy.socket  (must be active;
            it needs the bridge device, so it is ordered after
            $BRIDGE-netdev.service)
          - systemctl is-active litellm.service  (the loopback backend on
            127.0.0.1:$LITELLM_PORT)
          - ip -br addr show $BRIDGE  (the bridge + its $GATEWAY address)
          - curl -fsS http://127.0.0.1:$LITELLM_PORT/v1/models  (the backend)
        or run: sudo $PROG doctor   (full diagnosis)
        (set AGENT_MICROVM_SKIP_PREFLIGHT=1 only in the stubbed test harness.)
      EOF
          return 1
      }

      cmd_run() {
          require_root run
          local task="" repo="" agent="" branch="" attach=0
          local rclass="$DEFAULT_RESOURCE_CLASS" wait_for=0 persist=0 no_preflight=0
          while [[ $# -gt 0 ]]; do
              case "$1" in
                  --persist-agent-state) persist=1; shift ;;
                  --no-preflight) no_preflight=1; shift ;;
                  --name)       task="''${2-}"; shift 2 ;;
                  --repository) repo="''${2-}"; shift 2 ;;
                  --agent)      agent="''${2-}"; shift 2 ;;
                  --branch)     branch="''${2-}"; shift 2 ;;
                  --resource-class) rclass="''${2-}"; shift 2 ;;
                  --wait)       wait_for="''${2-}"; shift 2 ;;
                  --attach)     attach=1; shift ;;
                  --name=*)       task="''${1#*=}"; shift ;;
                  --repository=*) repo="''${1#*=}"; shift ;;
                  --agent=*)      agent="''${1#*=}"; shift ;;
                  --branch=*)     branch="''${1#*=}"; shift ;;
                  --resource-class=*) rclass="''${1#*=}"; shift ;;
                  --wait=*)       wait_for="''${1#*=}"; shift ;;
                  # --no-preflight is a bare boolean flag (like --attach and
                  # --persist-agent-state): no `=value` form is accepted, so
                  # `--no-preflight=anything` is rejected as an unknown argument
                  # rather than silently coercing a non-numeric value to 0.
                  *) die "run: unknown argument '$1'" ;;
              esac
          done
          validate_resource_class "$rclass"
          validate_wait "$wait_for"
          (( ! persist )) || [[ -n "$agent" ]] \
              || die "run: --persist-agent-state requires --agent <name>"
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

          # Fail FAST if the model endpoint a guest must reach is not up: a
          # sandbox whose agent cannot reach LiteLLM dies within seconds and
          # the 95-min real-KVM runs only surfaced that by accident. This is
          # the cheap, read-only host-side check; `doctor` is the deep one.
          #
          # `--no-preflight` is the documented escape hatch for the INTERACTIVE
          # `run` (a human is present, and a shell in the workspace is still
          # useful for debugging even when the endpoint is down). `submit`
          # (batch, no human watching) keeps the preflight FAIL-CLOSED and has
          # NO such flag, so an unattended run can never boot a doomed VM
          # silently. The test-only AGENT_MICROVM_SKIP_PREFLIGHT=1 remains the
          # harness override for both.
          if (( no_preflight )); then
              log "WARNING: --no-preflight: skipping the model-endpoint preflight; if the endpoint is unreachable the guest agent will die within seconds (run 'sudo $PROG doctor' to diagnose)"
          else
              preflight_model_endpoint
          fi

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
          # `|| true`: an EXIT trap must complete every step it can, so a leaked
          # mount (cleanup_slot's non-zero) must not abort it. The leak is
          # logged and emitted as a `mount-leak` event by unmount_verified.
          on_exit() { (( ''${committed:-0} )) || cleanup_slot "''${slot:-}" || true; }
          trap on_exit EXIT
          trap 'exit 130' INT TERM

          local run_mode="detached"
          (( attach )) && run_mode="attached"
          set_event_context "$task" "" "$agent" "$rclass" "$run_mode"
          emit_event task-submitted

          allocate_slot "$rclass" "$wait_for"
          slot="$ALLOC_SLOT"
          token="$ALLOC_TOKEN"
          set_event_context "$task" "$slot" "$agent" "$rclass" "$run_mode"
          emit_event slot-allocated

          local clone ip
          ip="$(slot_ip "$slot")"
          log "allocated slot $slot ($ip; class $rclass, $(slot_vcpu "$slot") vCPU, $(slot_mem "$slot") MiB)"
          # Deterministic clone path; create_clone runs in THIS shell (see its
          # header) so a failure exits cmd_run and its EXIT trap cleans up.
          clone="$WORKSPACE_ROOT/$task"
          create_clone "$top" "$task" "$branch"
          emit_event workspace-created
          ${sessionPrepare}setup_bind_mount "$slot" "$clone"
          ${configSeedStage}# An interactive slot must not accidentally pick up a stale batch job.
          clear_job "$slot"
          # Agent state is DISPOSABLE unless explicitly requested (ticket 5 B).
          if (( persist )); then
              [[ -n "$agent" ]] || die "--persist-agent-state requires --agent <name>"
              setup_agent_state "$slot" "$task" "$agent"
          else
              # Fail CLOSED: if the previous task's state share could not be
              # released, booting now would show that state to this task.
              clear_agent_state_slot "$slot" \
                  || die "could not clear the agent-state share of slot $slot"
          fi
          ${sessionVerify}emit_event vm-start-requested
          start_vm "$slot"

          # Persist the full session record for status/list (§34). No secrets.
          write_session_marker "$slot" "$token" "$run_mode" "$task" "$top" \
              "$clone" "$agent" "$branch" "" "$persist"

          if (( attach )); then
              [[ "$SSH_ENABLED" == "1" ]] || die "--attach requires enableSsh = true"
              [[ -n "$agent" ]] || die "--attach requires --agent <name>"
              # On readiness failure the EXIT trap runs cleanup_slot: the VM is
              # stopped and the bind unmounted, but the workspace clone is kept.
              require_known_hosts
              wait_ready "$ip" || die "guest not ready; tearing down slot $slot (workspace kept at $clone)"
              emit_event vm-ready
              emit_event agent-started
              log "attaching to $slot; running 'agent-run $agent' in /workspace"
              ssh "''${SSH_VERIFY_OPTS[@]}" \
                  ''${AGENT_MICROVM_SSH_KEY:+-i "$AGENT_MICROVM_SSH_KEY"} \
                  -t "$SSH_USER@$ip" -- agent-run "$agent" || true
              # Foreground session finished: tear the VM down, keep the clone.
              emit_event agent-finished
              log "session ended; tearing down $slot (workspace kept at $clone)"
              local leaked=0
              cleanup_slot "$slot" || leaked=1
              emit_event cleanup-completed
              committed=1
              trap - EXIT INT TERM
              (( ! leaked )) \
                  || die "slot $slot was released but a bind mount SURVIVED; run '$PROG recover' once its holder is gone"
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
          local rclass="$DEFAULT_RESOURCE_CLASS" wait_for=0 persist=0
          while [[ $# -gt 0 ]]; do
              case "$1" in
                  --persist-agent-state) persist=1; shift ;;
                  --name)        task="''${2-}"; shift 2 ;;
                  --repository)  repo="''${2-}"; shift 2 ;;
                  --agent)       agent="''${2-}"; shift 2 ;;
                  --branch)      branch="''${2-}"; shift 2 ;;
                  --prompt-file) prompt="''${2-}"; shift 2 ;;
                  --timeout)     timeout_s="''${2-}"; shift 2 ;;
                  --resource-class) rclass="''${2-}"; shift 2 ;;
                  --wait)        wait_for="''${2-}"; shift 2 ;;
                  --name=*)        task="''${1#*=}"; shift ;;
                  --repository=*)  repo="''${1#*=}"; shift ;;
                  --agent=*)       agent="''${1#*=}"; shift ;;
                  --branch=*)      branch="''${1#*=}"; shift ;;
                  --prompt-file=*) prompt="''${1#*=}"; shift ;;
                  --timeout=*)     timeout_s="''${1#*=}"; shift ;;
                  --resource-class=*) rclass="''${1#*=}"; shift ;;
                  --wait=*)        wait_for="''${1#*=}"; shift ;;
                  *) die "submit: unknown argument '$1'" ;;
              esac
          done
          validate_resource_class "$rclass"
          validate_wait "$wait_for"
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

          # Same fast-fail host-side endpoint check as `run`: do not boot a VM
          # whose agent can only die in seconds. See preflight_model_endpoint.
          preflight_model_endpoint

          mkdir -p -- "$RUN_DIR" "$SLOTS_DIR" "$WORKSPACE_ROOT"
          # Root-only: an archived result carries its run's allocation token.
          install -d -m "$JOB_ARCHIVE_DIR_MODE" -o root -g root -- "$RESULTS_DIR"

          local slot="" token="" committed=0
          # shellcheck disable=SC2317  # invoked via trap
          # `|| true` as in cmd_run: the trap must finish, the leak is reported
          # by unmount_verified (log + `mount-leak` event).
          on_exit() { (( ''${committed:-0} )) || cleanup_slot "''${slot:-}" || true; }
          trap on_exit EXIT
          trap 'exit 130' INT TERM

          set_event_context "$task" "" "$agent" "$rclass" "batch"
          # The prompt's PATH and SIZE are safe to log; its CONTENT never is.
          emit_event task-submitted "" "" "prompt=$prompt_real ($(stat -c %s -- "$prompt_real") bytes), timeout=''${timeout_s}s"

          allocate_slot "$rclass" "$wait_for"
          slot="$ALLOC_SLOT"
          token="$ALLOC_TOKEN"
          set_event_context "$task" "$slot" "$agent" "$rclass" "batch"
          emit_event slot-allocated

          local clone ip
          ip="$(slot_ip "$slot")"
          log "allocated slot $slot ($ip; class $rclass, $(slot_vcpu "$slot") vCPU, $(slot_mem "$slot") MiB) for batch task '$task'"
          clone="$WORKSPACE_ROOT/$task"
          create_clone "$top" "$task" "$branch"
          emit_event workspace-created
          ${sessionPrepare}setup_bind_mount "$slot" "$clone"
          ${configSeedStage}prepare_job "$slot" "$task" "$agent" "$prompt_real" "$timeout_s" \
              "$token" "$rclass" "$persist"
          if (( persist )); then
              setup_agent_state "$slot" "$task" "$agent"
          else
              # Fail CLOSED, as in cmd_run.
              clear_agent_state_slot "$slot" \
                  || die "could not clear the agent-state share of slot $slot"
          fi
          write_session_marker "$slot" "$token" "batch" "$task" "$top" \
              "$clone" "$agent" "$branch" "$timeout_s" "$persist"
          ${sessionVerify}emit_event vm-start-requested
          start_vm "$slot"

          # --- wait for the AUTHORITATIVE controller result ------------------
          # HOST timeout = the job's own timeout + a grace period, so the guest
          # CONTROLLER (which stops the worker's cgroup at the job's own
          # deadline) still gets to write its result.
          #
          # Only the verified controller document ends this wait. A result
          # written by the untrusted worker (in worker/, in /workspace, or
          # anywhere else) is never even looked at, and a document that fails
          # verification is an INFRASTRUCTURE error, never a success.
          local deadline=$(( timeout_s + JOB_GRACE )) waited=0 state="" rc=70
          local last_phase="" phase="" vrc=0 reject_reason="" result_json=""
          log "waiting up to ''${deadline}s for the guest controller's result"
          while :; do
              vrc=0
              verify_job_result "$slot" "$task" "$token" "$agent" || vrc=$?
              if (( vrc == 0 )); then
                  result_json="$VERIFY_JSON"
                  state="$(jq -r '.state' <<< "$result_json")"
                  break
              fi
              if (( vrc == 3 )); then
                  # A HOST-side bug (bad arguments, verifier missing), not a
                  # guest protocol violation. Fail closed, but say whose fault
                  # it is — otherwise this sends the operator hunting a
                  # guest-side security incident.
                  reject_reason="$VERIFY_REASON"
                  log "cannot verify the guest result: $reject_reason"
                  emit_event result-rejected infrastructure-error "" "$reject_reason"
                  state="infrastructure-error"
                  break
              fi
              if (( vrc == 2 )); then
                  reject_reason="$VERIFY_REASON"
                  log "REJECTED the guest result: $reject_reason"
                  emit_event result-rejected infrastructure-error "" "$reject_reason"
                  state="infrastructure-error"
                  break
              fi
              # No result yet: mirror the CONTROLLER's (trusted, but
              # non-authoritative) progress phase into the host event log, so
              # one stream tells the whole story.
              phase="$(job_phase "$slot" "$task" "$token" "$agent" || true)"
              if [[ -n "$phase" && "$phase" != "$last_phase" ]]; then
                  case "$phase" in
                      running)    emit_event agent-started "$phase" ;;
                      timing-out) emit_event timeout "$phase" ;;
                      cancelling) emit_event cancellation "$phase" ;;
                  esac
                  last_phase="$phase"
              fi
              if (( waited >= deadline )); then
                  log "job did not finish within ''${deadline}s; stopping the VM"
                  state="timed-out"
                  reject_reason="host deadline ''${deadline}s exceeded"
                  emit_event timeout "$state" "" "$reject_reason"
                  break
              fi
              # A guest that died without writing a result must not hang us for
              # the full timeout window.
              if (( waited > 0 )) && ! service_active "$slot"; then
                  log "microvm@$slot stopped without a valid controller result"
                  state="infrastructure-error"
                  reject_reason="the VM stopped without a valid controller result"
                  break
              fi
              sleep 3
              waited=$(( waited + 3 ))
          done

          # A concurrent `cancel` for THIS allocation may have already recorded
          # the authoritative `cancelled` verdict and released the slot while
          # this loop was sleeping. When that happens the loop above sees a
          # stopped VM with no readable result and calls it an
          # infrastructure-error — but a cancellation must NEVER silently
          # degrade into another state, and this run must not overwrite the
          # archive that `cancel` already wrote. Detect it by the only
          # host-side evidence that survives cancel's teardown: the archived
          # result. Only an archive for THIS task naming state `cancelled` is
          # accepted (the archive is per-task and written by root only).
          local cancel_adopted=0
          if [[ -z "$result_json" && -f "$RESULTS_DIR/$task.json" ]] \
              && [[ "$(jq -r '.state // ""' "$RESULTS_DIR/$task.json" 2>/dev/null || true)" == "cancelled" ]]; then
              log "a concurrent cancel already recorded 'cancelled' for '$task'; keeping that verdict (not '$state')"
              state="cancelled"
              cancel_adopted=1
          fi

          case "$state" in
              completed) rc=0 ;;
              failed)    rc=1 ;;
              timed-out) rc=124 ;;
              # A cancellation is an EXPECTED terminal state, not an
              # infrastructure error: report the conventional "terminated by
              # SIGINT" code (128+2), the same code the guest controller puts
              # in the authoritative result, instead of falling into the 70
              # bucket below.
              cancelled) rc=130 ;;
              *)         rc=70 ;;
          esac

          # A FAILED (or infrastructure-error) job is the one case where the
          # worker's own stderr is worth showing: it holds the real reason the
          # agent died (e.g. "connection refused" from the dead LiteLLM
          # endpoint) that the authoritative result does not. Surface a BOUNDED
          # tail BEFORE cleanup_slot clears the job dir, labelled UNTRUSTED and
          # never written into the result JSON or the event stream.
          surface_worker_stderr "$slot" "$task" "$state"

          # Archive the result (host-only, outside every guest share) BEFORE the
          # job dir is cleared, so `status <task>` can still report the outcome.
          # Either the VALIDATED controller document or an explicitly
          # host-generated record — never a raw guest file.
          local archived="$RESULTS_DIR/$task.json" archived_note=""
          archived_note="$archived"
          # A failed archive write must not abort the teardown below (and must
          # not be reported as a path that exists).
          if [[ -n "$result_json" ]]; then
              archive_controller_result "$task" "$result_json" \
                  || archived_note="<NOT ARCHIVED, see the warning above>"
          elif (( cancel_adopted )); then
              # The concurrent `cancel` owns this archive; overwriting it with a
              # host-generated record would replace the controller's verdict
              # with a weaker one.
              [[ -f "$archived" ]] || archived_note="<NOT ARCHIVED>"
          else
              archive_result "$task" "$(host_result_json "$task" "$token" "$slot" \
                  "$agent" "$state" "$rc" "''${reject_reason:-no valid controller result}")" \
                  || archived_note="<NOT ARCHIVED, see the warning above>"
          fi

          # Teardown: stop the VM, unmount, drop job data + marker. The
          # workspace clone is ALWAYS kept (§35).
          #
          # A surviving bind mount is an INFRASTRUCTURE failure of this run: the
          # job's own verdict is still archived and reported, but a batch run
          # that leaked a mount must not exit 0. It only upgrades a success — a
          # real job failure/timeout keeps its own, more specific code.
          local leaked=0
          if (( cancel_adopted )); then
              # `cancel` already ran the token-guarded teardown for this
              # allocation. Repeating it here would report phantom leaks for
              # mounts that are legitimately gone.
              log "slot $slot was already released by the concurrent cancel; skipping teardown"
          else
              cleanup_slot "$slot" || leaked=1
          fi
          emit_event vm-stopped "$state"
          emit_event cleanup-completed "$state" "$rc"
          committed=1
          trap - EXIT INT TERM
          if (( leaked && rc == 0 )); then
              log "ERROR: the job succeeded but slot $slot was released with a LEAKED bind mount"
              rc=70
          fi

          cat >&2 <<EOF
      $PROG: batch task '$task' finished with state '$state'.
        workspace: $clone
        result:    $archived_note
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
          local token agent
          token="$(marker_field "$slot" token || true)"
          [[ -n "$token" ]] || die "slot $slot has no allocation token; refusing (use 'recover')"
          agent="$(marker_field "$slot" agent || true)"

          set_event_context "$task" "$slot" "$agent" \
              "$(slot_class "$slot")" "$(marker_field "$slot" mode || true)"
          emit_event cancellation "cancelled" "130"
          log "cancelling task '$task' on slot $slot (workspace kept)"

          # Ask the GUEST CONTROLLER to cancel first, bound to THIS allocation's
          # token: it stops the worker's whole cgroup, records `cancelled` and
          # writes the authoritative result. A stale request cannot affect a
          # differently-tokened (i.e. newly allocated) job.
          install -d -m "$JOB_ARCHIVE_DIR_MODE" -o root -g root -- "$RESULTS_DIR"
          request_guest_cancel "$slot" "$task" "$token"
          local waited=0 vrc=0 cancelled_json="" preexisting_json="" terminal_state=""
          if [[ "$(marker_field "$slot" mode || true)" == "batch" ]] && service_active "$slot"; then
              log "waiting up to ''${CANCEL_WAIT}s for the guest controller to confirm"
              while (( waited < CANCEL_WAIT )); do
                  vrc=0
                  verify_job_result "$slot" "$task" "$token" "$agent" || vrc=$?
                  if (( vrc == 0 )); then
                      # A VALID result is not necessarily a CANCELLATION. The
                      # worker may have finished, failed or hit its deadline
                      # between planting the request and this poll, and the
                      # controller then wrote that verdict instead. Adopting it
                      # as "the cancellation" is how a cancelled task came to be
                      # archived as `timed-out`: only `.state == "cancelled"`
                      # confirms the cancellation. Any other terminal state is
                      # the truthful outcome and is preserved verbatim — the
                      # host must never rewrite it into a cancellation that did
                      # not happen.
                      terminal_state="$(jq -r '.state // ""' <<<"$VERIFY_JSON")"
                      if [[ "$terminal_state" == "cancelled" ]]; then
                          cancelled_json="$VERIFY_JSON"
                      else
                          preexisting_json="$VERIFY_JSON"
                          log "task '$task' had already terminated as '$terminal_state' before the cancellation took effect; recording THAT verdict"
                      fi
                      break
                  fi
                  if (( vrc == 3 )); then
                      log "cannot verify the guest result while cancelling: $VERIFY_REASON"
                      emit_event result-rejected infrastructure-error "" "$VERIFY_REASON"
                      break
                  fi
                  if (( vrc == 2 )); then
                      log "REJECTED the guest result while cancelling: $VERIFY_REASON"
                      emit_event result-rejected infrastructure-error "" "$VERIFY_REASON"
                      break
                  fi
                  service_active "$slot" || break
                  sleep 2
                  waited=$(( waited + 2 ))
              done
          fi

          # Record the cancellation, so the outcome of a cancelled job is not
          # silently indistinguishable from a crash. The controller's own
          # (validated) record wins; otherwise the host says so explicitly.
          local archived="$RESULTS_DIR/$task.json"
          if [[ -n "$cancelled_json" ]]; then
              archive_controller_result "$task" "$cancelled_json" || true
          elif [[ -n "$preexisting_json" ]]; then
              # The job had ALREADY reached a different terminal state; archive
              # the controller's real verdict rather than a forged cancellation.
              archive_controller_result "$task" "$preexisting_json" || true
          else
              archive_result "$task" "$(host_result_json "$task" "$token" "$slot" \
                  "$agent" cancelled 130 "cancelled by the operator; the guest controller did not confirm")" \
                  || true
          fi
          # Requests a clean shutdown first, waits a bounded interval, then
          # force-kills — all inside cleanup_slot/stop_vm — and unmounts + drops
          # the runtime job data. Token-guarded.
          local leaked=0
          cleanup_slot_owned "$slot" "$token" || leaked=1
          emit_event cleanup-completed "cancelled" "130"
          if [[ -e "$archived" ]]; then
              if [[ -n "$preexisting_json" ]]; then
                  log "the task had already finished as '$terminal_state' (NOT cancelled); that verdict is recorded at $archived"
              else
                  log "cancelled; result recorded at $archived"
              fi
          else
              log "cancelled; the result could NOT be archived (see the warning above)"
          fi
          # Report the cancellation as done ONLY if it really released the slot.
          (( ! leaked )) \
              || die "cancelled, but slot $slot was released with a LEAKED bind mount; run '$PROG recover' once its holder is gone"
      }

      # ==== FOREIGN per-slot state =========================================
      # Several host directories are keyed by SLOT NAME. Every command here
      # iterates "''${SLOT_NAMES[@]}", i.e. the pool of the CURRENT generation, so
      # per-slot state written under a name this generation does not define — e.g.
      # `slots/agent-0/` from before the `agent-<class>-<i>` rename — is invisible
      # to `list`, `status` and `recover` alike, and nothing ever cleans it up.
      # "Unused" was true; "silent" was the problem.
      #
      # Only directories this feature OWNS are scanned. $STATE_ROOT is
      # deliberately NOT in the list: it is microvm.nix's state dir for EVERY
      # microVM on the host, so it is handled separately and restricted to the
      # `agent-*` naming plus the one mount point we create there.
      readonly FOREIGN_SCAN_DIRS=(
          "$SLOTS_DIR"
          "$JOBS_ROOT"
          "$STATE_SLOTS_ROOT"
          "$HOSTKEYS_ROOT"
      )

      # "<foreign slot name> <path>" per line. Nothing is deleted here.
      foreign_slot_paths() {
          local dir entry name
          for dir in "''${FOREIGN_SCAN_DIRS[@]}"; do
              [[ -d "$dir" ]] || continue
              for entry in "$dir"/*; do
                  [[ -e "$entry" ]] || continue
                  name="''${entry##*/}"
                  is_slot_name "$name" && continue
                  printf '%s %s\n' "$name" "$entry"
              done
          done
          for entry in "$STATE_ROOT"/agent-*; do
              [[ -d "$entry" ]] || continue
              name="''${entry##*/}"
              is_slot_name "$name" && continue
              # Only the workspace mount point we created, never the VM state dir
              # itself (that belongs to microvm.nix).
              [[ -d "$entry/workspace" ]] || continue
              printf '%s %s\n' "$name" "$entry/workspace"
          done
      }

      # Defensive: only ever remove something under a root this feature owns.
      foreign_path_is_ours() {
          case "$1" in
              "$RUNTIME_ROOT"/*) return 0 ;;
              "$STATE_ROOT"/*)   return 0 ;;
              *) return 1 ;;
          esac
      }

      # ==== recovery (ticket 4) ============================================
      # Reconciles every slot's marker with the actual systemd/mount state.
      # NEVER deletes a workspace clone. --dry-run only prints.
      cmd_recover() {
          require_root recover
          local dry=0 prune=0
          while [[ $# -gt 0 ]]; do
              case "$1" in
                  --dry-run) dry=1; shift ;;
                  --prune-foreign) prune=1; shift ;;
                  *) die "recover: unknown argument '$1'" ;;
              esac
          done
          local slot marker active mounted state_mounted mode pid pid_start task acted=0 failed=0
          for slot in "''${SLOT_NAMES[@]}"; do
              marker=0
              [[ -e "$(session_file "$slot")" ]] && marker=1
              active=0
              service_active "$slot" && active=1
              mounted=0
              findmnt -n -- "$(mount_point "$slot")" >/dev/null 2>&1 && mounted=1
              # BOTH binds a slot can hold. The agent-state bind
              # (<runtimeRoot>/state/slots/<slot>, ticket 5 B) is held by the
              # SAME per-slot virtiofsd as the workspace bind, so a SIGKILLed
              # guest leaks it exactly the same way — and while it survives, the
              # task's persisted state is still reachable through it and
              # clear_agent_state_slot refuses to run. Recovering only the
              # workspace bind left that leak undetected AND unreported: recover
              # printed `ok`.
              state_mounted=0
              findmnt -n -- "$(state_slot_dir "$slot")" >/dev/null 2>&1 && state_mounted=1
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
              elif (( ! marker && ! active && (mounted || state_mounted) )); then
                  if (( mounted && state_mounted )); then
                      reason="stale bind mounts (workspace + agent state)"
                  elif (( mounted )); then
                      reason="stale bind mount"
                  else
                      reason="stale agent-state bind mount"
                  fi
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
                  (( state_mounted )) && printf '%s:   would unmount %s\n' "$slot" "$(state_slot_dir "$slot")"
                  [[ -e "$(job_spec "$slot")" ]] \
                      && printf '%s:   would clear job data in %s\n' "$slot" "$(job_dir "$slot")"
                  (( marker )) && printf '%s:   would drop the allocation marker\n' "$slot"
                  printf '%s:   would KEEP the workspace clone\n' "$slot"
                  continue
              fi

              printf '%s: recovering — %s\n' "$slot" "$reason"
              set_event_context "''${task:-}" "$slot" "" "$(slot_class "$slot")" "''${mode:-}"
              emit_event recovery-action "" "" "$reason"
              if (( active )); then
                  printf '%s:   stopping %s\n' "$slot" "microvm@$slot.service"
                  stop_vm "$slot"
              fi
              if (( mounted )); then
                  printf '%s:   unmounting %s\n' "$slot" "$(mount_point "$slot")"
                  # "unmounting X" must not be printed for a mount that is still
                  # there afterwards. teardown_bind_mount verifies (and stops the
                  # slot's virtiofsd, which is what holds the share after a
                  # SIGKILLed guest); if the mount survives anyway, say so, and
                  # let the whole recover run exit non-zero.
                  if ! teardown_bind_mount "$slot"; then
                      printf '%s:   FAILED to unmount %s (still mounted)\n' \
                          "$slot" "$(mount_point "$slot")"
                      failed=1
                  fi
              fi
              if (( state_mounted )); then
                  printf '%s:   unmounting %s\n' "$slot" "$(state_slot_dir "$slot")"
                  # Same contract as the workspace bind: VERIFIED, never lazy,
                  # and a survivor is named and makes the whole run fail.
                  if ! teardown_agent_state "$slot"; then
                      printf '%s:   FAILED to unmount %s (still mounted)\n' \
                          "$slot" "$(state_slot_dir "$slot")"
                      failed=1
                  fi
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

          # --- per-slot state under a name the current pool does not define ---
          # Reported as its OWN finding, never mixed into the per-slot lines
          # above, and never removed unless --prune-foreign says so: it may be
          # the only remaining copy of something the operator still wants.
          local fname fpath found_foreign=0
          local -a fnames=() fpaths=()
          while read -r fname fpath; do
              [[ -n "$fname" ]] || continue
              fnames+=("$fname")
              fpaths+=("$fpath")
          done < <(foreign_slot_paths)
          if (( ''${#fpaths[@]} )); then
              found_foreign=1
              printf 'foreign: %d per-slot path(s) whose slot name is NOT in the current pool\n' \
                  "''${#fpaths[@]}"
              printf 'foreign:   (left over from a generation with different slot names; no other\n'
              printf 'foreign:    command iterates them, and no workspace clone is keyed by slot)\n'
              local i mounted_foreign
              for i in "''${!fpaths[@]}"; do
                  fname="''${fnames[$i]}"
                  fpath="''${fpaths[$i]}"
                  mounted_foreign=0
                  findmnt -n -- "$fpath" >/dev/null 2>&1 && mounted_foreign=1
                  printf 'foreign:   %s (slot name %s)%s\n' "$fpath" "$fname" \
                      "$( (( mounted_foreign )) && printf ' [STILL MOUNTED]' )"
                  if (( ! prune )); then
                      printf 'foreign:     left alone; remove it with: %s recover --prune-foreign\n' "$PROG"
                      continue
                  fi
                  if (( dry )); then
                      (( mounted_foreign )) && printf 'foreign:     would unmount %s\n' "$fpath"
                      printf 'foreign:     would remove %s\n' "$fpath"
                      continue
                  fi
                  foreign_path_is_ours "$fpath" \
                      || die "refusing to remove a path outside the feature's roots: $fpath"
                  set_event_context "" "$fname" "" "" ""
                  if (( mounted_foreign )); then
                      printf 'foreign:     unmounting %s\n' "$fpath"
                      # The SAME verified path as every other unmount: a foreign
                      # bind may be held by a virtiofsd of that old slot name.
                      if ! unmount_verified "$fpath" "$fname"; then
                          printf 'foreign:     FAILED to unmount %s (still mounted); not removing it\n' "$fpath"
                          failed=1
                          continue
                      fi
                  fi
                  # Emitted HERE, not before the unmount: an action that could
                  # not be carried out (a wedged foreign bind, handled above)
                  # must not appear in the lifecycle stream as if it had been.
                  emit_event recovery-action "" "" "foreign per-slot state: $fpath"
                  printf 'foreign:     removing %s\n' "$fpath"
                  rm -rf -- "''${fpath:?}"
              done
          elif (( prune )); then
              printf 'foreign: no per-slot state outside the current pool\n'
          fi

          if (( ! acted && ! found_foreign )); then
              log "nothing to recover"
          fi
          # Nothing to do is success; a recovery that could not complete is not.
          # Merely REPORTING foreign state is not a failure — it is a finding.
          (( ! failed )) || die "recover could not release every resource (see the FAILED lines above)"
      }

      # ==== retained-usage report (ticket 5 C) =============================
      # Workspace clones and task-scoped agent state are deliberately KEPT after
      # a run, so they grow without bound unless the operator prunes them. This
      # makes that growth visible (and points at the command that removes it).
      cmd_usage() {
          require_root usage
          local task ws_bytes st_bytes total_ws=0 total_st=0 dir

          dir_bytes() {
              local d="$1"
              [[ -d "$d" ]] || { printf '0'; return 0; }
              du -sb --one-file-system -- "$d" 2>/dev/null | cut -f1
          }
          human() { numfmt --to=iec --suffix=B -- "$1"; }

          printf '%-32s %12s %12s %s\n' "TASK" "WORKSPACE" "AGENTSTATE" "IN USE BY"
          for dir in "$WORKSPACE_ROOT"/*; do
              [[ -d "$dir" ]] || continue
              task="$(basename -- "$dir")"
              ws_bytes="$(dir_bytes "$dir")"
              st_bytes=0
              if [[ -d "$STATE_TASKS_ROOT/$task" ]]; then
                  st_bytes="$(dir_bytes "$STATE_TASKS_ROOT/$task")"
              fi
              total_ws=$(( total_ws + ws_bytes ))
              total_st=$(( total_st + st_bytes ))
              # Which slot (if any) currently holds this task.
              local holder="-" f
              for f in "$SLOTS_DIR"/*/session.json; do
                  [[ -e "$f" ]] || continue
                  if [[ "$(jq -r '.task // ""' "$f" 2>/dev/null || true)" == "$task" ]]; then
                      holder="$(jq -r '.slot' "$f")"
                      break
                  fi
              done
              printf '%-32s %12s %12s %s\n' "$task" "$(human "$ws_bytes")" \
                  "$(human "$st_bytes")" "$holder"
          done
          printf '%-32s %12s %12s\n' "TOTAL" "$(human "$total_ws")" "$(human "$total_st")"

          # Transient runtime data, for completeness: per-slot job dirs and the
          # archived results. These are small, but a runaway agent could fill the
          # job output dir.
          printf '\nruntime:\n'
          printf '  workspaces:    %s (%s)\n' "$WORKSPACE_ROOT" "$(human "$(dir_bytes "$WORKSPACE_ROOT")")"
          printf '  agent state:   %s (%s)\n' "$STATE_TASKS_ROOT" "$(human "$(dir_bytes "$STATE_TASKS_ROOT")")"
          ${jobDataUsageLine}
          printf '  job results:   %s (%s)\n' "$RESULTS_DIR" "$(human "$(dir_bytes "$RESULTS_DIR")")"
          printf '\nremove a retained workspace (and its task state) with:\n'
          printf '  %s workspace-remove <task>\n' "$PROG"
      }

      cmd_stop() {
          require_root stop
          [[ $# -ge 1 ]] || die "stop: <slot|task> required"
          local slot
          slot="$(resolve_slot "$1")" || die "no such slot or task: $1"
          set_event_context "$(marker_field "$slot" task || true)" "$slot" \
              "$(marker_field "$slot" agent || true)" "$(slot_class "$slot")" \
              "$(marker_field "$slot" mode || true)"
          log "stopping $slot (workspace kept)"
          # A leaked bind must reach the OPERATOR as a non-zero exit: `stop`
          # returning 0 while a bind survives is exactly the "claimed success"
          # this feature refuses everywhere else. The events are still emitted
          # first, because the teardown itself did happen.
          local leaked=0
          cleanup_slot "$slot" || leaked=1
          emit_event vm-stopped
          emit_event cleanup-completed
          (( ! leaked )) \
              || die "slot $slot was released but a bind mount SURVIVED; run '$PROG recover' once its holder is gone"
      }

      cmd_destroy() {
          require_root destroy
          [[ $# -ge 1 ]] || die "destroy: <slot|task> required"
          local slot
          slot="$(resolve_slot "$1")" || die "no such slot or task: $1"
          # §35: destroy removes ephemeral runtime + slot transient + bind
          # mount + VM process state, but must NOT delete workspace/git/patches.
          log "destroying $slot ephemeral state (workspace kept)"
          cleanup_slot "$slot" \
              || die "slot $slot was released but a bind mount SURVIVED; run '$PROG recover' once its holder is gone"
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
          local mode timeout_s jstate persisted
          for slot in "''${targets[@]}"; do
              f="$(session_file "$slot")"
              ip="$(slot_ip "$slot")"
              mac="$(slot_mac "$slot")"
              cid="$(slot_cid "$slot")"
              if service_active "$slot"; then state="running"; else state="stopped"; fi
              task=""; workspace=""; agent=""; start=""; owner=""
              mode=""; timeout_s=""; persisted=""
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
                  persisted="$(jq -r '.persist_agent_state // ""' "$f")"
              fi
              # Job state: the VERIFIED live controller result while the slot
              # runs (or its progress phase, or `rejected` when the document
              # does not belong to this allocation), else the archived result of
              # the last run of that task. A worker-written file is never read.
              jstate=""
              if [[ -e "$f" ]]; then
                  local stoken sagent svrc=0
                  stoken="$(jq -r '.token // ""' "$f" 2>/dev/null || true)"
                  sagent="$(jq -r '.agent // ""' "$f" 2>/dev/null || true)"
                  if [[ "$(id -u)" -ne 0 ]]; then
                      # The live controller channel is root-only 0700, so for a
                      # non-root caller "no result yet" and "permission denied"
                      # are indistinguishable. Say so instead of silently
                      # implying the job has no state.
                      jstate="unreadable (run as root)"
                  elif [[ -n "$task" && -n "$stoken" && -n "$sagent" ]]; then
                      verify_job_result "$slot" "$task" "$stoken" "$sagent" || svrc=$?
                      if (( svrc == 0 )); then
                          jstate="$(jq -r '.state // ""' <<< "$VERIFY_JSON")"
                      elif (( svrc == 3 )); then
                          jstate="unverifiable (host-side verifier error)"
                      elif (( svrc == 2 )); then
                          jstate="rejected (protocol error)"
                      else
                          jstate="$(job_phase "$slot" "$task" "$stoken" "$sagent" || true)"
                          [[ -z "$jstate" ]] || jstate="running ($jstate)"
                      fi
                  fi
              fi
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
        class:     $(slot_class "$slot") ($(slot_vcpu "$slot") vCPU, $(slot_mem "$slot") MiB)
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
        agent state: $( [[ "''${persisted:-0}" == "1" ]] && echo "persisted (task-scoped)" || echo "disposable" )
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
              printf '%-18s %-8s %-8s %-16s %s\n' "$slot" "$(slot_class "$slot")" \
                  "$state" "$(slot_ip "$slot")" "''${task:-<free>}"
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
                      # A surviving bind here is FATAL, not a warning: the very
                      # next step is `rm -rf` of the clone, which would delete
                      # the task's files THROUGH the mount.
                      cleanup_slot "$slot" \
                          || die "refusing to remove $clone: slot $slot still has a bind mount on it (run '$PROG recover' once its holder is gone)"
                  else
                      die "workspace is in use by slot $slot; stop it first with: $PROG stop $slot"
                  fi
              fi
          done
          log "removing workspace $clone"
          rm -rf -- "$clone"
          # The task's persisted agent state is part of the same task and would
          # otherwise be orphaned (and keep growing) after its workspace is
          # gone. Nothing outside <state>/tasks/<task> is touched.
          if [[ -d "$STATE_TASKS_ROOT/$task" ]]; then
              log "removing task-scoped agent state $STATE_TASKS_ROOT/$task"
              rm -rf -- "''${STATE_TASKS_ROOT:?}/$task"
          fi
          # Same for the archived job result of that task.
          rm -f -- "$RESULTS_DIR/$task.json"
      }

      # ==== `doctor`: read-only host-side diagnosis ==========================
      # The preflight inside `run`/`submit` is a single fast go/no-go. `doctor`
      # is the DEEP, read-only diagnosis an operator runs when that preflight
      # fires (or when a guest mysteriously cannot reach the model API): it
      # reports the state of every component the model-API path depends on, so
      # the 95-min “why does the worker die after 2 s?” investigation collapses
      # to one command. Root-only: it reads root-owned host keys and inspects
      # iptables. Exits non-zero if ANY check fails, so it is scriptable.
      cmd_doctor() {
          require_root doctor
          local problems=0 url loopback_url
          # A one-line summary line per component; prefixed `OK` / `FAIL`.
          report() { printf '%-7s %s\n' "$1" "$2"; }
          fail()   { report FAIL "$1"; problems=$((problems + 1)); }
          ok()     { report OK   "$1"; }

          printf '%s: host-side diagnosis (bridge=%s gateway=%s litellmPort=%s profile=%s)\n' \
              "$PROG" "$BRIDGE" "$GATEWAY" "$LITELLM_PORT" "''${NETWORK_PROFILE:-<unset>}"
          section_hdr() { printf '\n== %s ==\n' "$1"; }

          if [[ "$LITELLM_CAPABLE" != "1" ]]; then
              ok "effective profile grants no model-API access (no LiteLLM checks needed)"
              printf '\n%s: %d problem(s)\n' "$PROG" "$problems"
              (( problems == 0 ))
              return
          fi

          section_hdr "host LiteLLM backend (loopback 127.0.0.1:$LITELLM_PORT)"
          if systemctl is-active --quiet litellm.service 2>/dev/null; then
              ok "litellm.service is active"
          else
              fail "litellm.service is NOT active (systemctl start litellm)"
          fi
          loopback_url="http://127.0.0.1:$LITELLM_PORT/v1/models"
          if curl -fsS -m "$PREFLIGHT_TIMEOUT" --connect-timeout "$PREFLIGHT_TIMEOUT" \
                  -o /dev/null "$loopback_url" 2>/dev/null; then
              ok "loopback LiteLLM answers GET $loopback_url"
          else
              fail "loopback LiteLLM does NOT answer $loopback_url (is litellm.service running on port $LITELLM_PORT?)"
          fi

          section_hdr "bridge-only forwarder socket ($GATEWAY:$LITELLM_PORT)"
          # The socket must be active AND ordered after the bridge netdev; a
          # socket that failed at boot (BindToDevice before the bridge exists)
          # stays failed and has no listener.
          if systemctl is-active --quiet agent-litellm-proxy.socket 2>/dev/null; then
              ok "agent-litellm-proxy.socket is active"
          else
              fail "agent-litellm-proxy.socket is NOT active (it needs $BRIDGE-netdev.service; try: systemctl restart agent-litellm-proxy.socket)"
          fi
          # Match the unit name LITERALLY: `.` is a regex wildcard in grep,
          # and a bare "$BRIDGE-netdev.service" would also match a (hypothetical)
          # `agentbr0-netdevXservice`. Escape every `.` — the same hardening the
          # gateway-address grep below uses — so the match is exact.
          if systemctl show -p After --value agent-litellm-proxy.socket 2>/dev/null \
                  | grep -qw -- "''${BRIDGE//./\\.}-netdev\\.service"; then
              ok "socket is ordered after $BRIDGE-netdev.service"
          else
              fail "socket is NOT ordered after $BRIDGE-netdev.service (a boot race leaves it with no listener)"
          fi
          url="http://$GATEWAY:$LITELLM_PORT/v1/models"
          if curl -fsS -m "$PREFLIGHT_TIMEOUT" --connect-timeout "$PREFLIGHT_TIMEOUT" \
                  -o /dev/null "$url" 2>/dev/null; then
              ok "bridge endpoint answers GET $url"
          else
              fail "bridge endpoint does NOT answer $url (the guest-forwarded path is broken)"
          fi

          section_hdr "private bridge + gateway address"
          if ip -br link show "$BRIDGE" >/dev/null 2>&1; then
              ok "bridge interface $BRIDGE exists"
          else
              fail "bridge interface $BRIDGE does NOT exist"
          fi
          if ip -br addr show "$BRIDGE" 2>/dev/null \
                  | grep -qw -- "''${GATEWAY//./\\.}"; then
              ok "bridge carries the gateway address $GATEWAY"
          else
              fail "bridge $BRIDGE does NOT carry the gateway address $GATEWAY"
          fi

          section_hdr "firewall (AGENT_MICROVM_* chains)"
          if iptables -L AGENT_MICROVM_INPUT -n >/dev/null 2>&1; then
              ok "AGENT_MICROVM_INPUT chain is installed"
          else
              fail "AGENT_MICROVM_INPUT chain is NOT installed (reload the firewall / switch-to-configuration)"
          fi
          # Test the rule instead of parsing `iptables -S`'s printed form.
          # `iptables -S` CANONICALISES the destination address: a rule added
          # as `-d 192.168.83.1` is printed back as `-d 192.168.83.1/32`, and a
          # grep for "-d <addr> <space> ..." therefore never matched — so the
          # check ALWAYS reported a problem and `doctor` exited non-zero on a
          # healthy host (breaking `sudo agent-microvm doctor && ...`).
          # `iptables -C <chain> <spec>` exits 0 iff a rule matching <spec>
          # exists; it is exactly what network.nix itself uses to guard its own
          # idempotent `-I` inserts. The spec is built from the SAME
          # `$SUBNET`/`$GATEWAY`/`$LITELLM_PORT` variables network.nix's
          # `inputAllowLines` installs the rule with, so the two cannot drift —
          # and `-C` still returns non-zero when the rule is genuinely absent.
          if iptables -C AGENT_MICROVM_INPUT \
                  -s "$SUBNET" -d "$GATEWAY" -p tcp --dport "$LITELLM_PORT" -j ACCEPT \
                  2>/dev/null; then
              ok "INPUT chain ACCEPTs tcp dport $LITELLM_PORT to $GATEWAY from the subnet"
          else
              fail "INPUT chain does NOT ACCEPT the LiteLLM endpoint (guest -> $GATEWAY:$LITELLM_PORT)"
          fi
          if iptables -L AGENT_MICROVM_FORWARD -n >/dev/null 2>&1; then
              ok "AGENT_MICROVM_FORWARD chain is installed"
          else
              fail "AGENT_MICROVM_FORWARD chain is NOT installed"
          fi

          section_hdr "per-slot SSH host keys"
          local missing=0 s
          for s in "''${SLOT_NAMES[@]}"; do
              [[ -e "$HOSTKEYS_ROOT/$s" ]] || missing=$((missing + 1))
          done
          if (( missing == 0 )); then
              ok "every slot has a host-key directory under $HOSTKEYS_ROOT"
          else
              fail "$missing slot(s) lack a host-key directory (systemctl start agent-microvm-hostkeys.service)"
          fi

          printf '\n%s: %d problem(s)\n' "$PROG" "$problems"
          (( problems == 0 ))
      }

      main() {
          [[ $# -ge 1 ]] || usage
          local cmd="$1"; shift
          case "$cmd" in
              run)              cmd_run "$@" ;;
              submit)           cmd_submit "$@" ;;
              cancel)           cmd_cancel "$@" ;;
              recover)          cmd_recover "$@" ;;
              usage)            cmd_usage "$@" ;;
              stop)             cmd_stop "$@" ;;
              destroy)          cmd_destroy "$@" ;;
              status)           cmd_status "$@" ;;
              doctor)           cmd_doctor "$@" ;;
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
