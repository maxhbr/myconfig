# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.microvm — OPT-IN, TASK-SCOPED agent state (improvement ticket 5
# part B).
#
# The guest home is a tmpfs and DISPOSABLE by default: every sandbox starts with
# no agent memories, skills, sessions or caches. Some agents (hermes today) get
# materially better with persistence, so the registry declares — per agent, from
# VERIFIED source paths — which directories are worth keeping
# (`persistentState.directories`, relative to the guest home; see ./agents.nix).
#
# Persistence is NEVER on by default: it must be requested per run with
# `agent-microvm run|submit --persist-agent-state`.
#
# Layout
# ------
#   ${runtimeRoot}/state/tasks/<task>/<agent>/<dir>   per TASK + AGENT, kept
#   ${runtimeRoot}/sessions/<slot>/state              the bind target inside the
#                                                     ONE writable share
#
# The per-slot directory is part of the session tree baked into the guest config
# (a share source cannot be per-task, since the pool is prebuilt). The launcher
# `mount --bind`s the per-task directory onto it while the slot runs — exactly
# the mechanism the workspace clone already uses — and leaves it EMPTY when
# persistence was not requested. So:
#
#   * task A's state is only ever visible while task A's slot runs; task B gets
#     its own directory and can never read A's (no shared parent is exposed);
#   * without `--persist-agent-state` the guest sees an empty directory, the
#     guest-side linker finds nothing to link, and the agent writes into its
#     disposable tmpfs home;
#   * only the DECLARED subdirectories are ever exposed — never the host home,
#     `~/.ssh`, an SSH agent socket, a Docker/Podman socket, the Nix daemon
#     socket, host-wide agent configuration, or another task's state.
#
# Guest side: `agent-state-link.service` (root, oneshot, ordered before the
# batch job CONTROLLER and before logins) symlinks each declared directory that
# exists in the share into the guest home. Ordering against the controller — not
# against the worker — is deliberate: the controller starts the (untrusted)
# worker only after it has validated the job, so the symlinks are in place
# before any agent process exists, and a template unit cannot be ordered
# against without an instance name anyway. It is driven by the registry, so a
# newly declared directory needs no change here.
{
  config,
  lib,
  pkgs,
  agentRegistry,
  # The ONE definition of the per-session tree (./session.nix): the per-slot
  # bind TARGET is the session tree's `state/` subdirectory, reached through the
  # one writable share. The per-TASK directories below
  # `<runtimeRoot>/state/tasks/` are host-only and independent of it.
  agentSession,
  ...
}:
let
  cfg = config.myconfig.ai.microvm;
  session = agentSession;

  # Union of every agent's declared state directories (relative to the guest
  # home). Sorted + deduplicated so the generated guest script is stable.
  declaredDirs = lib.unique (
    lib.sort (a: b: a < b) (
      lib.concatMap (a: a.persistentState.directories) (lib.attrValues agentRegistry.agents)
    )
  );

  paths = rec {
    root = "${cfg.runtimeRoot}/state";
    # Per-task, per-agent state (kept across runs of that task).
    tasksRoot = "${root}/tasks";
    taskDir = task: agent: "${tasksRoot}/${task}/${agent}";
    # Per-slot bind target = the `state/` subdirectory of the ONE writable
    # session share (./session.nix). `slotsRoot` is the historical (four-share)
    # location; nothing creates anything under it any more, but the launcher's
    # foreign-state scan still reports RESIDUE a host carries from before the
    # consolidation.
    slotsRoot = "${root}/slots";
    slotDir = slotName: session.hostStateDir slotName;

    guestMountPoint = session.guestStateDir;
    guestHome = "/home/agent";
    inherit declaredDirs;
  };

  # --- guest-side linker -------------------------------------------------
  agent-state-link = pkgs.writeShellApplication {
    name = "agent-state-link";
    runtimeInputs = with pkgs; [ coreutils ];
    text = ''
      set -euo pipefail

      readonly SHARE=${lib.escapeShellArg paths.guestMountPoint}
      readonly HOME_DIR=${lib.escapeShellArg paths.guestHome}
      log() { printf 'agent-state-link: %s\n' "$*" >&2; }

      # No share (or an empty one) means "no persistence for this run": the
      # agent then uses its disposable tmpfs home. That is the DEFAULT.
      if [[ ! -d "$SHARE" ]]; then
          log "no agent-state share at $SHARE; keeping the disposable home"
          exit 0
      fi

      link_one() {
          local rel="$1"
          local src="$SHARE/$rel"
          local target="$HOME_DIR/$rel"
          # Only directories the HOST prepared are linked, so a run without
          # --persist-agent-state links nothing.
          [[ -d "$src" ]] || return 0
          mkdir -p -- "$(dirname -- "$target")"
          if [[ -L "$target" ]]; then
              rm -f -- "$target"
          elif [[ -e "$target" ]]; then
              # Never destroy real data in the home: only an EMPTY directory is
              # replaced (the tmpfs home may already hold a freshly created,
              # empty dir), anything else is left alone and reported.
              if [[ -d "$target" ]] && rmdir -- "$target" 2>/dev/null; then
                  :
              else
                  log "refusing to replace non-empty $target with the persisted state"
                  return 0
              fi
          fi
          ln -s -- "$src" "$target"
          # Deliberately NO chown of the symlink: Linux ignores symlink
          # ownership/permissions for access decisions, and what actually
          # matters — the ownership of the TARGET directory tree — was set by
          # the host when it created the task-scoped state
          # (guestAgentUid/guestAgentGid, passed through unchanged by virtiofsd).
          log "linked $target -> $src"
      }

      ${lib.concatMapStringsSep "\n" (d: "      link_one ${lib.escapeShellArg d}") declaredDirs}
    '';
    meta = with lib; {
      description = "Link task-scoped agent state into the guest home (myconfig.ai.microvm)";
      platforms = platforms.linux;
    };
  };

  guestModule = {
    systemd.services.agent-state-link = {
      description = "Link task-scoped agent state into the guest home";
      wantedBy = [ "multi-user.target" ];
      # The share must be mounted first; the linker runs before the batch job
      # controller (which is what starts the untrusted worker) and, in practice,
      # well before an interactive login.
      unitConfig.RequiresMountsFor = paths.guestMountPoint;
      before = [ "agent-job-controller.service" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = lib.getExe agent-state-link;
      };
    };
  };
in
{
  config = lib.mkMerge [
    {
      _module.args.agentState = paths // {
        inherit guestModule;
        linker = agent-state-link;
      };
    }

    (lib.mkIf cfg.enable {
      # The per-TASK tree is host-only. The per-SLOT bind TARGET is part of the
      # session tree, which ./session.nix creates from the ONE layout table —
      # emitting it here as well would duplicate the same rule.
      systemd.tmpfiles.rules = [
        "d ${paths.root} 0755 root root - -"
        "d ${paths.tasksRoot} 0755 root root - -"
      ];
    })
  ];
}
