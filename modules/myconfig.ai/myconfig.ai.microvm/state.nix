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
#   ${runtimeRoot}/state/slots/<slot>                 the share source
#
# The per-slot directory is the share source baked into the guest config (a
# share source cannot be per-task, since the pool is prebuilt). The launcher
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
  agentResourceClasses,
  ...
}:
let
  cfg = config.myconfig.ai.microvm;

  slots = (import ./slots.nix { inherit lib; }).mkSlots agentResourceClasses;

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
    # Per-slot bind target = the virtiofs share source.
    slotsRoot = "${root}/slots";
    slotDir = slotName: "${slotsRoot}/${slotName}";

    guestTag = "agentstate";
    guestMountPoint = "/var/lib/agent-state";
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
      assertions = [
        {
          # A persisted directory that also comes from the host primary user's
          # home-manager dotfiles would be fought over by HM activation and the
          # linker. Keep the two disjoint.
          #
          # This covers the `full` provisioning path ONLY (it is gated on
          # `guestDotfiles.enable`). The mirror guard for the `lite` path —
          # runtime config staging vs. these same directories — lives in
          # ./config-seed.nix, which owns that allowlist.
          assertion =
            let
              hmPrefixes = cfg.guestDotfiles.homeFilePrefixes;
              collides = d: lib.any (p: lib.hasPrefix p "${d}/" || lib.hasPrefix "${d}/" p) hmPrefixes;
            in
            !cfg.guestDotfiles.enable || !(lib.any collides declaredDirs);
          message = ''
            myconfig.ai.microvm: an agent's persistentState.directories collides
            with guestDotfiles.homeFilePrefixes. The guest home-manager
            activation and the agent-state linker would fight over the same
            path — keep the persisted state directories disjoint from the
            provisioned dotfiles.
          '';
        }
      ];

      # virtiofsd needs its share source to exist before the VM starts, for
      # EVERY slot — including slots that never persist anything (they then see
      # an empty directory). Owned by the guest agent so it can write there.
      systemd.tmpfiles.rules = [
        "d ${paths.root} 0755 root root - -"
        "d ${paths.tasksRoot} 0755 root root - -"
        "d ${paths.slotsRoot} 0755 root root - -"
      ]
      ++ map (
        slot:
        "d ${paths.slotDir slot.name} 0755 ${toString cfg.guestAgentUid} ${toString cfg.guestAgentGid} - -"
      ) slots;
    })
  ];
}
