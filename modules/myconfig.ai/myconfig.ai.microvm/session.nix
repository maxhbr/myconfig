# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.microvm — THE CONSOLIDATED PER-SESSION TREE (lightweight plan
# phase 4), i.e. the single source of truth for every path, owner and mode of
# the ONE writable virtiofs share (plus the ONE read-only share) every guest
# gets.
#
# Problem
# -------
# Historically every guest carried FOUR virtiofs shares — the workspace, the
# batch job directory, the agent-state directory and (since phase 3) the config
# seed — plus the read-only SSH host-key share. Each of them was a separate host
# source directory, a separate guest mount unit and, most importantly, a
# separate thing to create, verify, unmount and clean up. Phase 4 collapsed that
# into (and the `full`-profile removal made it the ONLY layout):
#
#   ${runtimeRoot}/sessions/<slot>/          root:root 0755   ONE WRITABLE SHARE
#     workspace/                             agent            the standalone clone
#                                                             (host bind mount)
#     input/                                 root:root 0755   immutable job input
#     controller/                            root:root 0700   TRUSTED result channel
#     worker/                                agent     0755   untrusted worker area
#     worker-logs/                           root:root 0755   root-opened worker logs
#     state/                                 agent     0755   opt-in agent state
#                                                             (host bind mount)
#
#   ${runtimeRoot}/sessions-ro/<slot>/        root:root 0700   ONE READ-ONLY SHARE
#     hostkeys/                              root:root 0700   the slot's ed25519
#                                                             host identity (0400)
#     config-seed/                           root:root 0500   the staged, allowlisted
#                                                             host agent configuration
#
# so a guest has EXACTLY ONE writable share and AT MOST ONE read-only share,
# and the launcher has exactly one tree to create, verify and remove.
#
# PER-CAPABILITY ENTRIES (lightweight plan phase 5)
# ------------------------------------------------
# Each entry of the tables below declares WHICH capabilities
# (`myconfig.ai.microvm.capabilities`) need it, and only the selected entries
# are created, verified and swept. The SHAPE is unchanged — still one writable
# and one read-only share, still the same paths and modes — a narrowed host
# simply has fewer subdirectories in them:
#
#   capabilities = [ "interactive" ]  no `input/`, `controller/`, `worker/`,
#                                     `worker-logs/` (no batch protocol at all)
#   capabilities = [ "batch" ]        no `hostkeys/` in the read-only tree (no
#                                     SSH host identity at all)
#
# Putting the decision HERE is what keeps it out of the consumers: the host
# tmpfiles rules, the generated pre-launch verifier, the launcher's
# `prepare_session`, the guest mounts and tests/microvm.nix are all rendered
# from the same filtered table.
#
# Two rules keep the narrowing from WEAKENING anything:
#
#   * the trust POLICY (`violationsOf`) and `modeOf`/`roModeOf` read the FULL
#     tables, so a weakening edit to an entry this host does not create still
#     fails evaluation, and a consumer may ask "what mode does `controller/`
#     have" without knowing this host's selection;
#   * the generated verifier additionally refuses any top-level entry the
#     FILTERED table does not declare. Narrowing stops creating (and verifying)
#     the other capability's subdirectories, but nothing removes the ones a
#     previous generation created — a leftover root-owned `input/` would
#     otherwise sit in the ONE writable share, exported to the guest, with no
#     rule covering it. The launcher's `prepare_session` sweeps them before every
#     launch; the verifier is the fail-closed half of that pair.
#
# WHY THE READ-ONLY TREE IS SEPARATE (deviation from the plan's sketch)
# ---------------------------------------------------------------------
# The plan's target tree puts `config-seed/` INSIDE the writable session share
# (mode 0555). Two of its own invariants outrank that layout sketch:
#
#   * invariant 7 — the guest must not be able to modify trusted control data.
#     The staged configuration is host-decided input; keeping it in a share
#     virtiofsd mounts `--readonly` is strictly stronger than relying on modes
#     inside a writable share (phase 3 already made this choice, see the
#     recorded deviations in the plan document).
#   * invariant 8 / "Keep SSH private host keys in a separate read-only share.
#     Do not place them in the writable session tree." — the plan says this
#     itself, and the same argument applies to the staged configuration.
#
# Since BOTH read-only payloads are root-owned, single-purpose and per-slot,
# they share ONE read-only virtiofs share (two subdirectories) rather than one
# share each: that satisfies the acceptance criterion "one writable virtiofs
# share plus at most one read-only share" without weakening anything. The
# staging MANIFEST stays outside every share (see ./config-seed.nix).
#
# TRUST BOUNDARIES ARE OWNERSHIP + MODES
# --------------------------------------
# virtiofsd passes ownership and modes through unchanged (no
# --translate-uid/gid), so the host-side owner/mode of every directory below IS
# the effective permission inside the guest. That is what keeps the untrusted
# guest `agent` user out of `input/`, `controller/`, `worker-logs/` and the
# whole read-only tree, exactly as ./job.nix and ./config-seed.nix already
# document for the four-share layout. The table below is the ONE place those
# facts are written down: ./job.nix, ./state.nix, ./config-seed.nix and
# ./hostkeys.nix derive their paths and modes from it, the host tmpfiles rules
# are generated from it, the pre-launch verifier is generated from it, and
# tests/microvm.nix asserts against it.
#
# There is no opt-out: this is the ONLY share layout the module knows. The
# capability set selects which of its SUBDIRECTORIES exist, never a second
# layout.
{
  config,
  lib,
  pkgs,
  # The effective resource-class table (see default.nix): the slot pool whose
  # per-slot share sources must exist before any VM starts.
  agentResourceClasses,
  # The ONE resolved capability set (see default.nix, lightweight plan phase 5).
  # THIS is where the interactive/batch decision belongs: every entry of the
  # layout table below declares WHICH capabilities need it, so a narrowed host
  # simply has fewer directories — and the tmpfiles rules, the pre-launch
  # verifier, the guest mounts, the launcher's `prepare_session` and the tests
  # all follow without a single `if` of their own.
  agentCapabilities,
  ...
}:
let
  cfg = config.myconfig.ai.microvm;

  slots = (import ./slots.nix { inherit lib; }).mkSlots agentResourceClasses;

  # ---- owners ----------------------------------------------------------
  # Only two identities ever own anything in a session tree: host root (the
  # TRUSTED side) and the guest `agent` user's numeric uid/gid (the UNTRUSTED
  # side). Everything is expressed through these two, so there is no third
  # identity to reason about.
  uidOf = owner: if owner == "root" then 0 else cfg.guestAgentUid;
  gidOf = owner: if owner == "root" then 0 else cfg.guestAgentGid;

  # ---- structure -------------------------------------------------------
  subdirs = {
    workspace = "workspace";
    input = "input";
    controller = "controller";
    worker = "worker";
    workerLogs = "worker-logs";
    state = "state";
  };
  roSubdirs = {
    hostkeys = "hostkeys";
    configSeed = "config-seed";
  };

  root = "${cfg.runtimeRoot}/sessions";
  roRoot = "${cfg.runtimeRoot}/sessions-ro";
  rootMode = "0755";
  # Root-ONLY: it carries the slot's private SSH host key and the operator's
  # staged configuration, so no unprivileged HOST user may traverse it either.
  roRootMode = "0700";

  # ---- THE layout table (the single source of truth) --------------------
  # `rel = ""` is the per-slot directory itself. `strictMode` marks the
  # directories whose mode the host fully controls (so the verifier can demand
  # an EXACT mode); the two bind-mount points show the mode of whatever is
  # mounted on them, so only their owner and the "no group/other write" rule
  # are asserted. `private` additionally forbids every group/other bit.
  #
  # `uid`/`gid` are attached by `withIds` below, so the owner -> numeric-id
  # mapping exists exactly once and every consumer (the host tmpfiles rules, the
  # pre-launch verifier and the launcher's tree preparation) uses the same
  # numbers.
  # `capabilities` is the PER-CAPABILITY part of the table (lightweight plan
  # phase 5):
  #
  #   * `null`      — UNCONDITIONAL: the entry is part of every guest, whatever
  #                   the host selects. Spelled as its own value rather than as
  #                   "the list of all declared capabilities", because those two
  #                   are only the same thing while exactly two tokens exist: a
  #                   third token (a phase-6 `vsock`, say) added to a list that
  #                   was meant to say "interactive AND batch need this" would
  #                   silently start giving the entry to a vsock-only host too.
  #   * a LIST      — the entry exists iff the host selected at least ONE of the
  #                   listed capabilities. Strictly ENUMERATIVE: it never refers
  #                   to `agentCapabilities.declared`, so adding a capability
  #                   cannot change the meaning of an existing entry.
  #
  # An empty list would mean "no host ever creates this", i.e. dead table data;
  # the assertion at the bottom of this file rejects it, together with an unknown
  # token in a list.
  rawLayout = [
    {
      rel = "";
      owner = "root";
      mode = "0755";
      strictMode = true;
      private = false;
      capabilities = null;
      purpose = "the session root (root-owned, so the guest agent cannot rename or shadow anything below it)";
    }
    {
      rel = subdirs.workspace;
      owner = "agent";
      mode = "0755";
      strictMode = false;
      private = false;
      capabilities = null;
      purpose = "the standalone clone, bind-mounted by the launcher and surfaced as /workspace in the guest";
    }
    {
      rel = subdirs.input;
      owner = "root";
      mode = "0755";
      strictMode = true;
      private = false;
      capabilities = [ "batch" ];
      purpose = "the IMMUTABLE batch job input (spec 0400, prompt 0444)";
    }
    {
      rel = subdirs.controller;
      owner = "root";
      mode = "0700";
      strictMode = true;
      private = true;
      capabilities = [ "batch" ];
      purpose = "the TRUSTED guest controller's channel, where the authoritative result is written";
    }
    {
      rel = subdirs.worker;
      owner = "agent";
      mode = "0755";
      strictMode = true;
      private = false;
      capabilities = [ "batch" ];
      purpose = "the UNTRUSTED worker's own area (the only writable part of the job data)";
    }
    {
      rel = subdirs.workerLogs;
      owner = "root";
      mode = "0755";
      strictMode = true;
      private = false;
      capabilities = [ "batch" ];
      purpose = "the worker's stdout/stderr, opened as root by the guest's systemd (so no component may be replaceable by the agent)";
    }
    {
      rel = subdirs.state;
      owner = "agent";
      mode = "0755";
      strictMode = false;
      private = false;
      capabilities = null;
      purpose = "the opt-in, task-scoped agent state, bind-mounted by the launcher";
    }
  ];

  rawRoLayout = [
    {
      rel = "";
      owner = "root";
      mode = roRootMode;
      strictMode = true;
      private = true;
      capabilities = null;
      purpose = "the read-only tree of the slot (host identity + staged configuration)";
    }
    {
      rel = roSubdirs.hostkeys;
      owner = "root";
      mode = "0700";
      strictMode = true;
      private = true;
      # The per-slot SSH host identity backs the `interactive` control channel
      # (the TCP sshd) AND the `vsock` control channel (the `sshd-vsock@`
      # unit, lightweight plan phase 6): a batch-only host that selects `vsock`
      # has no TCP sshd but still needs a pinned host key so the VSOCK channel
      # is host-key-verified exactly like the TAP one. `hostkeys.nix` provisions
      # the key pair + the known_hosts database for the same union.
      capabilities = [
        "interactive"
        "vsock"
      ];
      purpose = "the slot's ed25519 SSH host identity (private key 0400, root-only)";
    }
    {
      rel = roSubdirs.configSeed;
      owner = "root";
      mode = "0500";
      strictMode = true;
      private = true;
      capabilities = null;
      purpose = "the staged, allowlisted host agent configuration (payload of ./config-seed.nix)";
    }
  ];

  # THE per-capability filter, applied exactly once per tree.
  selectedByCapability = lib.filter (
    e: e.capabilities == null || lib.any (c: lib.elem c agentCapabilities.selected) e.capabilities
  );
  # Table HYGIENE, checked by an assertion below rather than trusted: an entry
  # with an empty capability list is dead data (no host would ever create it) and
  # one naming an undeclared token is a typo that would silently drop a
  # directory — including its trust-relevant owner/mode — from every host.
  malformedCapabilityEntries = lib.filter (
    e:
    e.capabilities != null
    && (e.capabilities == [ ] || lib.any (c: !(lib.elem c agentCapabilities.declared)) e.capabilities)
  ) (rawLayout ++ rawRoLayout);

  withIds = map (
    e:
    e
    // {
      uid = uidOf e.owner;
      gid = gidOf e.owner;
    }
  );
  # The FULL tables (every capability's entries) and the SELECTED ones. Only the
  # selected entries are created, verified and mounted; the full tables stay the
  # authority over a directory's owner/mode, so a consumer may ask "what mode
  # does `controller/` have" (./job.nix does, for the guest-side assertions it
  # bakes in) without having to know whether this host creates it.
  fullLayout = withIds rawLayout;
  fullRoLayout = withIds rawRoLayout;
  layout = selectedByCapability fullLayout;
  roLayout = selectedByCapability fullRoLayout;

  # ---- the POLICY the layout must satisfy ------------------------------
  # Exposed as a FUNCTION (via `_module.args.agentSession.violationsOf`) so the
  # rules are not merely applied to the table below but can be exercised
  # against a deliberately broken one — tests/microvm.nix feeds it a layout
  # that makes `input/`/`controller/` guest-writable, or that puts the SSH host
  # keys into the writable tree, and requires it to complain.
  #
  # The module assertion at the bottom applies it to the REAL tables, so a
  # future edit that weakens the trust boundary fails evaluation instead of
  # producing a guest that quietly hands its control data to the agent.
  # Octal string -> integer (Nix has no octal literal, and `fromJSON` would
  # read "0700" as decimal 700).
  octalToInt =
    s:
    let
      digits = lib.stringToCharacters s;
    in
    lib.foldl' (acc: c: acc * 8 + lib.toInt c) 0 digits;

  # The directories whose ROOT ownership is the trust boundary itself: the
  # session root (nothing may be renamed or shadowed below it), the immutable
  # job input, the trusted controller channel and the root-opened worker logs.
  trustedWritableRels = [
    ""
    subdirs.input
    subdirs.controller
    subdirs.workerLogs
  ];

  violationsOf =
    {
      writableRoot,
      readOnlyRoot,
      writable,
      readOnly,
      hostKeyDir,
      configSeedDir,
    }:
    let
      groupOtherWrite = e: (lib.bitAnd (octalToInt e.mode) (octalToInt "022")) != 0;
      groupOtherAny = e: (lib.bitAnd (octalToInt e.mode) (octalToInt "077")) != 0;
      pathOf = base: e: if e.rel == "" then base else "${base}/${e.rel}";
      inside = parent: p: p == parent || lib.hasPrefix "${parent}/" p;
    in
    # (1) nothing in either tree may be group/other-writable: the guest agent
    #     is a group/other identity for every root-owned path.
    map (e: "'${pathOf writableRoot e}' is group/other-writable (mode ${e.mode})") (
      lib.filter groupOtherWrite writable
    )
    ++ map (e: "read-only '${pathOf readOnlyRoot e}' is group/other-writable (mode ${e.mode})") (
      lib.filter groupOtherWrite readOnly
    )
    # (2) the trusted control directories must be ROOT-owned; an agent-owned
    #     `input/`, `controller/` or `worker-logs/` would let the untrusted
    #     side rewrite its own job or forge a result.
    ++ map (e: "'${pathOf writableRoot e}' must be root-owned, is owned by '${e.owner}'") (
      lib.filter (e: lib.elem e.rel trustedWritableRels && e.owner != "root") writable
    )
    # (3) ... and their modes must additionally deny group/other ACCESS where
    #     the data itself is confidential (the controller channel carries the
    #     allocation token).
    ++ map (e: "'${pathOf writableRoot e}' grants group/other access (mode ${e.mode})") (
      lib.filter (e: e.private && groupOtherAny e) writable
    )
    ++ map (e: "read-only '${pathOf readOnlyRoot e}' grants group/other access (mode ${e.mode})") (
      lib.filter (e: e.private && groupOtherAny e) readOnly
    )
    # (4) the whole read-only tree must be root-owned: it is the ONLY thing the
    #     guest may not influence at all.
    ++ map (e: "read-only '${pathOf readOnlyRoot e}' must be root-owned, is owned by '${e.owner}'") (
      lib.filter (e: e.owner != "root") readOnly
    )
    # (5) the SSH private host keys and the staged configuration must NEVER
    #     live in the WRITABLE session tree (the plan says so explicitly for
    #     the host keys; phase 3 established the same for the staged tree).
    ++ lib.optional (inside writableRoot hostKeyDir) "the SSH host-key directory '${hostKeyDir}' is inside the WRITABLE session tree '${writableRoot}'"
    ++ lib.optional (
      !(inside readOnlyRoot hostKeyDir)
    ) "the SSH host-key directory '${hostKeyDir}' is not inside the read-only tree '${readOnlyRoot}'"
    ++ lib.optional (inside writableRoot configSeedDir) "the staged configuration '${configSeedDir}' is inside the WRITABLE session tree '${writableRoot}'"
    ++ lib.optional (
      !(inside readOnlyRoot configSeedDir)
    ) "the staged configuration '${configSeedDir}' is not inside the read-only tree '${readOnlyRoot}'"
    # (6) the two trees must be disjoint, or the read-only one would be
    #     reachable through the writable share (and vice versa).
    ++ lib.optional (
      inside writableRoot readOnlyRoot || inside readOnlyRoot writableRoot
    ) "the writable tree '${writableRoot}' and the read-only tree '${readOnlyRoot}' overlap";

  # ---- the ONE definition of every session path / mode ------------------
  paths = rec {
    # ---- host side ----------------------------------------------------
    inherit
      root
      roRoot
      rootMode
      roRootMode
      subdirs
      roSubdirs
      layout
      roLayout
      fullLayout
      fullRoLayout
      violationsOf
      ;
    slotDir = slotName: "${root}/${slotName}";
    roSlotDir = slotName: "${roRoot}/${slotName}";
    hostWorkspaceDir = slotName: "${slotDir slotName}/${subdirs.workspace}";
    hostStateDir = slotName: "${slotDir slotName}/${subdirs.state}";
    hostHostkeysDir = slotName: "${roSlotDir slotName}/${roSubdirs.hostkeys}";
    hostConfigSeedDir = slotName: "${roSlotDir slotName}/${roSubdirs.configSeed}";

    # Mode of a layout entry, by its relative name — so a consumer (./job.nix,
    # ./config-seed.nix) does not carry a second copy of the number. `rel = ""`
    # is the per-slot directory of the respective tree.
    # Looked up in the FULL table on purpose: a directory's owner/mode is a
    # policy fact of the layout, independent of whether THIS host's capability
    # set creates the directory.
    modeOf =
      rel:
      (lib.findFirst (
        e: e.rel == rel
      ) (throw "myconfig.ai.microvm.session: no layout entry '${rel}'") fullLayout).mode;
    roModeOf =
      rel:
      (lib.findFirst (
        e: e.rel == rel
      ) (throw "myconfig.ai.microvm.session: no read-only layout entry '${rel}'") fullRoLayout).mode;

    # ---- guest side (identical for every slot — the share hides the slot) --
    guestTag = "session";
    guestMountPoint = "/run/agent-session";
    guestRoTag = "sessionro";
    guestRoMountPoint = "/run/agent-session-ro";
    # The workspace as the SHARE presents it ...
    guestWorkspaceSource = "${guestMountPoint}/${subdirs.workspace}";
    # ... and where every agent (and `agent-run`'s findmnt/writability checks)
    # expects it: a bind mount inside the guest, so nothing above the module
    # has to learn a new path.
    guestWorkspace = "/workspace";
    guestStateDir = "${guestMountPoint}/${subdirs.state}";
    guestHostkeysDir = "${guestRoMountPoint}/${roSubdirs.hostkeys}";
    guestConfigSeedDir = "${guestRoMountPoint}/${roSubdirs.configSeed}";
  };

  # ---- host tmpfiles rules (generated from the table) -------------------
  # virtiofsd refuses to start when a share source is missing, so BOTH trees
  # must exist for EVERY slot before any VM starts — including a slot that has
  # never been launched. The modes ARE the trust boundary (virtiofsd passes
  # them through unchanged), so they come from the table above rather than from
  # a hand-written list.
  tmpfilesRules = [
    "d ${root} ${rootMode} root root - -"
    "d ${roRoot} ${roRootMode} root root - -"
  ]
  ++ lib.concatMap (
    slot:
    map (
      e:
      "d ${
        if e.rel == "" then paths.slotDir slot.name else "${paths.slotDir slot.name}/${e.rel}"
      } ${e.mode} ${toString (uidOf e.owner)} ${toString (gidOf e.owner)} - -"
    ) layout
    ++ map (
      e:
      "d ${
        if e.rel == "" then paths.roSlotDir slot.name else "${paths.roSlotDir slot.name}/${e.rel}"
      } ${e.mode} ${toString (uidOf e.owner)} ${toString (gidOf e.owner)} - -"
    ) roLayout
  ) slots;

  # ---- host-side PRE-LAUNCH verifier -----------------------------------
  # Generated from the same table, run by the launcher (as root) after the
  # session tree has been prepared and everything staged into it, and BEFORE
  # the VM (and therefore virtiofsd) starts. A tree whose ownership or modes
  # are not exactly what the trust split needs must never be handed to a guest:
  # `controller/` at mode 0755 would let the agent forge the authoritative
  # result, and a symlinked `input/` would let it rewrite its own job.
  verifier = pkgs.writeShellApplication {
    name = "agent-microvm-verify-session";
    runtimeInputs = with pkgs; [
      coreutils # stat, realpath, dirname
      findutils # find
    ];
    text = ''
      set -euo pipefail

      readonly SESSION_ROOT=${lib.escapeShellArg root}
      readonly SESSION_RO_ROOT=${lib.escapeShellArg roRoot}
      readonly SLOTS=(${lib.concatMapStringsSep " " (s: lib.escapeShellArg s.name) slots})
      # The top-level entries the FILTERED table declares for this host, i.e.
      # everything that may legitimately exist directly inside a slot's two
      # trees. Anything else is refused below.
      readonly SESSION_ENTRIES=(${
        lib.concatMapStringsSep " " (e: lib.escapeShellArg e.rel) (lib.filter (e: e.rel != "") layout)
      })
      readonly SESSION_RO_ENTRIES=(${
        lib.concatMapStringsSep " " (e: lib.escapeShellArg e.rel) (lib.filter (e: e.rel != "") roLayout)
      })

      PROG="agent-microvm-verify-session"
      die() { printf '%s: REFUSING TO LAUNCH: %s\n' "$PROG" "$*" >&2; exit 1; }

      [[ $# -eq 1 ]] || { printf 'usage: %s <slot>\n' "$PROG" >&2; exit 64; }
      slot="$1"
      # The paths are derived from a slot name of the PREBUILT pool, never from
      # a caller-supplied path, so this command cannot be pointed elsewhere.
      slot_known=0
      for known in "''${SLOTS[@]}"; do
          if [[ "$known" == "$slot" ]]; then
              slot_known=1
          fi
      done
      (( slot_known )) || die "unknown slot '$slot'"

      readonly SESSION_DIR="$SESSION_ROOT/$slot"
      readonly SESSION_RO_DIR="$SESSION_RO_ROOT/$slot"

      # `stat` on the path ITSELF, never dereferencing a symlink.
      owner_of() { stat -c %u -- "$1"; }
      mode_of()  { stat -c %a -- "$1"; }

      # A directory only root may modify. Used for every PARENT of the two
      # trees: a mode 0700 controller directory is worthless if some
      # unprivileged identity can rename or replace a directory above it.
      assert_root_parent() {
          local d="$1" owner mode
          [[ -d "$d" ]] || die "a parent directory of the session tree is missing: $d"
          owner="$(owner_of "$d")"
          (( owner == 0 )) || die "parent directory $d must be owned by uid 0 (is $owner)"
          mode="$(mode_of "$d")"
          (( (8#"$mode" & 8#022) == 0 )) \
              || die "parent directory $d is group/other-writable (mode $mode)"
      }

      assert_parents() {
          local p
          p="$(realpath -m -- "$1")"
          while :; do
              p="$(dirname -- "$p")"
              assert_root_parent "$p"
              [[ "$p" == "/" ]] && break
          done
      }

      # ONE directory of the layout table. `mode` empty means "the host does
      # not fully control it" (a bind-mount point shows the mounted tree's
      # mode), in which case only the owner and the no-group/other-write rule
      # are enforced.
      verify_dir() {
          local path="$1" label="$2" uid="$3" mode="$4" private="$5" have_owner have_mode
          # A symlink here would mean the path we verified is not the path
          # virtiofsd exports — the classic traversal/escape.
          [[ ! -L "$path" ]] || die "$label is a SYMLINK (traversal/escape): $path"
          [[ -d "$path" ]] || die "$label is missing or not a directory: $path"
          have_owner="$(owner_of "$path")"
          [[ "$have_owner" == "$uid" ]] \
              || die "$label must be owned by uid $uid, is owned by $have_owner: $path"
          have_mode="$(mode_of "$path")"
          (( (8#"$have_mode" & 8#022) == 0 )) \
              || die "$label is group/other-writable (mode $have_mode): $path"
          if (( private )); then
              (( (8#"$have_mode" & 8#077) == 0 )) \
                  || die "$label grants group/other access (mode $have_mode): $path"
          fi
          if [[ -n "$mode" ]]; then
              [[ "$have_mode" == "$mode" ]] \
                  || die "$label must be mode $mode, is $have_mode: $path"
          fi
      }

      assert_parents "$SESSION_DIR"
      assert_parents "$SESSION_RO_DIR"

      # --- the WRITABLE tree (generated from the layout table) -------------
      ${lib.concatMapStringsSep "\n      " (
        e:
        "verify_dir ${if e.rel == "" then ''"$SESSION_DIR"'' else ''"$SESSION_DIR/${e.rel}"''} ${
          lib.escapeShellArg (if e.rel == "" then "the session root" else "the session '${e.rel}' directory")
        } ${toString e.uid} ${
          lib.escapeShellArg (if e.strictMode then lib.removePrefix "0" e.mode else "")
        } ${if e.private then "1" else "0"}"
      ) layout}

      # --- the READ-ONLY tree ----------------------------------------------
      ${lib.concatMapStringsSep "\n      " (
        e:
        "verify_dir ${if e.rel == "" then ''"$SESSION_RO_DIR"'' else ''"$SESSION_RO_DIR/${e.rel}"''} ${
          lib.escapeShellArg (
            if e.rel == "" then "the read-only session root" else "the read-only '${e.rel}' directory"
          )
        } ${toString e.uid} ${
          lib.escapeShellArg (if e.strictMode then lib.removePrefix "0" e.mode else "")
        } ${if e.private then "1" else "0"}"
      ) roLayout}

      # --- NOTHING the table does not declare may be in either tree ---------
      # The per-directory checks above verify every declared entry, which alone
      # says nothing about UNDECLARED ones. That gap is not theoretical: a host
      # that narrows `myconfig.ai.microvm.capabilities` (lightweight plan phase 5)
      # stops creating — and stops verifying — the other capability's
      # subdirectories, while the ones a previous generation created are still
      # there after an unclean shutdown (nothing sweeps them at boot). A leftover
      # root-owned `input/` or `worker-logs/` would then sit inside the ONE
      # writable share, exported to the guest, with no rule covering it at all.
      # The launcher's `prepare_session` sweeps such entries before every launch;
      # this is the FAIL-CLOSED half of that, so a sweep that did not happen
      # refuses the launch instead of exporting an unverified directory.
      assert_no_extras() {
          local dir="$1" label="$2"
          shift 2
          local name known allowed
          while IFS= read -r name; do
              known=0
              for allowed in "$@"; do
                  [[ "$name" == "$allowed" ]] && known=1
              done
              (( known )) || die "$label contains '$name', which the session layout table does not declare for this host (a stale entry from a previous myconfig.ai.microvm.capabilities selection, or an operator leftover): $dir/$name"
          done < <(find "$dir" -mindepth 1 -maxdepth 1 -printf '%f\n')
      }

      assert_no_extras "$SESSION_DIR" "the session tree" "''${SESSION_ENTRIES[@]}"
      assert_no_extras "$SESSION_RO_DIR" "the read-only session tree" "''${SESSION_RO_ENTRIES[@]}"

      # --- the SSH private host key never enters the writable tree ----------
      # Structural enforcement of the plan's own rule ("Keep SSH private host
      # keys in a separate read-only share. Do not place them in the writable
      # session tree."): a bug in a future refactor must fail the LAUNCH, not
      # quietly hand the slot's identity to the untrusted side.
      #
      # The two BIND-MOUNT points are PRUNED, and that is not a hole: by the
      # time this runs they carry the user's own git clone and the task's
      # persisted agent state, i.e. content the HOST never writes key material
      # into and the guest agent can create at will. Without the prune an
      # ordinary repository file (`somedir/ssh_host_ed25519_key.pub` is entirely
      # plausible in a NixOS/agenix clone, and a `.pub` is not secret) would
      # refuse the launch with a security error — and a hostile agent could
      # deny every future launch of the slot by creating one. Host-written key
      # material can only ever land in the root-owned subdirectories, which are
      # all still walked. The names come from the layout table, never from
      # literals.
      if [[ -n "$(find "$SESSION_DIR" -maxdepth 3 \
                      \( -path "$SESSION_DIR/${subdirs.workspace}" \
                         -o -path "$SESSION_DIR/${subdirs.state}" \) -prune -o \
                      -name 'ssh_host_*' -print -quit 2>/dev/null)" ]]; then
          die "SSH host-key material found in the WRITABLE session tree $SESSION_DIR"
      fi

      # ... and the key that IS there is root-only.
      key="$SESSION_RO_DIR/${roSubdirs.hostkeys}/ssh_host_ed25519_key"
      if [[ -e "$key" || -L "$key" ]]; then
          [[ ! -L "$key" ]] || die "the SSH host key is a symlink: $key"
          [[ -f "$key" ]] || die "the SSH host key is not a regular file: $key"
          key_owner="$(owner_of "$key")"
          (( key_owner == 0 )) || die "the SSH host key must be root-owned (is uid $key_owner): $key"
          key_mode="$(mode_of "$key")"
          (( (8#"$key_mode" & 8#077) == 0 )) \
              || die "the SSH host key is readable beyond root (mode $key_mode): $key"
      fi

      printf '%s: slot %s: session tree ownership/modes verified\n' "$PROG" "$slot" >&2
    '';
    meta = with lib; {
      description = "Verify the ownership and modes of a myconfig.ai.microvm session tree before launch";
      platforms = platforms.linux;
    };
  };

  # ---- guest-side NixOS module fragment --------------------------------
  guestModule = {
    # `/workspace` is where `agent-run` (findmnt + writability check) and
    # every agent expect the clone. With the consolidated share it lives
    # INSIDE the session mount, so bind it to the historical path rather than
    # teaching every consumer a new one. `x-systemd.requires-mounts-for`
    # orders it after the virtiofs session mount.
    fileSystems.${paths.guestWorkspace} = {
      device = paths.guestWorkspaceSource;
      fsType = "none";
      options = [
        "bind"
        "x-systemd.requires-mounts-for=${paths.guestMountPoint}"
      ];
    };
  }
  // lib.optionalAttrs cfg.enableSsh {
    # The TCP sshd (`sshd.service`, the `interactive` capability) reads its host
    # key from the read-only share; make that ordering explicit instead of
    # relying on local-fs.target having completed. Scoped to `enableSsh` (the
    # ONLY host shape whose TCP sshd is live): a batch+vsock host MASKS the
    # `sshd.service` (guest.nix), so a `RequiresMountsFor` on that dead unit
    # would be a no-op — the live control channel there is the VSOCK
    # `sshd-vsock@`, whose ordering is handled by the socket directive below.
    systemd.services.sshd.unitConfig.RequiresMountsFor = paths.guestHostkeysDir;
  }
  // lib.optionalAttrs agentCapabilities.vsock {
    # The VSOCK sshd (`sshd-vsock@`, the `vsock` capability) reads the SAME
    # per-slot host key from the read-only share, so its activation socket must
    # be ordered against that mount too. nixpkgs defines `systemd.sockets.sshd-
    # vsock` with `overrideStrategy = "asDropin"` (a dropin on the generator-
    # created socket), so this adds `RequiresMountsFor=` to that dropin: the
    # socket (wantedBy `sockets.target`, started early) waits for the host-key
    # mount before it listens, eliminating the boot race in which the VSOCK
    # sshd accepts before the read-only share is up. Gated on `vsock` (absent on
    # a default host), so a default host's guest closure is byte-for-byte
    # unchanged; the `sshd-vsock` socket attrset itself is nixpkgs' dropin and
    # is left as-is when `vsock` is not selected.
    systemd.sockets.sshd-vsock.unitConfig.RequiresMountsFor = paths.guestHostkeysDir;
  };
in
{
  config = lib.mkMerge [
    # Path/layout definitions + the generated verifier and guest fragment,
    # exported for guest.nix, job.nix, state.nix, config-seed.nix, hostkeys.nix
    # and launcher.nix.
    {
      _module.args.agentSession = paths // {
        inherit guestModule verifier tmpfilesRules;
      };
    }

    (lib.mkIf cfg.enable {
      assertions = [
        {
          # The layout table IS the trust boundary; a weakening edit must fail
          # the build rather than produce a guest that can forge its own job
          # result or read the operator's staged configuration.
          # Applied to the FULL tables, so a weakening edit to an entry this
          # host happens not to select still fails the build.
          assertion =
            violationsOf {
              writableRoot = root;
              readOnlyRoot = roRoot;
              writable = fullLayout;
              readOnly = fullRoLayout;
              hostKeyDir = paths.hostHostkeysDir (lib.head slots).name;
              configSeedDir = paths.hostConfigSeedDir (lib.head slots).name;
            } == [ ];
          message = ''
            myconfig.ai.microvm.session: the session layout violates the trust
            boundary:
            ${lib.concatStringsSep "\n" (
              map (v: "  - ${v}") (violationsOf {
                writableRoot = root;
                readOnlyRoot = roRoot;
                writable = fullLayout;
                readOnly = fullRoLayout;
                hostKeyDir = paths.hostHostkeysDir (lib.head slots).name;
                configSeedDir = paths.hostConfigSeedDir (lib.head slots).name;
              })
            )}
          '';
        }
        {
          # Table hygiene (see `malformedCapabilityEntries`): an empty capability
          # list or an undeclared token would silently remove a directory (and
          # its trust-relevant owner/mode) from EVERY host.
          assertion = malformedCapabilityEntries == [ ];
          message = ''
            myconfig.ai.microvm.session: malformed `capabilities` in the layout
            table (use `null` for "every host", or a non-empty list of declared
            capabilities — ${lib.concatStringsSep ", " agentCapabilities.declared}):
            ${lib.concatStringsSep "\n" (
              map (
                e: "  - entry '${e.rel}': [ ${lib.concatStringsSep " " e.capabilities} ]"
              ) malformedCapabilityEntries
            )}
          '';
        }
      ];

      # The generated verifier, so an operator (and the real-KVM validation
      # suite) can check a slot's session tree by hand with exactly the policy
      # the launcher enforces.
      environment.systemPackages = [ verifier ];

      systemd.tmpfiles.rules = tmpfilesRules;
    })
  ];
}
