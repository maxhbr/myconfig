# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.microvm — RUNTIME, ALLOWLISTED AGENT-CONFIGURATION STAGING
# (lightweight plan phase 3), the replacement for guest Home Manager activation
# in the `lite` profile.
#
# Problem
# -------
# `../guest-home.nix` runs home-manager INSIDE the guest for the `agent` user
# and bakes the host primary user's rendered dotfiles into the guest closure.
# That works, but it means (a) every instruction/skill/config edit needs a guest
# REBUILD before a sandbox sees it, and (b) the guest carries the whole
# home-manager activation machinery.
#
# Design (plan phase 3)
# ---------------------
#   host allowlisted config
#           │ copy with symlink dereferencing, at LAUNCH time (../launcher.nix)
#           ▼
#   ${runtimeRoot}/config-seed/<slot>/home        root:root 0500/0400
#           │ per-slot, READ-ONLY virtiofs share (../guest.nix), whose source
#           │ is exactly that `home` PAYLOAD directory — the sibling manifest
#           │ stays host-side, outside the share
#           ▼
#   /run/agent-config-seed
#           │ root-owned guest oneshot, BEFORE sshd and the job controller
#           ▼
#   /home/agent                                   agent-owned, DISPOSABLE
#
# There is NO live host-home mount anywhere in that chain: only the paths of
# the ALLOWLIST below (per-agent `configPaths` from the authoritative registry
# ../agents.nix, following `enabledAgents`, plus the module-wide
# `configSeed.extraPaths`) are ever copied, and the copy is a fresh, cleaned
# per-slot directory owned by root.
#
# Security properties (invariants 1, 4, 7 and 8 of the plan)
# ----------------------------------------------------------
#   * ALLOWLIST ONLY — exact files and exact directories, never "$HOME",
#     "~/.config" or a whole agent config root. `.codex/config.toml` is staged;
#     `.codex/` as a whole is NOT (it would carry `auth.json`).
#   * A CREDENTIAL DENYLIST (auth.json, credentials*, *.pem, *.key, id_rsa,
#     id_ed25519, .env, .netrc, cookies*, *session*, …) is applied as defence
#     in depth to every path component — at EVALUATION time to the allowlist
#     itself (so a bad registry/`extraPaths` entry fails the build) AND at
#     runtime to every file inside an allowlisted DIRECTORY *and to the
#     RESOLVED target of every entry/file*, so a benignly NAMED symlink
#     (`.codex/config.toml` -> `.codex/auth.json`, `.agents/skills/x` -> `~/.ssh`)
#     cannot smuggle credential material past a name-only check.
#   * ESCAPES ARE REJECTED — an entry with `..`, an absolute path or a symlink
#     resolving outside the configured host home is refused. The only exception
#     is `/nix/store`, which is where home-manager renders every dotfile: such
#     symlinks are DEREFERENCED (the guest gets a plain copy, never a link into
#     a store it does not have).
#   * NO SOCKETS/DEVICES/FIFOS/SETUID — only regular files and directories are
#     copied, and setuid/setgid files are skipped.
#   * ROOT-OWNED, ROOT-ONLY — the staged tree is root:root 0500/0400, so the
#     untrusted guest `agent` user can neither modify nor even read what the
#     host staged directly (invariant 7); it only ever sees the COPY the guest
#     root seeder hands it. The same modes keep other UNPRIVILEGED HOST users
#     out of the operator's staged configuration while it sits under the
#     persistent `runtimeRoot`. The share is additionally mounted READ-ONLY.
#   * CLEANED BEFORE EVERY LAUNCH — the per-slot destination is removed and
#     recreated by the stager, so nothing from a previous task can leak into
#     the next one.
#   * A MANIFEST (`manifest.json`, root:root 0400) records the policy plus what
#     was staged and what was skipped and why, so an operator can audit a
#     session without guessing. It lives NEXT TO the payload directory, not
#     inside it: the share source is the payload, so the manifest — which
#     names the host home and every skipped credential-SHAPED host file name —
#     is never visible to the untrusted guest.
#   * MODEL CREDENTIALS ARE NEVER STAGED — the upstream key lives only in the
#     host LiteLLM proxy; the guest gets the endpoint through
#     `environment.variables` (../guest.nix) and the boot-time model discovery
#     (../guest-model-config.nix).
#
# Residual risks (deliberately NOT claimed to be handled)
# -------------------------------------------------------
# Everything above assumes the host home is TRUSTED. An attacker who already
# has write access INSIDE it can still defeat the name-based controls:
#   * TOCTOU — `realpath -e` and the subsequent `install` are two syscall
#     sequences; the resolved path can be swapped for a symlink in between and
#     root `install` would follow it.
#   * HARDLINKS — a hardlink has no target name, so `x/config.toml` hardlinked
#     to `auth.json` is indistinguishable from a real config file.
# Both are out of scope: an attacker with write access to the trusted home can
# simply edit an allowlisted file instead. They are documented here and in
# docs/agent-microvm-security-model.md so nobody mistakes the denylist for a
# boundary against a compromised host home.
#
# Profile boundary
# ----------------
# `configSeed.enable` defaults to the resolved profile's `configSeed` field
# (../profiles.nix): FALSE for `full` (which keeps guest home-manager
# activation, byte-for-byte) and TRUE for `lite`. The two mechanisms are
# mutually exclusive and an assertion rejects enabling both, so there is
# exactly one provisioning path per guest.
{
  config,
  lib,
  pkgs,
  myconfig,
  # The ONE authoritative supported-agent registry instance (../agents.nix, via
  # default.nix). Its `configPaths` union already follows `enabledAgents`, so
  # the staged set shrinks with the agent selection.
  agentRegistry,
  # The ONE resolved profile entry (../profiles.nix), which decides whether this
  # mechanism or guest home-manager activation provisions the guest home.
  agentProfile,
  # The effective resource-class table (see default.nix): the slot pool whose
  # per-slot share sources must exist before any VM starts.
  agentResourceClasses,
  # The ONE definition of the batch-job units (job.nix), so the guest seeding
  # oneshot can be ordered before the TRUSTED job controller by NAME from a
  # single source rather than a second hardcoded string.
  agentJobs,
  # The ONE definition of the task-scoped agent-state paths (state.nix). Its
  # `declaredDirs` must stay DISJOINT from what is staged here, otherwise the
  # seeding copy and the state linker would fight over the same directory.
  agentState,
  # The ONE definition of the CONSOLIDATED per-session tree (./session.nix,
  # lightweight plan phase 4). With it, the staged payload is the `config-seed/`
  # subdirectory of the ONE READ-ONLY share — still root-owned, still mounted
  # read-only, and still with the MANIFEST outside every share (it moves to a
  # host-only directory under `<runtimeRoot>/config-seed/<slot>/`, because the
  # read-only share source is the slot directory itself there).
  agentSession,
  ...
}:
let
  cfg = config.myconfig.ai.microvm;
  seedCfg = cfg.configSeed;
  session = agentSession;

  slots = (import ./slots.nix { inherit lib; }).mkSlots agentResourceClasses;

  # --- the credential DENYLIST (defence in depth) -------------------------
  # The primary control is the positive allowlist; this list exists so that a
  # careless allowlist entry — or a credential file that appears INSIDE an
  # allowlisted directory later on — is still refused. Matched
  # case-insensitively against every PATH COMPONENT, by the eval assertions
  # below and by the runtime stager (which renders these very lists).
  denyNames = [
    ".aws"
    ".env"
    ".git-credentials"
    ".gnupg"
    ".kube"
    ".netrc"
    ".npmrc"
    ".ssh"
    "auth.json"
    "credentials"
    "credentials.json"
    "id_ecdsa"
    "id_ed25519"
    "id_rsa"
    "token.json"
    "tokens.json"
  ];
  denySuffixes = [
    ".gpg"
    ".kdbx"
    ".key"
    ".p12"
    ".pem"
    ".pfx"
  ];
  denyInfixes = [
    "cookies"
    "credential"
    "password"
    "secret"
    "session"
    "token"
  ];

  lower = lib.toLower;
  componentDenied =
    c:
    let
      lc = lower c;
    in
    lib.elem lc denyNames
    || lib.any (s: lib.hasSuffix s lc) denySuffixes
    || lib.any (i: lib.hasInfix i lc) denyInfixes;
  pathDenied = p: lib.any componentDenied (lib.splitString "/" p);

  # A staged entry must be a plain, relative, `..`-free path in a conservative
  # character set: it is rendered into a shell array and joined onto both the
  # host home and the guest home, so no metacharacter, no leading `-`, no `~`
  # and no trailing slash are accepted.
  pathWellFormed =
    p:
    lib.isString p
    && p != ""
    && !lib.hasPrefix "/" p
    && !lib.hasPrefix "-" p
    && !lib.hasSuffix "/" p
    && !lib.hasInfix ".." p
    && builtins.match "[A-Za-z0-9._][-A-Za-z0-9._/]*" p != null;

  # The EFFECTIVE allowlist: the union of the SELECTED agents' declared
  # `configPaths` (so it follows `enabledAgents`) plus the module-wide
  # `extraPaths`. Sorted + deduplicated so the generated stager is stable.
  allowedPaths = lib.unique (
    lib.sort (a: b: a < b) (agentRegistry.configPaths ++ seedCfg.extraPaths)
  );

  malformedPaths = lib.filter (p: !pathWellFormed p) allowedPaths;
  deniedPaths = lib.filter (p: pathWellFormed p && pathDenied p) allowedPaths;

  # Staged configuration and PERSISTED agent state must be disjoint: the
  # seeding oneshot runs before the state linker, which refuses to replace a
  # non-empty directory (../state.nix), so an overlap would silently disable
  # persistence for that directory. Prefix-wise in both directions, because
  # nesting is just as bad as equality.
  stateCollisions = lib.filter (
    p: lib.any (d: p == d || lib.hasPrefix "${d}/" p || lib.hasPrefix "${p}/" d) agentState.declaredDirs
  ) allowedPaths;

  # --- the ONE definition of every config-seed path / mode ----------------
  paths = rec {
    # Whether this mechanism is active at all (consumed by guest.nix, which
    # drops guest home-manager activation exactly then, and by launcher.nix,
    # which only renders its staging code then).
    enable = cfg.enable && seedCfg.enable;

    # ---- host side ----------------------------------------------------
    # The PAYLOAD root: the read-only session tree under the consolidated
    # layout (phase 4), its own root otherwise.
    root = if session.enable then session.roRoot else "${cfg.runtimeRoot}/config-seed";
    slotDir = slotName: "${root}/${slotName}";
    # The PAYLOAD lives in its own subdirectory. In the four-share layout that
    # subdirectory — NOT the slot directory — is the share source (see
    # `shareSource`); in the consolidated layout the read-only share source is
    # the slot directory of the READ-ONLY tree, which the payload is one level
    # below. Either way the MANIFEST lives outside every share: it names the
    # host home and every skipped credential-SHAPED host file name, which the
    # untrusted side has no business learning.
    homeSubdir = if session.enable then session.roSubdirs.configSeed else "home";
    manifestName = "manifest.json";
    hostPayloadDir = slotName: "${slotDir slotName}/${homeSubdir}";
    # Host-ONLY location of the manifest. Under the consolidated layout the
    # payload's sibling is inside the read-only SHARE, so the manifest moves to
    # a directory that is not shared with any guest at all.
    manifestRoot = if session.enable then "${cfg.runtimeRoot}/config-seed" else root;
    manifestDir = slotName: "${manifestRoot}/${slotName}";
    hostManifest = slotName: "${manifestDir slotName}/${manifestName}";
    # What ../guest.nix hands to virtiofsd (only used in the four-share
    # layout; the consolidated one exports the read-only slot directory).
    shareSource = hostPayloadDir;

    # ---- permission facts (host stager + guest seeder agree) -----------
    # virtiofsd passes ownership through unchanged, so these ARE the effective
    # permissions inside the guest: root-owned and NOT writable by the
    # unprivileged `agent` user (invariant 7).
    # ROOT-ONLY (0500/0400) rather than world-readable: virtiofsd runs as root
    # on the host and the guest seeder runs as root in the guest, so nothing
    # unprivileged — on either side — needs to read the staged tree, and the
    # operator's configuration is not exposed to other local host users while
    # it sits under the persistent `runtimeRoot`.
    # Under the consolidated layout the modes of the tree's own directories
    # come from the ONE session layout table (./session.nix), so there is a
    # single place that decides them; they are the same root-only values.
    rootMode = if session.enable then session.roRootMode else "0700";
    slotDirMode = if session.enable then session.roModeOf "" else "0500";
    dirMode = if session.enable then session.roModeOf session.roSubdirs.configSeed else "0500";
    fileMode = "0400";
    manifestMode = "0400";
    # Modes of the manifest's own (host-only) directories. In the four-share
    # layout they ARE the payload root and the payload's slot directory, so
    # they must keep exactly those modes; in the consolidated layout they are
    # separate, root-only directories outside every share.
    manifestRootMode = if session.enable then "0700" else rootMode;
    manifestDirMode = if session.enable then "0700" else slotDirMode;
    # Bounds on what a single launch may copy, so a huge (or maliciously
    # grown) allowlisted directory cannot fill the host runtime root or stall
    # a launch. Over-budget entries are SKIPPED and recorded in the manifest.
    maxFileBytes = 1048576;
    maxTotalBytes = 33554432;
    # Bound on the NUMBER of files one launch may stage. Two `jq` forks per
    # staged file is the stager's dominant cost, so an accidentally huge
    # allowlisted tree would otherwise add seconds to every launch. Files past
    # the budget are SKIPPED and recorded in the manifest.
    maxFiles = 1024;
    # Depth bound for walking an allowlisted directory (a symlink loop cannot
    # turn the copy into an unbounded walk).
    maxDepth = 12;

    # ---- guest side (identical for every slot — the share hides the slot) --
    guestTag = "configseed";
    guestMountPoint = if session.enable then session.guestConfigSeedDir else "/run/agent-config-seed";
    # The share source IS the payload directory, so the guest sees the staged
    # home directly at the mount point (and the manifest not at all).
    guestPayloadDir = guestMountPoint;
    guestHome = "/home/agent";
    guestUser = "agent";
    # The guest `agent` user is an `isNormalUser`, so its primary group is
    # `users` (same reasoning as job.nix's `workerGroup`).
    guestGroup = "users";
    guestUnit = "agent-config-seed.service";

    # The policy itself, exported so tests/docs read the SAME data the
    # generated scripts are rendered from.
    inherit
      allowedPaths
      denyNames
      denySuffixes
      denyInfixes
      ;
    hostHome = seedCfg.hostHome;
  };

  bashArray = xs: lib.concatMapStringsSep " " lib.escapeShellArg xs;

  # --- host-side stager --------------------------------------------------
  # Invoked by the launcher (as root) once per launch, with the SLOT name as
  # its only argument. Everything else — the host home, the allowlist, the
  # denylist, the modes and the budgets — is BAKED IN by Nix, so the launcher
  # cannot widen the policy and no caller-supplied path is ever expanded.
  stager = pkgs.writeShellApplication {
    name = "agent-microvm-stage-config";
    runtimeInputs = with pkgs; [
      coreutils
      findutils
      jq
    ];
    text = ''
      set -euo pipefail

      # ---- baked policy (never taken from the caller) --------------------
      readonly HOST_HOME=${lib.escapeShellArg paths.hostHome}
      readonly SEED_ROOT=${lib.escapeShellArg paths.root}
      readonly PAYLOAD_SUBDIR=${lib.escapeShellArg paths.homeSubdir}
      # The manifest lives OUTSIDE every guest share (see the header): its own
      # host-only root under the consolidated layout, the payload's sibling in
      # the four-share one.
      readonly MANIFEST_ROOT=${lib.escapeShellArg paths.manifestRoot}
      readonly MANIFEST_NAME=${lib.escapeShellArg paths.manifestName}
      readonly MANIFEST_ROOT_MODE=${lib.escapeShellArg paths.manifestRootMode}
      readonly MANIFEST_DIR_MODE=${lib.escapeShellArg paths.manifestDirMode}
      readonly ROOT_MODE=${lib.escapeShellArg paths.rootMode}
      readonly SLOT_DIR_MODE=${lib.escapeShellArg paths.slotDirMode}
      readonly DIR_MODE=${lib.escapeShellArg paths.dirMode}
      readonly FILE_MODE=${lib.escapeShellArg paths.fileMode}
      readonly MANIFEST_MODE=${lib.escapeShellArg paths.manifestMode}
      readonly MAX_FILE_BYTES=${toString paths.maxFileBytes}
      readonly MAX_TOTAL_BYTES=${toString paths.maxTotalBytes}
      readonly MAX_FILES=${toString paths.maxFiles}
      readonly MAX_DEPTH=${toString paths.maxDepth}
      # The ONLY paths that may ever cross the boundary (allowlist, generated
      # from the SELECTED agents' registry `configPaths` + `extraPaths`).
      readonly ALLOWLIST=(${bashArray paths.allowedPaths})
      # Defence in depth (see the module header): applied to every path
      # component, case-insensitively.
      readonly DENY_NAMES=(${bashArray paths.denyNames})
      readonly DENY_SUFFIXES=(${bashArray paths.denySuffixes})
      readonly DENY_INFIXES=(${bashArray paths.denyInfixes})
      readonly SLOTS=(${bashArray (map (s: s.name) slots)})

      PROG="agent-microvm-stage-config"
      die() { printf '%s: error: %s\n' "$PROG" "$*" >&2; exit 1; }
      log() { printf '%s: %s\n' "$PROG" "$*" >&2; }

      [[ $# -eq 1 ]] || die "usage: $PROG <slot>"
      slot="$1"
      # The destination is derived from a slot name of the PREBUILT pool, never
      # from a caller-supplied path, so this command cannot be pointed at an
      # arbitrary directory.
      slot_known=0
      for known in "''${SLOTS[@]}"; do
          if [[ "$known" == "$slot" ]]; then
              slot_known=1
          fi
      done
      (( slot_known )) || die "unknown slot '$slot'"

      readonly SLOT_DIR="$SEED_ROOT/$slot"
      readonly PAYLOAD="$SLOT_DIR/$PAYLOAD_SUBDIR"
      readonly MANIFEST_DIR="$MANIFEST_ROOT/$slot"
      readonly MANIFEST="$MANIFEST_DIR/$MANIFEST_NAME"

      # ---- denylist ------------------------------------------------------
      path_is_denied() {
          local path="$1" comp lc pat
          local -a comps=()
          local IFS=/
          # `read -ra` splits on IFS WITHOUT globbing, so a file name
          # containing shell metacharacters cannot expand here.
          read -ra comps <<< "$path"
          for comp in ''${comps[@]+"''${comps[@]}"}; do
              [[ -n "$comp" ]] || continue
              lc="''${comp,,}"
              for pat in "''${DENY_NAMES[@]}"; do
                  if [[ "$lc" == "$pat" ]]; then
                      return 0
                  fi
              done
              for pat in "''${DENY_SUFFIXES[@]}"; do
                  if [[ "$lc" == *"$pat" ]]; then
                      return 0
                  fi
              done
              for pat in "''${DENY_INFIXES[@]}"; do
                  if [[ "$lc" == *"$pat"* ]]; then
                      return 0
                  fi
              done
          done
          return 1
      }

      # ---- manifest accumulation ----------------------------------------
      staged_ndjson="$(mktemp)"
      skipped_ndjson="$(mktemp)"
      trap 'rm -f -- "$staged_ndjson" "$skipped_ndjson"' EXIT
      total_bytes=0
      total_files=0
      note_staged() {
          jq -nc --arg path "$1" --arg kind "$2" --argjson bytes "$3" \
              '{path:$path, kind:$kind, bytes:$bytes}' >> "$staged_ndjson"
      }
      note_staged_dir() {
          jq -nc --arg path "$1" --argjson files "$2" \
              '{path:$path, kind:"directory", files:$files}' >> "$staged_ndjson"
      }
      note_skipped() {
          jq -nc --arg path "$1" --arg reason "$2" \
              '{path:$path, reason:$reason}' >> "$skipped_ndjson"
          log "skipped $1: $2"
      }

      # ---- the destination is CLEANED before every launch ----------------
      # Nothing a previous task staged (or a previous generation's allowlist
      # allowed) may survive into this launch.
      install -d -m "$ROOT_MODE" -o root -g root -- "$SEED_ROOT"
      install -d -m "$SLOT_DIR_MODE" -o root -g root -- "$SLOT_DIR"
      install -d -m "$MANIFEST_ROOT_MODE" -o root -g root -- "$MANIFEST_ROOT"
      install -d -m "$MANIFEST_DIR_MODE" -o root -g root -- "$MANIFEST_DIR"
      rm -rf -- "$PAYLOAD" "$MANIFEST"
      install -d -m "$DIR_MODE" -o root -g root -- "$PAYLOAD"

      home_real="$(realpath -e -- "$HOST_HOME")" \
          || die "the configured host home does not exist: $HOST_HOME"
      [[ -d "$home_real" ]] || die "the configured host home is not a directory: $home_real"

      # A resolved path may only live INSIDE the configured host home, or in
      # /nix/store (where home-manager renders the dotfiles the host symlinks
      # into its home — those links are DEREFERENCED into plain copies).
      resolved_is_allowed() {
          case "$1" in
              "$home_real"/*) return 0 ;;
              /nix/store/*) return 0 ;;
              *) return 1 ;;
          esac
      }

      # The denylist must also be applied to the RESOLVED TARGET, not only to
      # the name a path is reached under. Without this, one benignly NAMED
      # symlink in the host home defeats the whole control:
      #   .codex/config.toml    -> .codex/auth.json   (a credential staged as
      #                                                 "config.toml")
      #   .agents/skills/notes  -> ~/.ssh             (every file under it
      #                                                 judged by its LINK-
      #                                                 relative name)
      # Both resolve INSIDE the host home, so `resolved_is_allowed` passes and
      # only a check on the real name can stop them.
      resolved_is_denied() {
          local real="$1" probe
          case "$real" in
              "$home_real"/*) probe="''${real#"$home_real"/}" ;;
              # A store path's own top-level name is a mangled, hash-prefixed
              # derivation name (home-manager renders `~/.config/git/config`
              # as `…-hm_.config-git-config`), so judging IT would produce
              # false positives; the components BELOW it are real file names
              # and are checked. Store content is world-readable anyway.
              /nix/store/*/*) probe="''${real#/nix/store/*/}" ;;
              /nix/store/*) probe="" ;;
              # Anything else never gets here (resolved_is_allowed rejects it
              # first); fail CLOSED regardless.
              *) return 0 ;;
          esac
          [[ -n "$probe" ]] || return 1
          path_is_denied "$probe"
      }

      # ---- one regular file ---------------------------------------------
      # Returns 0 only when the file was actually STAGED, so callers can count
      # what landed rather than what was attempted.
      stage_file() {
          local rel="$1" src="$2" real size
          real="$(realpath -e -- "$src" 2>/dev/null)" || {
              note_skipped "$rel" "rejected: unresolvable path"
              return 1
          }
          resolved_is_allowed "$real" || {
              note_skipped "$rel" "rejected: resolves outside the host home ($real)"
              return 1
          }
          # The denylist on the RESOLVED name, not just on `$rel` (see
          # resolved_is_denied): a benignly named link must not stage a
          # credential.
          if resolved_is_denied "$real"; then
              note_skipped "$rel" "rejected: resolves onto a credential-shaped path"
              return 1
          fi
          # `-f` after symlink resolution: sockets, FIFOs and device nodes can
          # never reach the guest.
          [[ -f "$real" ]] || {
              note_skipped "$rel" "rejected: not a regular file"
              return 1
          }
          if [[ -u "$real" || -g "$real" ]]; then
              note_skipped "$rel" "rejected: setuid/setgid file"
              return 1
          fi
          size="$(stat -Lc %s -- "$real")"
          if (( size > MAX_FILE_BYTES )); then
              note_skipped "$rel" "rejected: larger than $MAX_FILE_BYTES bytes ($size)"
              return 1
          fi
          if (( total_bytes + size > MAX_TOTAL_BYTES )); then
              note_skipped "$rel" "rejected: the ''${MAX_TOTAL_BYTES}-byte staging budget is exhausted"
              return 1
          fi
          if (( total_files >= MAX_FILES )); then
              note_skipped "$rel" "rejected: the ''${MAX_FILES}-file staging budget is exhausted"
              return 1
          fi
          install -d -m "$DIR_MODE" -o root -g root -- "$(dirname -- "$PAYLOAD/$rel")"
          # Root-owned and NOT writable by the guest agent (invariant 7); the
          # copy dereferences, so no store symlink reaches the guest.
          install -m "$FILE_MODE" -o root -g root -- "$real" "$PAYLOAD/$rel"
          total_bytes=$(( total_bytes + size ))
          total_files=$(( total_files + 1 ))
          note_staged "$rel" file "$size"
      }

      # ---- one allowlisted directory ------------------------------------
      # `find -L` FOLLOWS symlinks, so a store-symlinked tree (the usual
      # home-manager shape) is dereferenced and only real files/directories are
      # considered — a dangling link, socket, FIFO or device is not of type
      # f/d and is therefore never even offered. Every file found is still put
      # through stage_file's own escape/type/denylist checks — on BOTH its
      # link-relative name and its RESOLVED target — because a symlink INSIDE
      # the tree could point anywhere (`skills/notes -> ~/.ssh`).
      stage_dir() {
          local rel="$1" real="$2" f sub real_sub count=0
          install -d -m "$DIR_MODE" -o root -g root -- "$PAYLOAD/$rel"
          while IFS= read -r -d "" f; do
              sub="''${f#"$real"}"
              sub="''${sub#/}"
              [[ -n "$sub" ]] || continue
              if path_is_denied "$sub"; then
                  note_skipped "$rel/$sub" "rejected: matches the credential denylist"
                  continue
              fi
              if [[ -d "$f" ]]; then
                  if ! real_sub="$(realpath -e -- "$f" 2>/dev/null)" \
                      || ! resolved_is_allowed "$real_sub"; then
                      note_skipped "$rel/$sub" "rejected: directory resolves outside the host home"
                      continue
                  fi
                  # Same reasoning as in stage_file: a subdirectory reached
                  # under a benign NAME may still BE `~/.ssh`, `~/.gnupg`, …
                  if resolved_is_denied "$real_sub"; then
                      note_skipped "$rel/$sub" "rejected: resolves onto a credential-shaped path"
                      continue
                  fi
                  install -d -m "$DIR_MODE" -o root -g root -- "$PAYLOAD/$rel/$sub"
                  continue
              fi
              # Count what was STAGED, not what was attempted, so the manifest
              # is not inflated by skipped files.
              if stage_file "$rel/$sub" "$f"; then
                  count=$(( count + 1 ))
              fi
          done < <(find -L "$real" -mindepth 1 -maxdepth "$MAX_DEPTH" \
                       \( -type d -o -type f \) -print0 2>/dev/null | sort -z)
          # `-maxdepth` truncation must never be SILENT: an operator reading
          # the manifest has to see that part of the tree was not considered.
          if [[ -n "$(find -L "$real" -mindepth "$(( MAX_DEPTH + 1 ))" \
                          \( -type d -o -type f \) -print -quit 2>/dev/null)" ]]; then
              note_skipped "$rel" "truncated: content deeper than $MAX_DEPTH levels was not considered"
          fi
          note_staged_dir "$rel" "$count"
      }

      # ---- one allowlist entry ------------------------------------------
      stage_entry() {
          local rel="$1" src real
          # The allowlist is baked in and eval-validated; re-check anyway, so
          # this script is safe on its own terms.
          case "$rel" in
              "" | /* | -* | *..* | */)
                  note_skipped "$rel" "rejected: not a plain relative path"
                  return 0
                  ;;
          esac
          if path_is_denied "$rel"; then
              note_skipped "$rel" "rejected: matches the credential denylist"
              return 0
          fi
          src="$HOST_HOME/$rel"
          # A MISSING optional path is normal (an agent may not be configured
          # on this host at all), not an error.
          if ! real="$(realpath -e -- "$src" 2>/dev/null)"; then
              note_skipped "$rel" "absent on the host"
              return 0
          fi
          resolved_is_allowed "$real" || {
              note_skipped "$rel" "rejected: resolves outside the host home ($real)"
              return 0
          }
          # `$rel` passing the denylist says nothing about WHAT it resolves to:
          # `.codex/config.toml -> .codex/auth.json` and
          # `.agents/skills -> ~/.ssh` both look innocent by name.
          if resolved_is_denied "$real"; then
              note_skipped "$rel" "rejected: resolves onto a credential-shaped path"
              return 0
          fi
          if [[ -d "$real" ]]; then
              stage_dir "$rel" "$real"
          elif [[ -f "$real" ]]; then
              stage_file "$rel" "$real" || true
          else
              note_skipped "$rel" "rejected: neither a regular file nor a directory"
          fi
      }

      for entry in ''${ALLOWLIST[@]+"''${ALLOWLIST[@]}"}; do
          stage_entry "$entry"
      done

      # ---- the manifest --------------------------------------------------
      # Records the POLICY plus exactly what was staged and what was skipped
      # (and why), so a session can be audited without re-deriving the rules.
      # It is written NEXT TO the payload directory (the share source is the
      # PAYLOAD), root-owned 0400, so it is readable by the host operator only
      # — never by the guest, which has no business learning the host home path
      # or which credential-shaped files exist next to the staged ones.
      manifest_tmp="$(mktemp "$MANIFEST_DIR/.manifest.XXXXXX")"
      allowlist_json="$(printf '%s\n' ''${ALLOWLIST[@]+"''${ALLOWLIST[@]}"} \
          | jq -R -s 'split("\n") | map(select(length > 0))')"
      jq -n \
          --argjson version 1 \
          --arg slot "$slot" \
          --arg hostHome "$HOST_HOME" \
          --arg stagedAt "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
          --argjson totalBytes "$total_bytes" \
          --argjson totalFiles "$total_files" \
          --argjson allowlist "$allowlist_json" \
          --slurpfile staged "$staged_ndjson" \
          --slurpfile skipped "$skipped_ndjson" \
          '{version:$version, slot:$slot, hostHome:$hostHome,
            stagedAt:$stagedAt, totalBytes:$totalBytes, totalFiles:$totalFiles,
            allowlist:$allowlist, staged:$staged, skipped:$skipped}' \
          > "$manifest_tmp" \
          || die "could not render the staging manifest"
      chown root:root -- "$manifest_tmp"
      chmod "$MANIFEST_MODE" -- "$manifest_tmp"
      mv -f -- "$manifest_tmp" "$MANIFEST"
      log "staged $(jq -s length "$staged_ndjson") allowlisted entr(y|ies) for slot $slot ($total_files file(s), $total_bytes bytes) -> $PAYLOAD"
    '';
    meta = with lib; {
      description = "Stage allowlisted host agent configuration for a myconfig.ai.microvm slot";
      platforms = platforms.linux;
    };
  };

  # --- guest-side seeder -------------------------------------------------
  # Root oneshot, ordered BEFORE sshd, the batch job controller and the
  # agent-state linker, so the disposable home is fully provisioned before any
  # agent process — interactive or batch — can exist.
  seeder = pkgs.writeShellApplication {
    name = "agent-config-seed-apply";
    runtimeInputs = with pkgs; [ coreutils ];
    text = ''
      set -euo pipefail

      readonly SEED=${lib.escapeShellArg paths.guestMountPoint}
      readonly PAYLOAD=${lib.escapeShellArg paths.guestPayloadDir}
      readonly HOME_DIR=${lib.escapeShellArg paths.guestHome}
      readonly OWNER=${lib.escapeShellArg paths.guestUser}
      readonly GROUP=${lib.escapeShellArg paths.guestGroup}

      log() { printf 'agent-config-seed: %s\n' "$*" >&2; }
      die() { printf 'agent-config-seed: error: %s\n' "$*" >&2; exit 1; }

      # The home is DISPOSABLE and recreated on every boot; make sure it exists
      # and belongs to the unprivileged agent before anything is copied in.
      install -d -m 0700 -o "$OWNER" -g "$GROUP" -- "$HOME_DIR"

      if [[ ! -d "$PAYLOAD" ]]; then
          log "no staged configuration at $PAYLOAD; keeping a bare home"
          exit 0
      fi

      # Defence in depth: the host stages this tree as root:root and mounts the
      # share read-only. If it is group/other-writable or not root-owned, the
      # trust assumption ("only the host decides what lands in the home") does
      # not hold, so fail CLOSED instead of copying attacker-controlled files.
      owner="$(stat -c %u -- "$SEED")"
      mode="$(stat -c %a -- "$SEED")"
      [[ "$owner" == "0" ]] || die "the config-seed share $SEED is not root-owned (uid $owner)"
      (( (8#"$mode" & 8#022) == 0 )) \
          || die "the config-seed share $SEED is group/other-writable (mode $mode)"

      # Plain copy, dereferencing anything left (the host already dereferenced
      # store symlinks), then hand the RESULT to the agent: the guest copy is
      # the agent's own, writable and disposable, while the staged original
      # stays root-owned and read-only.
      cp -R --dereference -- "$PAYLOAD/." "$HOME_DIR/"
      chown -R "$OWNER:$GROUP" -- "$HOME_DIR"
      chmod -R u+rwX,go= -- "$HOME_DIR"
      log "seeded $HOME_DIR from $PAYLOAD"
    '';
    meta = with lib; {
      description = "Copy the staged host agent configuration into the disposable guest home (myconfig.ai.microvm)";
      platforms = platforms.linux;
    };
  };

  # --- guest-side NixOS module fragment ----------------------------------
  # EMPTY unless staging is active, so a `full`-profile guest is unchanged.
  guestModule = lib.optionalAttrs paths.enable {
    systemd.services.agent-config-seed = {
      description = "Seed the disposable agent home from the staged host configuration";
      wantedBy = [ "multi-user.target" ];
      # Resolves to the generated .mount unit of the config-seed share, so the
      # seeding never runs against an unmounted (empty) directory.
      unitConfig.RequiresMountsFor = paths.guestMountPoint;
      after = [ "local-fs.target" ];
      # The home must be provisioned before ANY agent process can exist:
      #   * sshd    — the interactive session,
      #   * the batch job CONTROLLER — which is what starts the untrusted
      #     worker (a template unit cannot be ordered against),
      #   * the agent-state linker — it symlinks persisted directories into the
      #     home, and copying afterwards could write THROUGH such a symlink
      #     into the host-side task state.
      #   * the boot-time model discovery — it writes
      #     `$HOME/.pi/agent/extensions/zz-microvm-models.ts` INTO the same
      #     home this unit populates with `cp -R` + `chown -R` + `chmod -R`.
      #     Under `full` that ordering came from `home-manager-agent.service`;
      #     under `lite` that unit does not exist, so it must be stated here
      #     (../guest-model-config.nix orders itself after THIS unit instead).
      before = [
        "sshd.service"
        agentJobs.controllerUnit
        "agent-state-link.service"
        "agent-model-config.service"
      ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = lib.getExe seeder;
        NoNewPrivileges = true;
        PrivateTmp = true;
        PrivateDevices = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        RestrictSUIDSGID = true;
        # It writes the agent's home, so ProtectHome must stay off; the staged
        # source is read-only for everyone but the host.
        ProtectHome = false;
        ReadOnlyPaths = [ "-${paths.guestMountPoint}" ];
        # Guest journal dies with the ephemeral guest; the console is captured
        # by `microvm@<slot>.service` on the host (same reasoning as job.nix).
        StandardOutput = "journal+console";
        StandardError = "journal+console";
      };
    };
  };
in
{
  options.myconfig.ai.microvm.configSeed = with lib; {
    enable = mkOption {
      type = types.bool;
      default = agentProfile.configSeed;
      defaultText = literalExpression "the resolved profile's `configSeed` field (false for `full`, true for `lite`)";
      description = ''
        Provision the guest `agent` home at LAUNCH time from an explicit
        ALLOWLIST of host configuration paths (see the per-agent `configPaths`
        of the registry in ./agents.nix plus `configSeed.extraPaths`) instead of
        running home-manager inside the guest (`guestDotfiles`).

        The host launcher copies the allowlisted paths into a cleaned, per-slot
        staging directory under
        `<runtimeRoot>/config-seed/<slot>/`, the guest sees that directory
        through a READ-ONLY virtiofs share, and a root-owned guest oneshot
        copies it into the disposable `/home/agent` before sshd and before the
        batch job controller start.

        Consequences: editing an allowlisted host file affects the NEXT launch
        without rebuilding the guest, the guest no longer runs home-manager
        activation, and the guest home stays disposable. There is NO live
        host-home mount: only the allowlisted paths cross the boundary, they
        are staged root-owned and non-writable by the guest agent, symlinks
        into `/nix/store` are dereferenced, paths that escape the host home are
        rejected, and sockets/devices/FIFOs/setuid files are never copied.

        Mutually exclusive with `guestDotfiles.enable` (there is exactly one
        provisioning path per guest); the default follows the `profile`.
      '';
    };

    hostHome = mkOption {
      type = types.str;
      default = config.users.users.${myconfig.user}.home;
      defaultText = literalExpression "config.users.users.<myconfig.user>.home";
      description = ''
        Absolute path of the host home the allowlisted configuration paths are
        resolved against. Every staged path must resolve INSIDE this directory
        (or, for a dereferenced dotfile symlink, in `/nix/store`); anything
        else is rejected. Only the allowlisted paths are read — this directory
        is NEVER mounted into a guest.
      '';
    };

    extraPaths = mkOption {
      type = types.listOf types.str;
      default = [
        ".config/git/attributes"
        ".config/git/config"
      ];
      example = literalExpression ''[ ".config/git/config" ".agents/skills" ]'';
      description = ''
        AGENT-INDEPENDENT additions to the staging allowlist, relative to
        `configSeed.hostHome`. Same rules as the registry's per-agent
        `configPaths`: exact files or exact directories, relative, `..`-free,
        and rejected at evaluation time when they match the credential
        denylist. Keep this a positive allowlist of known-safe paths — never a
        whole configuration root.
      '';
    };
  };

  config = lib.mkMerge [
    # Path/policy definitions + the generated scripts and guest fragment,
    # exported for guest.nix (share + unit) and launcher.nix (staging).
    {
      # NOTE: consumed by ../guest.nix (share + guest unit), ../launcher.nix
      # (per-launch staging) and ../guest-model-config.nix (ordering).
      _module.args.agentConfigSeed = paths // {
        inherit
          stager
          seeder
          guestModule
          ;
      };
    }

    (lib.mkIf cfg.enable {
      assertions = [
        {
          assertion = !seedCfg.enable || lib.hasPrefix "/" seedCfg.hostHome;
          message = "myconfig.ai.microvm.configSeed.hostHome must be an absolute path.";
        }
        {
          # A malformed entry would be joined onto both the host home and the
          # guest home; reject it at EVAL rather than skipping it at runtime.
          assertion = malformedPaths == [ ];
          message = ''
            myconfig.ai.microvm.configSeed: the staging allowlist contains
            path(s) that are not plain, relative, `..`-free paths:
            ${lib.concatStringsSep ", " (map (p: "'${p}'") malformedPaths)}.
            A staged path must be relative to `configSeed.hostHome` (so it can
            never escape the host home), must not contain `..`, must not start
            with `/` or `-`, must not end with `/` and may only use
            [A-Za-z0-9._-/].
          '';
        }
        {
          # Defence in depth for invariant 8: a credential-shaped allowlist
          # entry is a policy bug and must fail the build, not be silently
          # skipped by the runtime denylist.
          assertion = deniedPaths == [ ];
          message = ''
            myconfig.ai.microvm.configSeed: the staging allowlist contains
            path(s) that look like CREDENTIAL material and must never be staged
            into a guest: ${lib.concatStringsSep ", " (map (p: "'${p}'") deniedPaths)}.
            Model-provider credentials stay in the host LiteLLM proxy; stage
            only non-sensitive configuration (see the denylist in
            ./config-seed.nix).
          '';
        }
        {
          # Exactly ONE provisioning path per guest: either runtime staging or
          # guest home-manager activation, never both (they would fight over
          # the same files, and the whole point of the phase is to remove the
          # activation from the lite guest).
          assertion = !(seedCfg.enable && cfg.guestDotfiles.enable);
          message = ''
            myconfig.ai.microvm: `configSeed.enable` (runtime configuration
            staging) and `guestDotfiles.enable` (home-manager activation INSIDE
            the guest) are mutually exclusive — the lite profile replaces the
            latter with the former. Set `guestDotfiles.enable = false` (the
            default whenever staging is enabled) or turn the staging off.
          '';
        }
        {
          # The MIRROR of ../state.nix's guestDotfiles collision guard, which is
          # gated on `guestDotfiles.enable` and is therefore dead code exactly
          # when this mechanism provisions the home. The seeding oneshot runs
          # BEFORE `agent-state-link.service`, so an overlap would have the
          # linker refuse to replace the (now non-empty) staged directory and
          # persistence would silently not happen.
          assertion = !seedCfg.enable || stateCollisions == [ ];
          message = ''
            myconfig.ai.microvm.configSeed: the staging allowlist overlaps the
            persisted agent-state directories
            (${lib.concatStringsSep ", " (map (p: "'${p}'") stateCollisions)}).
            The seeding oneshot copies the staged tree into the guest home
            BEFORE the agent-state linker runs, so the linker would find a
            non-empty directory and refuse to link the persisted state —
            persistence would silently stop working. Keep the staged
            configuration paths disjoint from every agent's
            `persistentState.directories`.
          '';
        }
      ];

      # The generated stager, so an operator (and the real-KVM validation
      # suite, ../runtime-validation.sh section `seed`) can run and audit the
      # exact staging policy the launcher uses. It needs root to write the
      # root-owned tree, and it takes NO policy argument — only a slot name.
      environment.systemPackages = lib.mkIf seedCfg.enable [ stager ];

      # virtiofsd refuses to start when a share source is missing, so every
      # slot's seed directory (and its payload subdirectory) must exist before
      # any VM starts — including a slot that has never been launched. The
      # MODES are the trust boundary: root-owned, not writable by the guest
      # `agent` user (virtiofsd passes ownership through unchanged).
      # Under the consolidated layout (phase 4) the payload directories are part
      # of the READ-ONLY session tree, which ./session.nix creates from the ONE
      # layout table; only the host-only MANIFEST directories are added here.
      systemd.tmpfiles.rules = lib.mkIf seedCfg.enable (
        lib.optionals (!session.enable) (
          [ "d ${paths.root} ${paths.rootMode} root root - -" ]
          ++ lib.concatMap (slot: [
            "d ${paths.slotDir slot.name} ${paths.slotDirMode} root root - -"
            "d ${paths.hostPayloadDir slot.name} ${paths.dirMode} root root - -"
          ]) slots
        )
        ++ lib.optionals session.enable (
          [ "d ${paths.manifestRoot} ${paths.manifestRootMode} root root - -" ]
          ++ map (slot: "d ${paths.manifestDir slot.name} ${paths.manifestDirMode} root root - -") slots
        )
      );
    })
  ];
}
