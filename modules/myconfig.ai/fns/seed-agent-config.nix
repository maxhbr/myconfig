# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Shared host→guest agent-configuration SEEDER for the user-space qemu
# microVM sandbox tiers (`agent-qemu-pi`, `agent-qemu-herdr`).
#
# This is the `sandboxed-*` counterpart of the heavyweight
# `myconfig.ai.microvm` config-seed mechanism
# (../myconfig.ai.microvm/config-seed.nix): instead of a root-owned,
# read-only virtiofs share staged by a privileged host daemon and copied by a
# guest-root oneshot, the sandboxed-* wrappers run entirely in user space (no
# host rebuild, no root, no systemd unit). So the staging + transfer happens
# at LAUNCH time, over the already-established SSH channel, from a fresh,
# ephemeral, per-invocation staging directory under `$runtime_dir`:
#
#   host $HOME  ──copy (allowlist + denylist, dereferenced)──▶  $staging
#                                                             (host-side, ephemeral)
#   $staging    ──rsync over SSH──▶  /home/agent              (guest, disposable)
#
# Nothing is baked into the Nix store: the allowlist and denylist are, but the
# host home contents are read at runtime. Secrets therefore never touch a
# tracked file or a store path — exactly the same posture as the credential
# env forwarding (`SetEnv` over SSH).
#
# ── Security properties (mirrors config-seed.nix invariants 4, 7, 8) ────────
#   * ALLOWLIST ONLY — exact files and exact directories per agent, never
#     `$HOME` or a whole agent config root. `.pi/agent/extensions` is staged;
#     `.pi` as a whole is NOT (it carries `agent/auth.json`, `sessions/`, …).
#   * A CREDENTIAL DENYLIST (auth.json, credentials*, *.pem, *.key, id_rsa,
#     id_ed25519, .env, .netrc, cookies*, *session*, *token*, …) is applied as
#     defence in depth to every path component AND to the RESOLVED target of
#     every entry, so a benignly named symlink cannot smuggle a credential.
#     A small, explicit `denyOverrides` allowlist (trademark/name-collision
#     exceptions, e.g. the `trustedtokens-provider` pi extension whose
#     "TrustedTokens" trademark contains the deny infix "token") EXEMPTS a path
#     prefix from the INFIX checks ONLY — the exact-name and suffix checks
#     still apply, so a real `auth.json` or `*.pem` placed inside an
#     overridden directory is still refused at eval AND runtime.
#   * NO SOCKETS/DEVICES/FIFOS/SETUID — only regular files and directories are
#     copied; everything else is skipped.
#   * ESCAPES ARE REJECTED — an entry with `..`, an absolute path or a symlink
#     resolving outside `$HOME` (or `/nix/store`, where home-manager renders
#     dotfiles) is refused. Store symlinks are DEREFERENCED (the guest gets a
#     plain copy, not a link — it shares the host store read-only, but a copy
#     is robust against a missing store path and matches the gVisor seeder).
#   * CLEANED PER LAUNCH — the staging directory is freshly created and the
#     guest home is a tmpfs discarded on VM exit, so nothing from a previous
#     task leaks into the next.
#
# Model credentials are NEVER staged: the allowlist deliberately excludes
# every credential file (`.pi/agent/auth.json`, `.codex/auth.json`,
# `.claude/.credentials.json`, …). LLM keys keep flowing over the SSH session
# environment (`SetEnv`), exactly as before.
#
# ── The allowlist source of truth ──────────────────────────────────────────
# The per-agent `configPaths` below TRACK `../myconfig.ai.microvm/agents.nix`
# (the authoritative registry for the heavyweight tier), but they are NOT an
# exact mirror: there is no `herdr` entry here — tier-4 `agents.nix` stages
# `.config/herdr/config.toml`, tier 3 does not (a known divergence tracked
# in doc/TODOs/seed-herdr-config-tier3-agent-qemu-herdr.md). The lists are
# duplicated here only because `agents.nix` is instantiated inside the microvm
# module tree with coupling arguments (litellmPort, hermesModel, enabledNames)
# that make importing it standalone impractical. Keep the two in sync apart
# from that documented divergence, and preserve the SAME
# credential-exclusion rationale. A future cleanup should lift the path lists
# into a dependency-free data module both tiers import.
{
  lib,
  pkgs,
}:

let
  # ── the credential DENYLIST (defence in depth) ─────────────────────────
  # Mirrors ../myconfig.ai.microvm/config-seed.nix `denyNames/Suffixes/Infixes`
  # verbatim. Matched case-insensitively against every path component and
  # against the resolved target's components below the host home / store root.
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

  # ── denylist OVERRIDES (exceptions for trademark/name collisions) ─────
  # A small, explicit allowlist of path PREFIXES (relative to `$HOME`) that
  # are EXEMPT from the credential denylist. This exists because the denylist
  # matches by NAME only (defence in depth), and a few legitimate, non-secret
  # files have names that collide with a deny infix/suffix purely as a
  # trademark or convention — most notably the `trustedtokens-provider` pi
  # extension ("TrustedTokens" is a TNG inference-service trademark; the
  # directory contains only TypeScript source + metadata, no API key).
  #
  # Overrides are PREFIXES, so an override for `.pi/agent/extensions/trustedtokens-provider`
  # exempts everything UNDER it. Each override is STILL subject to:
  #   * the eval-time `pathWellFormed` check (no `..`, no absolute path, …),
  #   * the allowlist itself — an override does not GRANT staging rights; it
  #     only lifts the denylist for an entry the allowlist already covers.
  # A credential file could only sneak in via an override if a real
  # credential were placed INSIDE an overridden directory AND its name did
  # not independently match the denylist — a narrow, auditable surface.
  denyOverrides = [
    # Pi provider extension for TrustedTokens (TNG Technology Consulting).
    # Contains only extension source (`index.ts`, `README.md`, `package.json`);
    # the API key flows through the `OPENAI_API_KEY` environment, never a file.
    ".pi/agent/extensions/trustedtokens-provider"
  ];

  # ── the per-agent configuration ALLOWLIST ─────────────────────────────
  # MIRRORS ../myconfig.ai.microvm/agents.nix `specs.<name>.configPaths`.
  # Exact files and exact directories, relative to `$HOME`. A credential-shaped
  # entry is rejected at eval time by `mkSeeder`'s assertions (below) and at
  # runtime by the denylist. Missing paths are simply absent in the guest, so
  # an entry may name something this host does not have.
  agentConfigPaths = {
    # `~/.claude` also holds `.credentials.json`, so only the two paths the
    # host home-manager config actually renders are allowlisted.
    claude = [
      ".agents/skills"
      ".claude/settings.json"
      ".claude/skills"
    ];
    # Codex keeps its OAuth/API credential in `~/.codex/auth.json` (and session
    # transcripts in `~/.codex/sessions`), so the DIRECTORY is never staged —
    # only the rendered configuration and the skills tree.
    codex = [
      ".agents/skills"
      ".codex/config.toml"
      ".codex/hooks.json"
      ".codex/skills"
    ];
    # `~/.config/opencode` also collects `auth.json` and host-coupled plugins,
    # so only the rendered configuration, agents/commands and the skills tree
    # are staged.
    opencode = [
      ".agents/skills"
      ".config/opencode/agents"
      ".config/opencode/commands"
      ".config/opencode/opencode.json"
      ".config/opencode/skills"
      ".config/opencode/tui.json"
    ];
    # `~/.pi` also holds session state and provider credentials, so only the
    # rendered agent configuration is staged.
    pi = [
      ".agents/skills"
      ".pi/agent/agents"
      ".pi/agent/extensions"
      ".pi/agent/keybindings.json"
      ".pi/agent/prompts"
      ".pi/agent/themes"
    ];
    # Hermes keeps config.yaml, `.env`, `auth.json`, state.db and sessions/ in
    # ONE root, so configuration is inseparable from credentials there:
    # nothing is staged. The guest gets its endpoint from the SSH environment.
    hermes = [ ];
    # qwen-code / github-copilot-cli carry no host-rendered configuration that
    # is safe + useful to stage (their config roots mix credentials), so they
    # contribute only the shared skills tree.
    qwen-code = [ ".agents/skills" ];
    github-copilot-cli = [ ".agents/skills" ];
  };

  # Module-wide additions, mirroring
  # `myconfig.ai.microvm.configSeed.extraPaths`: agent-independent, known-safe
  # paths every sandboxed guest benefits from.
  extraPaths = [
    ".config/git/attributes"
    ".config/git/config"
  ];

  # The union of the allowlists for a given set of agent names, sorted +
  # deduplicated so the generated stager is stable. Agents not present in the
  # map contribute only the shared `extraPaths`.
  configPathsFor =
    agentNames:
    lib.unique (
      lib.sort (a: b: a < b) (extraPaths ++ lib.concatMap (n: agentConfigPaths.${n} or [ ]) agentNames)
    );

  # ── validation (mirrors config-seed.nix eval assertions) ──────────────
  # A malformed or credential-shaped entry is a policy bug; fail at eval.
  lower = lib.toLower;
  # A path is OVERRIDDEN when it lies under one of the `denyOverrides`
  # prefixes. The denylist is then SKIPPED for that path — but only the
  # INFIX checks (see `componentDenied`); exact-name and suffix checks
  # always apply, so a real `auth.json` placed inside an overridden
  # directory is still refused at eval AND runtime.
  pathOverridden = p: lib.any (prefix: p == prefix || lib.hasPrefix "${prefix}/" p) denyOverrides;

  componentDenied =
    c:
    let
      lc = lower c;
    in
    lib.elem lc denyNames
    || lib.any (s: lib.hasSuffix s lc) denySuffixes
    || lib.any (i: lib.hasInfix i lc) denyInfixes;
  # Like `pathDenied` but skips the infix checks (the override exists
  # precisely for trademark names colliding with a deny infix).
  componentDeniedNoInfix =
    c:
    let
      lc = lower c;
    in
    lib.elem lc denyNames || lib.any (s: lib.hasSuffix s lc) denySuffixes;
  pathDenied =
    p:
    if pathOverridden p then
      lib.any componentDeniedNoInfix (lib.splitString "/" p)
    else
      lib.any componentDenied (lib.splitString "/" p);

  pathWellFormed =
    p:
    lib.isString p
    && p != ""
    && !lib.hasPrefix "/" p
    && !lib.hasPrefix "-" p
    && !lib.hasSuffix "/" p
    && !lib.hasInfix ".." p
    && builtins.match "[A-Za-z0-9._][-A-Za-z0-9._/]*" p != null;

  validatePaths =
    paths:
    let
      malformed = lib.filter (p: !pathWellFormed p) paths;
      # `pathDenied` already applies the surgical override (infix-only
      # exemption), so a credential-shaped entry like `auth.json` is STILL
      # rejected even under an override prefix — matching the runtime bash
      # logic exactly. The override only exempts the INFIX checks that
      # collide with trademarks like "trustedtokens".
      denied = lib.filter (p: pathWellFormed p && pathDenied p) paths;
    in
    lib.optional (malformed != [ ]) (
      "seed-agent-config: the staging allowlist contains path(s) that are not plain, relative, '..'-free paths: "
      + lib.concatStringsSep ", " (map (p: "'${p}'") malformed)
      + "."
    )
    ++ lib.optional (denied != [ ]) (
      "seed-agent-config: the staging allowlist contains path(s) that look like CREDENTIAL material and must never be staged into a guest: "
      + lib.concatStringsSep ", " (map (p: "'${p}'") denied)
      + "."
    );

  bashArray = xs: lib.concatMapStringsSep " " lib.escapeShellArg xs;

  # ── the seeder builder ────────────────────────────────────────────────
  # Returns a `writeShellApplication` named `seed-agent-config` that, given a
  # forwarded SSH port, an identity file and the `agent@host` target, copies
  # the allowlisted + denylist-filtered host configuration into the guest
  # `/home/agent`. The allowlist + denylist are BAKED IN (never taken from the
  # caller), so the launcher cannot widen the policy.
  #
  # Usage (from a sandboxed-* wrapper):
  #   seed-agent-config "$ssh_port" "$runtime_dir/id" 127.0.0.1 agent
  mkSeeder =
    { configPaths }:
    let
      errors = validatePaths configPaths;
    in
    if errors != [ ] then
      throw (lib.concatStringsSep "\n" errors)
    else
      pkgs.writeShellApplication {
        name = "seed-agent-config";
        runtimeInputs = with pkgs; [
          coreutils
          findutils
          rsync
          openssh
        ];
        text = ''
          set -euo pipefail

          # ---- baked policy (never taken from the caller) --------------------
          readonly ALLOWLIST=(${bashArray configPaths})
          readonly DENY_NAMES=(${bashArray denyNames})
          readonly DENY_SUFFIXES=(${bashArray denySuffixes})
          readonly DENY_INFIXES=(${bashArray denyInfixes})
          readonly DENY_OVERRIDES=(${bashArray denyOverrides})
          readonly GUEST_HOME=/home/agent

          PROG="seed-agent-config"
          die() { printf '%s: error: %s\n' "$PROG" "$*" >&2; exit 1; }
          log() { printf '%s: %s\n' "$PROG" "$*" >&2; }

          [[ $# -ge 4 ]] || die "usage: $PROG <ssh-port> <identity> <host> <user> [extra-ssh-opts...]"
          ssh_port="$1"; identity="$2"; ssh_host="$3"; ssh_user="$4"; shift 4

          readonly HOST_HOME="''${HOME:?HOME must be set}"
          [[ -d "$HOST_HOME" ]] || die "host home is not a directory: $HOST_HOME"
          home_real="$(realpath -e -- "$HOST_HOME")" \
            || die "cannot resolve host home: $HOST_HOME"

          # ---- per-invocation staging directory (ephemeral) ------------------
          staging="$(mktemp -d "''${TMPDIR:-/tmp}/seed-agent-config.XXXXXX")"
          cleanup() { rm -rf -- "$staging"; }
          trap cleanup EXIT INT TERM

          # ---- denylist (component-level, case-insensitive) -----------------
          # `path_is_denied <path>` returns 0 (denied) when ANY component of
          # `<path>` matches the denylist. A path under one of the
          # `DENY_OVERRIDES` prefixes (trademark/name-collision exceptions) is
          # exempt from the INFIX checks ONLY — the exact-name and suffix
          # checks still apply, so a real `auth.json` or `*.pem` placed inside
          # an overridden directory is still refused.
          path_is_overridden() {
              local path="$1" prefix
              for prefix in "''${DENY_OVERRIDES[@]+''${DENY_OVERRIDES[@]}}"; do
                  if [[ "$path" == "$prefix" || "$path" == "$prefix"/* ]]; then
                      return 0
                  fi
              done
              return 1
          }
          path_is_denied() {
              local path="$1" comp lc pat
              local -a comps=()
              local IFS=/
              local overridden=0
              path_is_overridden "$path" && overridden=1
              read -ra comps <<< "$path"
              for comp in "''${comps[@]+''${comps[@]}}"; do
                  [[ -n "$comp" ]] || continue
                  lc="''${comp,,}"
                  for pat in "''${DENY_NAMES[@]}"; do
                      [[ "$lc" == "$pat" ]] && return 0
                  done
                  for pat in "''${DENY_SUFFIXES[@]}"; do
                      [[ "$lc" == *"$pat" ]] && return 0
                  done
                  # Infix checks are SKIPPED for overridden paths (the override
                  # exists precisely to allow trademark names like
                  # "trustedtokens" that collide with a deny infix).
                  if [[ "$overridden" == 0 ]]; then
                      for pat in "''${DENY_INFIXES[@]}"; do
                          [[ "$lc" == *"$pat"* ]] && return 0
                      done
                  fi
              done
              return 1
          }

          # A resolved path may only live INSIDE the host home or in /nix/store
          # (home-manager renders dotfiles there). Anything else is rejected.
          resolved_is_allowed() {
              case "$1" in
                  "$home_real"/*) return 0 ;;
                  /nix/store/*) return 0 ;;
                  *) return 1 ;;
              esac
          }

          # The denylist must also apply to the RESOLVED target, so a benignly
          # named symlink cannot stage a credential (e.g. a link named
          # `config.toml` -> `auth.json`).
          resolved_is_denied() {
              local real="$1" probe
              case "$real" in
                  "$home_real"/*) probe="''${real#"$home_real"/}" ;;
                  /nix/store/*/*) probe="''${real#/nix/store/*/}" ;;
                  /nix/store/*) probe="" ;;
                  *) return 0 ;;
              esac
              [[ -n "$probe" ]] || return 1
              path_is_denied "$probe"
          }

          staged=0
          skipped=0

          # ---- stage one regular file ----------------------------------------
          # Dereferences symlinks (store links -> plain copies), enforces the
          # denylist on BOTH the relative name and the resolved target, and
          # skips anything that is not a regular file.
          stage_file() {
              local rel="$1" src="$2" real
              if ! real="$(realpath -e -- "$src" 2>/dev/null)"; then
                  log "skipped $rel: unresolvable"
                  skipped=$((skipped + 1)); return 0
              fi
              if ! resolved_is_allowed "$real"; then
                  log "skipped $rel: resolves outside host home ($real)"
                  skipped=$((skipped + 1)); return 0
              fi
              if resolved_is_denied "$real"; then
                  log "skipped $rel: resolves onto a credential-shaped path"
                  skipped=$((skipped + 1)); return 0
              fi
              if [[ ! -f "$real" ]]; then
                  log "skipped $rel: not a regular file"
                  skipped=$((skipped + 1)); return 0
              fi
              if [[ -u "$real" || -g "$real" ]]; then
                  log "skipped $rel: setuid/setgid file"
                  skipped=$((skipped + 1)); return 0
              fi
              mkdir -p -- "$staging/$(dirname -- "$rel")"
              install -m 0644 -- "$real" "$staging/$rel"
              staged=$((staged + 1))
          }

          # ---- stage one allowlisted directory (walk, filter, copy) ---------
          stage_dir() {
              local rel="$1" real="$2" f sub real_sub
              mkdir -p -- "$staging/$rel"
              while IFS= read -r -d "" f; do
                  sub="''${f#"$real"}"; sub="''${sub#/}"
                  [[ -n "$sub" ]] || continue
                  # Check the denylist on the FULL home-relative path
                  # ("$rel/$sub"), not just "$sub" relative to the walked
                  # directory, so a `DENY_OVERRIDES` prefix (which is a
                  # full home-relative path) can match.
                  if path_is_denied "$rel/$sub"; then
                      log "skipped $rel/$sub: matches the credential denylist"
                      skipped=$((skipped + 1)); continue; fi
                  if [[ -d "$f" ]]; then
                      if ! real_sub="$(realpath -e -- "$f" 2>/dev/null)" \
                          || ! resolved_is_allowed "$real_sub"; then
                          log "skipped $rel/$sub: directory resolves outside host home"
                          skipped=$((skipped + 1)); continue
                      fi
                      if resolved_is_denied "$real_sub"; then
                          log "skipped $rel/$sub: resolves onto a credential-shaped path"
                          skipped=$((skipped + 1)); continue
                      fi
                      mkdir -p -- "$staging/$rel/$sub"
                      continue
                  fi
                  stage_file "$rel/$sub" "$f"
              done < <(find -L "$real" -mindepth 1 -maxdepth 12 \
                           \( -type d -o -type f \) -print0 2>/dev/null | sort -z)
          }

          # ---- stage one allowlist entry ------------------------------------
          stage_entry() {
              local rel="$1" src real
              case "$rel" in
                  "" | /* | -* | *..* | */)
                      log "skipped $rel: not a plain relative path"
                      skipped=$((skipped + 1)); return 0 ;;
              esac
              if path_is_denied "$rel"; then
                  log "skipped $rel: matches the credential denylist"
                  skipped=$((skipped + 1)); return 0; fi
              src="$HOST_HOME/$rel"
              if ! real="$(realpath -e -- "$src" 2>/dev/null)"; then
                  # A MISSING optional path is normal (an agent may not be
                  # configured on this host at all), not an error.
                  return 0
              fi
              if ! resolved_is_allowed "$real"; then
                  log "skipped $rel: resolves outside host home ($real)"
                  skipped=$((skipped + 1)); return 0
              fi
              if resolved_is_denied "$real"; then
                  log "skipped $rel: resolves onto a credential-shaped path"
                  skipped=$((skipped + 1)); return 0
              fi
              if [[ -d "$real" ]]; then
                  stage_dir "$rel" "$real"
              elif [[ -f "$real" ]]; then
                  stage_file "$rel" "$real"
              else
                  log "skipped $rel: neither a regular file nor a directory"
                  skipped=$((skipped + 1))
              fi
          }

          for entry in "''${ALLOWLIST[@]+''${ALLOWLIST[@]}}"; do
              stage_entry "$entry"
          done

          log "staged $staged file(s) (skipped $skipped) from $HOST_HOME"

          # ---- transfer the cleaned tree into the guest over SSH ------------
          # rsync over the per-invocation SSH channel. `--checksum` is not
          # needed (the guest home is a fresh tmpfs every boot); a plain
          # recursive copy suffices. `-L` dereferences any link left in
          # staging (there should be none). The guest already owns its home,
          # so no chown is needed.
          ssh_rsh="ssh -p $ssh_port -i $identity \
            -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null \
            -o LogLevel=ERROR $*"

          # Create the guest home (idempotent) so rsync has a target.
          ssh -p "$ssh_port" -i "$identity" \
            -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null \
            -o LogLevel=ERROR "$@" \
            "$ssh_user@$ssh_host" "mkdir -p '$GUEST_HOME'" 2>/dev/null || true

          rsync --recursive --links --perms --times \
            --rsh="$ssh_rsh" \
            "$staging/" "$ssh_user@$ssh_host:$GUEST_HOME/" \
            || die "rsync of staged configuration into the guest failed"

          log "seeded $GUEST_HOME for $ssh_user@$ssh_host"
        '';
        meta = with lib; {
          description = "Stage allowlisted host agent configuration into a sandboxed-* microVM guest home over SSH";
          platforms = platforms.linux;
        };
      };
in
{
  # The credential denylist (exported for documentation/tests).
  inherit
    denyNames
    denySuffixes
    denyInfixes
    denyOverrides
    ;

  # The per-agent allowlist, mirroring ../myconfig.ai.microvm/agents.nix.
  inherit agentConfigPaths extraPaths;

  # The union allowlist for a set of agent names.
  inherit configPathsFor;

  # Build a `seed-agent-config` script baking in the given allowlist.
  inherit mkSeeder;
}
