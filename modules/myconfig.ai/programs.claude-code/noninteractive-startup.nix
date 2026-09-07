# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Make Claude Code start NON-INTERACTIVELY against the proxy described by the
# environment (`$ANTHROPIC_AUTH_TOKEN` + `$ANTHROPIC_BASE_URL`), instead of
# opening the first-run onboarding wizard (theme picker, "Let's get started",
# login-method selection).
#
# ── Why a launcher wrapper and not a home-manager file? ────────────────────
# Claude Code keeps the onboarding markers in its GLOBAL, MUTABLE config file
# (`~/.claude.json`), NOT in the `~/.claude/settings.json` that
# `programs.claude-code.settings` renders. That file is rewritten by Claude
# Code on nearly every interaction (`numStartups`, `projects.*`, tips, …), so
# it cannot be a read-only store symlink deployed via `home.file`.
#
# A home-manager ACTIVATION script would work for the plain host `claude`, but
# not for the sandboxed wrappers built in `./default.nix`: the bubblewrap
# wrapper (`fns/bubblewrap-simple-app.nix`) gives the agent a `tmpfs` `$HOME`,
# so a host-side `~/.claude.json` is invisible inside and the wizard would come
# back on every launch. Seeding from the launcher itself therefore fixes ALL
# tiers at once (plain, `*-bwrap`, `agent-bubblewrap-*`, `*-worktree`), because
# it runs with whatever `$HOME` / `$CLAUDE_CONFIG_DIR` the sandbox set up.
#
# ── What is seeded (and what is NOT) ───────────────────────────────────────
# Verified against the packaged `claude-code` 2.1.260 binary:
#   * `hasCompletedOnboarding` — the ONLY gate for the onboarding wizard
#     (`if (config.hasCompletedOnboarding) return null` before rendering it).
#   * `lastOnboardingVersion`, `theme` — written by the wizard alongside it;
#     seeded only when the user has not set them, so `/theme` keeps working.
#   * NOTHING credential-shaped. Authentication comes exclusively from the
#     environment: with `ANTHROPIC_AUTH_TOKEN` set, Claude Code sends it as the
#     bearer token for `ANTHROPIC_BASE_URL` and — unlike `ANTHROPIC_API_KEY` —
#     never asks the "Do you want to use this API key?" question, so no
#     `customApiKeyResponses` approval has to be pre-seeded either. No token
#     ever reaches the Nix store or a tracked file.
#   * NOT the per-project trust dialog ("Is this a project you trust?"). That
#     is a per-directory security gate, not first-run configuration; blanket
#     pre-approval would be the equivalent of `--dangerously-skip-permissions`.
#     Inside the sandboxes it is disabled through `CLAUDE_CODE_SANDBOXED=1`
#     (see `./default.nix`), where the filesystem is already confined.
{
  lib,
  pkgs,
}:

{
  # The upstream `claude-code` package to wrap.
  package,
}:

let
  version = lib.getVersion package;

  # Idempotent seeder for the global config file. Runs on every launch, but
  # takes a single `jq` fast path (and no write at all) once the markers are
  # present. A corrupt or unreadable config is left untouched — Claude Code
  # has its own auto-repair for that and we must never destroy user state.
  seedOnboarding = pkgs.writeShellApplication {
    name = "claude-code-seed-onboarding";
    runtimeInputs = with pkgs; [
      coreutils
      jq
    ];
    text = ''
      # Mirrors the upstream resolution order of the GLOBAL config file:
      # `$CLAUDE_CONFIG_DIR/.config.json` when that file exists, otherwise
      # `''${CLAUDE_CONFIG_DIR:-$HOME}/.claude.json`.
      config_dir="''${CLAUDE_CONFIG_DIR:-$HOME/.claude}"
      config_file="''${CLAUDE_CONFIG_DIR:-$HOME}/.claude.json"
      if [ -e "$config_dir/.config.json" ]; then
          config_file="$config_dir/.config.json"
      fi

      warn() { printf 'claude-code-seed-onboarding: %s\n' "$*" >&2; }

      if [ ! -e "$config_file" ]; then
          mkdir -p -- "$(dirname -- "$config_file")" 2>/dev/null || true
          # The file also ends up holding session/project state, so keep it
          # private from the very first byte.
          (umask 077 && printf '{}\n' >"$config_file") 2>/dev/null || {
              warn "cannot create $config_file — onboarding may be shown"
              exit 0
          }
      fi

      # Fast path: already seeded, nothing to write.
      if jq -e '.hasCompletedOnboarding == true' "$config_file" >/dev/null 2>&1; then
          exit 0
      fi

      # The scratch file must NOT live next to the config: in the jailed
      # wrappers only `~/.claude.json` itself is bind-mounted writable, the
      # home directory around it is not.
      tmp="$(mktemp)"
      trap 'rm -f -- "$tmp"' EXIT

      if ! jq --arg version ${lib.escapeShellArg version} '
              .hasCompletedOnboarding = true
            | .lastOnboardingVersion = (.lastOnboardingVersion // $version)
            | .theme = (.theme // "dark")
          ' "$config_file" >"$tmp" 2>/dev/null; then
          warn "$config_file is not valid JSON — left untouched"
          exit 0
      fi

      # Overwrite in place (keeps inode + mode, which Claude Code watches)
      # instead of renaming over the bind-mounted file.
      cat -- "$tmp" >"$config_file" 2>/dev/null \
          || warn "cannot update $config_file — onboarding may be shown"
    '';
    meta = with lib; {
      description = "Seed the Claude Code onboarding markers in the global config so no first-run wizard is shown";
      platforms = platforms.linux;
    };
  };
in
# `bin/claude` runs the seeder first, then execs the real binary with the
# original argv0. Everything else of the upstream package is passed through.
pkgs.symlinkJoin {
  name = "claude-code-noninteractive-${version}";
  paths = [ package ];
  inherit (package) meta;
  inherit version;
  postBuild = ''
    mv "$out/bin/claude" "$out/bin/.claude-upstream"
    cat >"$out/bin/claude" <<EOF
    #! ${pkgs.runtimeShell} -e
    ${lib.getExe seedOnboarding} || true
    exec -a "\$0" "$out/bin/.claude-upstream" "\$@"
    EOF
    chmod +x "$out/bin/claude"
  '';
  passthru = (package.passthru or { }) // {
    inherit seedOnboarding;
    unwrapped = package;
  };
}
