# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# `mysbx-git-default-diff` — the `GIT_EXTERNAL_DIFF` wrapper of the
# generated user config layer (bd myconfig-kvo, ../docs/design/config.md
# D6 "the generated `[env]`").
#
# On a difftastic host Home Manager writes `diff.external = <difft>` into
# `~/.config/git/config` — a file mysbx mounts read-only below
# `/mysbx-home`, so EVERY `git diff` inside the sandbox goes through the
# structural diff renderer. That output is made for humans: an agent
# payload (or any script that parses `git diff`) cannot read it, and on
# a host whose difftastic closure drifted the absolute store path may
# not even resolve. `GIT_EXTERNAL_DIFF` overrides `diff.external`, so the
# generated layer pins it at THIS script — which renders the DEFAULT
# unified diff with the `diff` binary of the dev-tool closure, making
# `git diff` inside the sandbox behave like a stock git again.
#
# Call contract (git(1), "Git Diffs"): for each added/removed/modified
# path git runs the command with 7 parameters
#   path old-file old-hex old-mode new-file new-hex new-mode
# and for an UNMERGED path with just 1: <path>. With the default
# `diff.external.trustExitCode = false` the command must exit 0 — any
# other code is a fatal "external diff died" — so every exit path here
# returns 0 and `diff`'s own "files differ" status (1) is swallowed.
#
# Everything the script needs is in its own closure (the writeShellApplication
# shebang and runtimeInputs are store paths, reachable inside the sandbox
# through the ro `/nix/store` bind): it does not rely on the sandbox
# `PATH`.
{
  lib,
  writeShellApplication,
  diffutils,
}:
writeShellApplication {
  name = "mysbx-git-default-diff";
  runtimeInputs = [ diffutils ];
  text = ''
    # An unmerged path: git passes only <path> and expects nothing from us.
    if [ "$#" -eq 1 ]; then
      exit 0
    fi
    # $1 path, $2 old-file, $3 old-hex, $4 old-mode, $5 new-file,
    # $6 new-hex, $7 new-mode. /dev/null marks an add or a delete; a
    # plain `diff -u` prints the right header for it, the labels keep
    # the a/… b/… prefixes of git's own output.
    if [ "$2" = "/dev/null" ]; then
      diff -u --label /dev/null --label "b/$1" "$2" "$5" || true
    elif [ "$5" = "/dev/null" ]; then
      diff -u --label "a/$1" --label /dev/null "$2" "$5" || true
    else
      diff -u --label "a/$1" --label "b/$1" "$2" "$5" || true
    fi
    exit 0
  '';
}
