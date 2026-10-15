# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# Name niri workspaces dynamically based on the windows they hold: the
# first rule (app-id regex, then title regex) that matches any window on
# a workspace names it. A second workspace that wants the same name gets
# name_2, name_3, … (niri silently ignores a rename to an already-taken
# name, so collisions must be resolved here). Workspaces without a
# matching rule (or window) are un-named again.
#
# Implemented as a simple reconciliation loop that polls niri's window
# and workspace state on an interval and applies
# set-/unset-workspace-name actions for the deltas. This avoids the
# event-stream protocol constraint (a socket that served an EventStream
# request accepts no further requests) and title-change churn.
{
  lib,
  pkgs,
}:
let
  # Build the niri-ws-names daemon for the given (patched) niri package
  # and rule list [{ app-id; title; name }].
  niri-ws-names =
    {
      niri,
      rules,
      interval ? 5,
    }:
    let
      # rules table file: app-id <TAB> title <TAB> name, one rule per
      # line, evaluated in order (first match wins). Written as a file
      # (not a shell/awk variable) so regex backslash escapes survive.
      rulesFile = pkgs.writeText "niri-ws-names-rules.tsv" (
        lib.concatMapStrings (r: "${r.app-id}\t${r.title}\t${r.name}\n") rules
      );
      # One reconciliation step: compute the desired name for every
      # workspace from the current window state, diff against the
      # current names, and apply the rename actions.
      niri-ws-names-step = pkgs.writeShellApplication {
        name = "niri-ws-names-step";
        runtimeInputs = [
          niri
          pkgs.jq
          pkgs.gawk
        ];
        text = ''
          set -euo pipefail

          tmpdir=$(mktemp -d /tmp/niri-ws-names.XXXXXX)
          trap 'rm -rf "$tmpdir"' EXIT

          # workspace_id <TAB> current_name ("" if unnamed), niri's order
          niri msg --json workspaces \
            | jq -r '.[] | [(.id|tostring), (.name // "")] | @tsv' \
            > "$tmpdir/workspaces.tsv"
          # window_id <TAB> workspace_id <TAB> app_id <TAB> title
          niri msg --json windows \
            | jq -r '.[] | [(.id|tostring), ((.workspace_id // 0)|tostring), (.app_id // ""), (.title // "")] | @tsv' \
            > "$tmpdir/windows.tsv"

          # workspace_id <TAB> desired_name ("" if no rule matches)
          # <TAB> current_name. Collision suffixes _2, _3, … are
          # assigned in workspace order.
          awk -v rules="${rulesFile}" \
              -v windows="$tmpdir/windows.tsv" \
            '
              # A workspace is only "managed" if its current name is
              # one the daemon could have produced: a rule name or a
              # numbered collision variant of one. Manually or statically
              # named workspaces (config.kdl `workspace "…"`) are left
              # alone.
              BEGIN {
                nr = 0
                while ((getline line < rules) > 0) {
                  split(line, rule[++nr], /\t/)
                  rulenames[rule[nr][3]] = 1
                }
                close(rules)
                nw = 0
                while ((getline line < windows) > 0)
                  split(line, win[++nw], /\t/)
                close(windows)
              }
              {
                ws = $1
                current = $2
                desired = ""
                for (i = 1; i <= nr && desired == ""; i++)
                  for (j = 1; j <= nw; j++)
                    if (win[j][2] == ws && win[j][3] ~ rule[i][1] && win[j][4] ~ rule[i][2]) {
                      desired = rule[i][3]
                      break
                    }
                if (desired != "" && ++count[desired] > 1)
                  desired = desired "_" count[desired]
                managed = 0
                if (current != "") {
                  base = current
                  sub(/_[0-9]+$/, "", base)
                  managed = (current in rulenames) || (base in rulenames && base != current)
                }
                print ws "\037" desired "\037" current "\037" managed
              }
            ' "$tmpdir/workspaces.tsv" > "$tmpdir/plan.tsv"

          # apply: set changed names, clear stale ones (only on
          # workspaces whose current name the daemon could have set).
          # --workspace on the CLI takes an index or name, so
          # workspaces are referenced by id via the raw JSON request
          # form (niri msg raw-request, verified against pinned niri
          # 26.4.0). Fields are joined with \x1f (unit separator): tab
          # is IFS whitespace, so `read` would collapse empty fields.
          while IFS=$'\037' read -r ws desired current managed; do
            if [[ -z "$desired" ]]; then
              if [[ -n "$current" && "$managed" == 1 ]]; then
                jq -cn --argjson id "$ws" \
                  '{Action:{UnsetWorkspaceName:{reference:{Id:$id}}}}' \
                  | niri msg raw-request \
                  || echo "niri-ws-names: unsetting name of workspace $ws failed" >&2
              fi
            elif [[ "$desired" != "$current" ]]; then
              jq -cn --arg name "$desired" --argjson id "$ws" \
                  '{Action:{SetWorkspaceName:{name:$name,workspace:{Id:$id}}}}' \
                | niri msg raw-request \
                || echo "niri-ws-names: naming workspace $ws '$desired' failed (name taken?)" >&2
            fi
          done < "$tmpdir/plan.tsv"
        '';
      };
    in
    pkgs.writeShellApplication {
      name = "niri-ws-names";
      runtimeInputs = [ niri-ws-names-step ];
      text = ''
        interval=${toString interval}
        while true; do
          if ! niri-ws-names-step; then
            echo "niri-ws-names: reconciliation step failed" >&2
          fi
          sleep "$interval"
        done
      '';
    };
in
{
  inherit niri-ws-names;
}
