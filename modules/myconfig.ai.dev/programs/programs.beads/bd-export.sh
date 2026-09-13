#!/usr/bin/env bash
# shellcheck shell=bash
#
# bd-export: export the full beads state of the current repository as
# human- and AI-readable markdown.
#
# Usage: bd-export [--force] <target-dir>
#
# Reads the .beads DB that bd auto-discovers from the working directory
# (this script has no --db of its own: run it from the repo you want to
# export) and writes a stateless, complete markdown snapshot:
#
#   <target-dir>/README.md        index: counts by status/type/priority,
#                                 ready set (open issues with no open
#                                 blockers — the same rule as `bd ready`),
#                                 the dependency graph, and one-line
#                                 summaries linking to the per-issue files,
#                                 grouped by status
#   <target-dir>/issues/<id>.md  one file per issue: metadata table,
#                                 verbatim description, acceptance
#                                 criteria, design notes, notes, labels,
#                                 dependencies in both directions
#                                 (outgoing grouped by type, incoming as a
#                                 reverse list), comments, timestamps and
#                                 close reason
#
# The export is a full regeneration per run (no incremental sync), so the
# target must be empty: bd-export creates <target-dir> and refuses to run
# if it already exists and is non-empty. --force (or BD_EXPORT_FORCE=1)
# empties the directory first. `--` ends option parsing.
#
# Non-issue record types of `bd export --all` (infra beads, templates,
# gates, memories — everything with `_type != "issue"`) are skipped: they
# can contain agent/session context and are not part of the issue
# landscape; the README notes which types were skipped. `bd export` itself
# only covers the issues table, not Dolt history — for a full database
# backup use `bd backup init` / `bd backup sync`.
#
# Runtime deps: bd (resolved from PATH at runtime — this script ships next
# to the bd package, so it runs wherever beads is installed), plus jq and
# core utilities via the wrapper's runtimeInputs. The JSONL → markdown
# transform is jq: a one-shot record→lines mapping needs no scripting
# language, and jq keeps the pipeline small (python3 would work too but is
# a much bigger runtime input for zero gain here).

set -euo pipefail

usage() {
    echo "Usage: bd-export [--force] <target-dir>" >&2
    echo "" >&2
    echo "Export the full beads state of the current repository (bd" >&2
    echo "auto-discovers .beads from the working directory) as markdown:" >&2
    echo "README.md index plus issues/<id>.md per issue into <target-dir>." >&2
    echo "The target directory is created and must be empty; --force" >&2
    echo "(or BD_EXPORT_FORCE=1) empties an existing one first." >&2
}

force=0
target=""
end_of_opts=0
for arg in "${@}"; do
    if [ "$end_of_opts" = 0 ] && [ "$arg" = "--" ]; then
        end_of_opts=1
    elif [ "$end_of_opts" = 0 ] && [ "$arg" = "-h" ] || [ "$arg" = "--help" ]; then
        usage
        exit 0
    elif [ "$end_of_opts" = 0 ] && [ "$arg" = "--force" ]; then
        force=1
    elif [ "$end_of_opts" = 0 ] && [ "${arg#-}" != "$arg" ]; then
        echo "bd-export: unknown option: $arg" >&2
        usage
        exit 2
    elif [ -z "$target" ]; then
        target="$arg"
    else
        echo "bd-export: unexpected argument: $arg (exactly one target directory)" >&2
        usage
        exit 2
    fi
done
if [ "${BD_EXPORT_FORCE:-0}" = "1" ]; then
    force=1
fi
if [ -z "$target" ]; then
    echo "bd-export: missing target directory" >&2
    usage
    exit 2
fi

command -v bd >/dev/null 2>&1 || {
    echo "bd-export: bd not found on PATH" >&2
    exit 1
}

export_all="$(mktemp)"
export_issues="$(mktemp)"
# Incoming dependency edges per issue ("<to-id>\t<type>\t<from-id>", sorted),
# precomputed once: `bd export` stores every edge only on the issue that
# created it, so the reverse direction ("who blocks me", "who discovered
# me") needs this pass to be visible on the target issue's own file.
reverse_deps_file="$(mktemp)"
trap 'rm -f "$export_all" "$export_issues" "$reverse_deps_file"' EXIT

# One `bd export --all` is the single source of truth for everything below.
bd export --all >"$export_all"

# Non-issue record types present in the export (skipped, see header).
skipped_types="$(
    jq -rs '
        [.[] | select(has("_type")) | select(._type != "issue") | ._type]
        | unique | if length == 0 then "none" else join(", ") end
    ' "$export_all"
)"

# Everything downstream works on issues only. Records without a `_type`
# are treated as issues so the script keeps working if bd drops the field.
jq -c 'select((._type? // "issue") == "issue")' "$export_all" >"$export_issues"

jq -r '
    . as $issue
    | (.dependencies // [])[]
    | "\(.depends_on_id)\t\(.type)\t\($issue.id)"
' "$export_issues" | sort >"$reverse_deps_file"

# --- target directory, with clobber-safety -------------------------------

mkdir -p -- "$target"
if [ "$force" = 1 ]; then
    find "$target" -mindepth 1 -delete
elif [ -n "$(ls -A "$target")" ]; then
    echo "bd-export: refusing to overwrite non-empty target: $target" >&2
    echo "(use --force or BD_EXPORT_FORCE=1 to empty it first)" >&2
    exit 1
fi
mkdir -p -- "$target/issues"

# --- per-issue files ------------------------------------------------------

# Stable, human-friendly order: open/in_progress first, then any other
# status, then closed; within a group by priority, then id. Emitted as
# "<sortkey>\t<id>" lines.
issue_ids="$(
    jq -r '
        (if .status == "open" or .status == "in_progress" then 0
         elif .status == "closed" then 2
         else 1 end) * 10 + .priority,
        (.id)
    ' "$export_issues" |
        paste - - | sort -t"$(printf '\t')" -k1,1n -k2,2 | cut -f2
)"

for id in $issue_ids; do
    # Outgoing edges are read from the issue record itself; incoming
    # edges come from the precomputed reverse table (see above).
    incoming="$(awk -F "\t" -v id="$id" '$1 == id {print $3 " (" $2 ")"}' "$reverse_deps_file")"
    jq -r --arg id "$id" --arg incoming "$incoming" '
        select(.id == $id)
        | (.priority | "P" + tostring) as $prio
        | (if (.labels // []) | length > 0 then
               "## Labels\n\n" + (.labels | map("- \(.)") | join("\n")) + "\n\n"
           else "" end) as $labels
        | ([$incoming | split("\n")[] | select(length > 0) | "- \(.)"]
           | if length == 0 then "" else "## Reverse dependencies (incoming edges)\n\n" + join("\n") + "\n\n" end) as $rev
        | ([(.comments // [])[]
           | "- (" + (.author // "unknown") + " at " + .created_at + "):\n\n    " + (.text // .body // "")]
           | if length == 0 then "" else "## Comments\n\n" + join("\n\n") + "\n\n" end) as $comments
        | "# \(.id): \(.title)
| Status    | Priority | Type | Assignee | Owner | Created by |
|-----------|----------|------|----------|-------|------------|
| \(.status) | \($prio) | \(.issue_type) | \(.assignee // "-") | \(.owner // "-") | \(.created_by // "-") |

- Created: \(.created_at)
- Updated: \(.updated_at)\(
    if .started_at != null then
        "\n- Started: \(.started_at)" else "" end)\(
    if .closed_at != null then
        "\n- Closed: \(.closed_at)" else "" end)\(
    if .parent != null then
        "\n- Parent issue: \(.parent)" else "" end)

## Description

\(.description)

\(
    if (.acceptance_criteria // "") != "" then
        "## Acceptance criteria\n\n\(.acceptance_criteria)\n\n" else "" end)\(
    if (.design // "") != "" then
        "## Design notes\n\n\(.design)\n\n" else "" end)\(
    if (.notes // "") != "" then
        "## Notes\n\n\(.notes)\n\n" else "" end)\($labels)\($rev)\(
    if ((.dependencies // []) | length) > 0 then
        (.dependencies
         | group_by(.type)
         | map("### " + .[0].type + "\n\n" +
               (map("- " + .depends_on_id) | join("\n")))
         | join("\n\n")
         | "## Dependencies\n\n" + . + "\n\n")
    else "" end)\($comments)\(
    if has("close_reason") and .close_reason != null then
        "## Close reason\n\n\(.close_reason)\n" else "" end)
"
    ' "$export_issues" >"$target/issues/$id.md"
done

# --- README index ---------------------------------------------------------

status_counts="$(
    jq -rs 'group_by(.status) | map("\(.[0].status): \(length)") | join(", ")' \
        "$export_issues"
)"
type_counts="$(
    jq -rs 'group_by(.issue_type) | map("\(.[0].issue_type): \(length)") | join(", ")' \
        "$export_issues"
)"
prio_counts="$(
    jq -rs 'group_by(.priority) | map("P\(.[0].priority): \(length)") | join(", ")' \
        "$export_issues"
)"

{
    echo "# beads export"
    echo ""
    echo "## Overview"
    echo ""
    echo "- Exported: $(date -u +"%Y-%m-%dT%H:%M:%SZ")"
    echo "- Issues by status: $status_counts"
    echo "- Issues by type: $type_counts"
    echo "- Issues by priority: $prio_counts"
    echo "- Skipped non-issue record types: $skipped_types"
    echo ""
    # shellcheck disable=SC2016  # (backticks are literal markdown, not
    # command substitution, for the echo lines below)
    if true; then
        echo 'This is a stateless full export of `bd export --all`: issue'
        echo 'details in [`issues/`](issues/) - one `issues/<id>.md` per issue,'
        echo 'indexed by id. Generated by `bd-export` from the repository whose'
        echo '`.beads` DB bd auto-discovers from the working directory.'
    fi
    echo ""

    # Ready set: open issues with no open blockers, the same rule as
    # `bd ready`: a depends-on/blocks edge counts as a blocker only if the
    # issue at the other end exists and is open or in_progress. Edges to
    # closed/missing issues do not block.
    echo "## Ready"
    echo ""
    # shellcheck disable=SC2016  # (backticks are literal markdown)
    if true; then
        echo 'Open issues with no open blockers (as `bd ready` reports):'
    fi
    echo ""
    jq -rs '
        (reduce .[] as $r ({};
            . + { ($r.id): ($r.status == "open" or $r.status == "in_progress") }
        )) as $open
        | [.[] | select(.status == "open")
            | select([ (.dependencies // [])[]
                       | select(.type == "depends-on" or .type == "blocks")
                       | .depends_on_id
                       | select($open[.] == true) ] | length == 0)]
        | if length == 0 then "_None._" else
            (sort_by(.priority)
             | map("- [\(.id)](issues/\(.id).md) - \(.title)")
             | join("\n"))
        end
    ' "$export_issues"
    echo ""

    # Dependency overview. In the export every edge is stored on the
    # blocked issue: a `blocks` row on A pointing at B means "A is blocked
    # by B" (`bd show` renders it under DEPENDS ON). depends-on/blocks are
    # the blocker relation; parent-child and discovered-from are
    # informational links.
    echo "## Dependencies"
    echo ""
    blocker_edges="$(
        jq -r '
            (.dependencies // [])[]
            | select(.type == "depends-on" or .type == "blocks")
            | "| \(.issue_id) | \(.type) | \(.depends_on_id) |"
        ' "$export_issues" | sort -u
    )"
    if [ -n "$blocker_edges" ]; then
        echo "| Blocked | Type | Blocks |"
        echo "|---------|------|--------|"
        printf '%s\n' "$blocker_edges"
    else
        echo "_None._"
    fi
    echo ""
    echo "Parent/child and discovered-from links:"
    echo ""
    links="$(
        jq -r '
            (.dependencies // [])[]
            | select(.type == "parent-child" or .type == "discovered-from")
            | "| \(.issue_id) | \(.type) | \(.depends_on_id) |"
        ' "$export_issues" | sort -u
    )"
    if [ -n "$links" ]; then
        echo "| From | Type | To |"
        echo "|------|------|----|"
        printf '%s\n' "$links"
    else
        echo "_None._"
    fi
    echo ""

    # Grouped issue lists, one-line summaries linking to the detail files.
    echo "## Issues"
    echo ""
    statuses="$(
        {
            printf '%s\n' open in_progress
            jq -r '.status' "$export_issues"
        } |
            awk '!seen[$0]++'
    )"
    while IFS= read -r status; do
        entries="$(
            jq -r --arg st "$status" \
                'select(.status == $st) | "\(.priority)\t\(.id)\t\(.title)"' \
                "$export_issues" |
                sort -t"$(printf '\t')" -k1,1n -k2,2
        )"
        [ -z "$entries" ] && continue
        count="$(printf '%s\n' "$entries" | grep -c . || true)"
        echo "### $status ($count)"
        echo ""
        printf '%s\n' "$entries" |
            while IFS="$(printf '\t')" read -r prio id title; do
                echo "- P$prio [\`$id\`](issues/$id.md) - $title"
            done
        echo ""
    done <<<"$statuses"
} >"$target/README.md"
