# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# fish tab completion for `agent-gvisor`.
#
# Hand-written on purpose, like the CLI itself: the crate is
# zero-dependency, so there is no clap/complete machinery to generate this
# from. The grammar mirrors docs/spec.md §1–§3 and rust/src/usage.txt and
# must be kept in sync with them by hand — the `agent-gvisor-completions`
# check (../../nix/checks.nix) enforces the sync: it parses this file with
# `fish -n` and fails when a subcommand or an option documented in
# usage.txt is not completed.
#
# Installed by ../../nix/agent-gvisor.nix into
# $out/share/fish/vendor_completions.d/agent-gvisor.fish.

# Session names from the registry, like the Rust CLI's Env::state_root
# (rust/src/state.rs): $AGENT_GVISOR_STATE, else
# ${XDG_STATE_HOME:-$HOME/.local/state}/agent-gvisor. A dangling registry
# symlink is not a session (the CLI probes with -e, which follows links).
function __agent_gvisor_sessions
    set -l state $AGENT_GVISOR_STATE
    if test -z "$state"
        set -l base $XDG_STATE_HOME
        if test -z "$base"
            set base $HOME/.local/state
        end
        set state $base/agent-gvisor
    end
    set -l sessions $state/sessions
    if not test -d "$sessions"
        return 1
    end
    for entry in $sessions/*
        test -e "$entry"; or continue
        string replace -r '.*/' '' -- $entry
    end
end

# The `start` grammar (docs/spec.md §1): after the `start` word, or after
# the NAME of the positional shorthand (`agent-gvisor NAME [flags…]`,
# where every first word that is no other action word starts a session).
# A leading flag is not completed: the dispatcher rejects a first `-` word
# with `unknown subcommand`.
function __agent_gvisor_in_start
    __fish_seen_subcommand_from start
    and return 0
    __fish_seen_subcommand_from list status run logs shell stop merge fetch push destroy doctor workmux
    and return 1
    test (count (commandline -opc)) -ge 2
end

# The NAME position of every name-taking subcommand (docs/spec.md §3):
# NAME is always the first word after the action.
function __agent_gvisor_expects_name
    __fish_seen_subcommand_from status run logs shell stop merge fetch push destroy
    and test (count (commandline -opc)) -eq 2
end

# Subcommands (and the usage words) — only at the first position.
complete -c agent-gvisor -f -n '__fish_use_subcommand' -a start -d 'Start a worktree session and its container'
complete -c agent-gvisor -f -n '__fish_use_subcommand' -a list -d 'List sessions (SESSION STATUS BRANCH WORKTREE)'
complete -c agent-gvisor -f -n '__fish_use_subcommand' -a status -d 'Show session fields, container state and git status'
complete -c agent-gvisor -f -n '__fish_use_subcommand' -a run -d 'Run a command in a stopped session container'
complete -c agent-gvisor -f -n '__fish_use_subcommand' -a logs -d 'Show the container logs (podman logs)'
complete -c agent-gvisor -f -n '__fish_use_subcommand' -a shell -d 'Open a shell (or run COMMAND) in the session'
complete -c agent-gvisor -f -n '__fish_use_subcommand' -a stop -d 'Stop the session container'
complete -c agent-gvisor -f -n '__fish_use_subcommand' -a merge -d 'Merge the session branch into the host repo'
complete -c agent-gvisor -f -n '__fish_use_subcommand' -a fetch -d 'Fetch the session branch from the session worktree into a local branch'
complete -c agent-gvisor -f -n '__fish_use_subcommand' -a push -d 'Push the session branch to a remote (fetches it first)'
complete -c agent-gvisor -f -n '__fish_use_subcommand' -a destroy -d 'Remove container, worktree and session state'
complete -c agent-gvisor -f -n '__fish_use_subcommand' -a workmux -d 'Run the whole workmux/tmux session of this repo in one sandbox'
complete -c agent-gvisor -f -n '__fish_use_subcommand' -a doctor -d 'Verify runtime, image and sandbox startup'
complete -c agent-gvisor -f -n '__fish_use_subcommand' -a help -d 'Print usage'
complete -c agent-gvisor -f -n '__fish_use_subcommand' -s h -l help -d 'Print usage'

# Existing session names for the NAME position (and for start --name,
# where they help with --force restarts of an existing session).
complete -c agent-gvisor -f -n '__agent_gvisor_expects_name' -a '(__agent_gvisor_sessions)' -d 'Existing session'
complete -c agent-gvisor -n '__agent_gvisor_in_start' -l name -x -a '(__agent_gvisor_sessions)' -d 'Session name (alternative to the positional NAME)'

# start options (docs/spec.md §2, descriptions from rust/src/usage.txt).
# The path-valued flags (--repo, --env-file, --home-seed) use `-r`: fish's
# default file completion, which walks directories — deliberately no
# `__fish_complete_directories`, whose helper signature is an internal
# detail of fish.
complete -c agent-gvisor -n '__agent_gvisor_in_start' -l repo -r -d 'Host repository (default: the current directory)'
complete -c agent-gvisor -n '__agent_gvisor_in_start' -l base -x -d 'Base commit/ref in the host repository (default: HEAD)'
complete -c agent-gvisor -n '__agent_gvisor_in_start' -l branch -x -d 'Worktree branch (default: agent/gvisor/NAME)'
complete -c agent-gvisor -n '__agent_gvisor_in_start' -l image -x -d 'Podman image (default: $AGENT_GVISOR_DEFAULT_IMAGE)'
complete -c agent-gvisor -n '__agent_gvisor_in_start' -l config -x -d 'Mount an agent configuration path HOST:DEST[:ro|rw]; repeatable'
complete -c agent-gvisor -n '__agent_gvisor_in_start' -l mount -x -d 'Add another bind mount HOST:DEST[:ro|rw]; repeatable'
complete -c agent-gvisor -n '__agent_gvisor_in_start' -l env -x -d 'Pass an environment variable KEY=VALUE; repeatable'
complete -c agent-gvisor -n '__agent_gvisor_in_start' -l env-file -r -d 'Pass a Podman environment file'
complete -c agent-gvisor -n '__agent_gvisor_in_start' -l network -x -a 'none host' -d 'Podman network mode (unset: the rootless default)'
complete -c agent-gvisor -n '__agent_gvisor_in_start' -l detach -d 'Run detached rather than interactively'
complete -c agent-gvisor -n '__agent_gvisor_in_start' -l memory -x -d 'Memory limit (default: 8g)'
complete -c agent-gvisor -n '__agent_gvisor_in_start' -l cpus -x -d 'CPU limit (default: 4)'
complete -c agent-gvisor -n '__agent_gvisor_in_start' -l pids-limit -x -d 'Process limit (default: 2048)'
complete -c agent-gvisor -n '__agent_gvisor_in_start' -l seccomp-unconfined -d 'Disable the inner OCI seccomp profile'
complete -c agent-gvisor -n '__agent_gvisor_in_start' -l nix -d 'Give the session a writable Nix store volume at /nix/store'
complete -c agent-gvisor -n '__agent_gvisor_in_start' -l no-nix -d 'Start without the writable Nix store, even if enabled by default'
complete -c agent-gvisor -n '__agent_gvisor_in_start' -l force -d 'Destroy an existing session of the same name first'
complete -c agent-gvisor -n '__agent_gvisor_in_start' -l home-seed -r -d 'Seed /home/agent from this directory'
complete -c agent-gvisor -n '__agent_gvisor_in_start' -l no-home-seed -d 'Start with an empty /home/agent'

# run options (docs/spec.md §3).
complete -c agent-gvisor -n '__fish_seen_subcommand_from run' -l detach -d 'Run detached rather than interactively'

# merge options (docs/spec.md §9 "merge").
complete -c agent-gvisor -n '__fish_seen_subcommand_from merge' -l repo -r -d 'Original host repo to merge into (default: the session origin repo)'
complete -c agent-gvisor -n '__fish_seen_subcommand_from merge' -l no-ff -d 'Create a merge commit even if a fast-forward is possible (the default)'
complete -c agent-gvisor -n '__fish_seen_subcommand_from merge' -l ff -d 'Allow a fast-forward, creating no merge commit'
complete -c agent-gvisor -n '__fish_seen_subcommand_from merge' -l squash -d 'Produce a single squashed commit on the target branch'

# fetch/push options (docs/spec.md §9 "fetch"/"push").
complete -c agent-gvisor -n '__fish_seen_subcommand_from fetch push' -l repo -r -d 'Target repository (default: the repository containing the current directory)'

# destroy options (docs/spec.md §9 "destroy").
complete -c agent-gvisor -n '__fish_seen_subcommand_from destroy' -l force -d 'Remove even a worktree with uncommitted changes'
complete -c agent-gvisor -n '__fish_seen_subcommand_from destroy' -l delete-branch -d 'Also delete the session branch'

# logs: the passthrough podman-logs arguments (docs/spec.md §3).
complete -c agent-gvisor -n '__fish_seen_subcommand_from logs' -s f -l follow -d 'Follow log output'
complete -c agent-gvisor -n '__fish_seen_subcommand_from logs' -s t -l timestamps -d 'Show timestamps'
complete -c agent-gvisor -n '__fish_seen_subcommand_from logs' -l details -d 'Show extra details'
complete -c agent-gvisor -n '__fish_seen_subcommand_from logs' -l tail -x -d 'Output the specified number of lines'
complete -c agent-gvisor -n '__fish_seen_subcommand_from logs' -l since -x -d 'Only show logs since TIMESTAMP'
complete -c agent-gvisor -n '__fish_seen_subcommand_from logs' -l until -x -d 'Only show logs until TIMESTAMP'
