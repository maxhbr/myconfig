# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# fish tab completion for `mysbx`.
#
# Hand-written on purpose, like the CLI itself: the crate is
# zero-dependency with a hand-rolled argument parser, so there is no
# clap/complete machinery to generate this from. The grammar mirrors
# ../src/usage.txt and the dispatcher in ../src/lib.rs and must be kept
# in sync with them by hand — the `mysbx-completions` check
# (../../nix/checks.nix) enforces the sync: it parses this file with
# `fish -n` and fails when a subcommand or an option documented in
# usage.txt is not completed.
#
# Session names come from the registry — the `clones/` directories of
# the sidecar the current directory resolves to — via a PURE DIRECTORY
# LISTING, not `mysbx session list`: that verb runs git probes per
# session (rev-parse, for-each-ref, rev-list), which is far too slow
# for a completion, while the directory IS the registry by design
# (docs/design/workspace.md D2: "clones/ is the registry"). The sidecar
# resolution mirrors repo::resolve (../src/repo.rs): the nearest
# ancestor with an existing sidecar wins, else the nearest `.git`
# ancestor, else the current directory.
#
# Installed by ../../nix/mysbx.nix into
# $out/share/fish/vendor_completions.d/mysbx.fish.

# The grammar words before the cursor: the words of the mysbx command
# line minus `mysbx` itself, minus the values of the value-taking
# flags, minus every flag word — so the positionals remain (the verb,
# the session NAME, git remotes). Stops at `--` (cli.md D4: everything
# after it is payload, never parsed), and RESTARTS after `gui`
# (cli.md D15: the whole tail of `gui` is the payload of the INNER
# mysbx, so the grammar begins anew).
function __mysbx_tokens
    set -l words (commandline -opc)
    set -e words[1]
    set -l out
    set -l skip 0
    for word in $words
        if test $skip -eq 1
            set skip 0
            continue
        end
        if test "$word" = '--'
            break
        end
        switch "$word"
            case gui
                set out
                continue
            case --multiplexer --backend --session --timeout --ro --rw --image
                set skip 1
                continue
            case '-*'
                continue
        end
        set -a out $word
    end
    for token in $out
        echo $token
    end
end

# Whether a `--` separator has been typed: everything after it is the
# payload (run, cli.md D4) or git's own arguments (merge, D6) — none
# of mysbx's grammar to complete.
function __mysbx_past_dd
    contains -- -- (commandline -opc)
end

# The bare/global position (no verb word typed yet): the bare form is
# valid (the interactive shell, cli.md D2), and the global flags are
# accepted only here and before `run` (D10).
function __mysbx_no_verb
    __mysbx_past_dd
    and return 1
    set -l t (__mysbx_tokens)
    not set -q t[1]
end

function __mysbx_in_run
    __mysbx_past_dd
    and return 1
    contains -- run (__mysbx_tokens)
end

function __mysbx_in_session
    __mysbx_past_dd
    and return 1
    contains -- session (__mysbx_tokens)
end

function __mysbx_in_worktree
    __mysbx_past_dd
    and return 1
    contains -- worktree (__mysbx_tokens)
end

function __mysbx_in_destroy
    __mysbx_past_dd
    and return 1
    contains -- destroy (__mysbx_tokens)
end

function __mysbx_in_merge
    __mysbx_past_dd
    and return 1
    contains -- merge (__mysbx_tokens)
end

function __mysbx_in_init
    __mysbx_past_dd
    and return 1
    contains -- init (__mysbx_tokens)
end

function __mysbx_in_gvisor_load_image
    __mysbx_past_dd
    and return 1
    contains -- gvisor-load-image (__mysbx_tokens)
end

# After `merge NAME` (usage.txt): the strategy flags need the NAME,
# they are the argument tail of the verb, not head position.
function __mysbx_merge_named
    __mysbx_past_dd
    and return 1
    set -l t (__mysbx_tokens)
    set -q t[1]
    and test "$t[1]" = merge
    and set -q t[2]
end

# The sub-verb position of the session group (workspace.md D7) and
# of the worktree group (docs/design/worktree.md W1).
function __mysbx_expects_session_verb
    __mysbx_past_dd
    and return 1
    set -l t (__mysbx_tokens)
    set -q t[1]
    and test "$t[1]" = session
    and not set -q t[2]
end

function __mysbx_expects_worktree_verb
    __mysbx_past_dd
    and return 1
    set -l t (__mysbx_tokens)
    set -q t[1]
    and test "$t[1]" = worktree
    and not set -q t[2]
end

# The NAME position: the first positional of the four handoff verbs
# (workspace.md D6) and of `session destroy` (D7) — session names;
# `worktree diff` / `worktree hunk` (worktree.md W1) — worktree
# handles, via [`__mysbx_expects_worktree_name`].
function __mysbx_expects_name
    __mysbx_past_dd
    and return 1
    set -l t (__mysbx_tokens)
    set -q t[1]
    or return 1
    switch "$t[1]"
        case fetch merge push diff
            not set -q t[2]
            and return 0
            return 1
        case session
            test "$t[2]" = destroy -o "$t[2]" = hunk
            and not set -q t[3]
            and return 0
            return 1
        case worktree
            return 1
    end
    return 1
end

# The NAME position of `worktree diff NAME` / `worktree hunk NAME`
# (docs/design/worktree.md W1).
function __mysbx_expects_worktree_name
    __mysbx_past_dd
    and return 1
    set -l t (__mysbx_tokens)
    set -q t[1]
    or return 1
    test "$t[1]" = worktree
    and test "$t[2]" = diff -o "$t[2]" = hunk
    and not set -q t[3]
end

# The REMOTE position of `push NAME [REMOTE]` (D6).
function __mysbx_expects_remote
    __mysbx_past_dd
    and return 1
    set -l t (__mysbx_tokens)
    set -q t[1]
    or return 1
    test "$t[1]" = push
    and set -q t[2]
    and not set -q t[3]
end

# The existing session names of the repo the current directory
# resolves to (see the header: the registry, not `session list`).
# Like the Rust `entries` (../src/sessionverbs.rs) only DIRECTORIES
# are registry entries — the per-session `NAME.json` result files in
# the same directory are not sessions — and debris (a directory
# without `.git`) still IS one: `session list` shows it and
# `session destroy` is the verb for it.
function __mysbx_sessions
    set -l dir (pwd -P)
    set -l repo
    set -l walk $dir
    while true
        if test -d "$walk.mysbx"
            set repo $walk
            break
        end
        if test "$walk" = /
            break
        end
        set walk (dirname $walk)
    end
    if not set -q repo[1]
        set -l gitwalk $dir
        while true
            if test -e "$gitwalk/.git"
                set repo $gitwalk
                break
            end
            if test "$gitwalk" = /
                break
            end
            set gitwalk (dirname $gitwalk)
        end
    end
    if not set -q repo[1]
        set repo $dir
    end
    set -l clones "$repo.mysbx/clones"
    test -d "$clones"
    or return 1
    for entry in $clones/*
        test -d "$entry"
        or continue
        path basename -- $entry
    end
end

# The worktree handles of the repo the current directory resolves
# to (docs/design/worktree.md W2: the `<repo>__worktrees` sibling IS
# the registry — a pure directory listing, the same discipline as
# `__mysbx_sessions`). An entry without a `.git` FILE is debris, not
# a worktree, and is not offered — but the `list`/`diff`/`hunk`
# verbs still NAME it (a completion offers, it never forbids).
function __mysbx_worktrees
    set -l dir (pwd -P)
    set -l repo
    set -l walk $dir
    while true
        if test -d "$walk.mysbx"
            set repo $walk
            break
        end
        if test "$walk" = /
            break
        end
        set walk (dirname $walk)
    end
    if not set -q repo[1]
        set -l gitwalk $dir
        while true
            if test -e "$gitwalk/.git"
                set repo $gitwalk
                break
            end
            if test "$gitwalk" = /
                break
            end
            set gitwalk (dirname $gitwalk)
        end
    end
    if not set -q repo[1]
        set repo $dir
    end
    set -l name (path basename -- $repo)
    set -l worktrees (path dirname -- $repo)/$name\__worktrees
    test -d "$worktrees"
    or return 1
    for entry in $worktrees/*
        test -d "$entry"
        or continue
        path basename -- $entry
    end
end

# The remotes of the host repo, for `push NAME [REMOTE]` (D6: the
# host repo's own remotes; `git remote` walks up to the same repo).
function __mysbx_remotes
    command git remote 2>/dev/null
end

# Subcommands (usage.txt "Commands") — offered in the bare/global
# position only: the dispatcher takes the verb as its first positional.
# `run -- CMD` and the bare form are the two faces of one pipeline;
# `gui` re-invokes mysbx in a terminal window; the handoff verbs and
# the session group are host-side (no sandbox is started).
complete -c mysbx -f -n '__mysbx_no_verb' -a run -d 'Run one command in the sandbox: mysbx run [flags] -- CMD...'
complete -c mysbx -f -n '__mysbx_no_verb' -a gui -d 'Run mysbx ARG... in a new terminal window (needs a graphical session)'
complete -c mysbx -f -n '__mysbx_no_verb' -a init -d 'Create the sidecar directory next to the current repo (idempotent)'
complete -c mysbx -f -n '__mysbx_no_verb' -a edit -d 'Open the sidecar config.toml in $EDITOR (creates it first)'
complete -c mysbx -f -n '__mysbx_no_verb' -a fetch -d 'Fast-forward the session branch agent/mysbx/NAME into the host repo'
complete -c mysbx -f -n '__mysbx_no_verb' -a merge -d 'Merge the fetched session branch into the current branch (--no-ff default)'
complete -c mysbx -f -n '__mysbx_no_verb' -a push -d 'Push the session branch through the host repo remote (fetches it first)'
complete -c mysbx -f -n '__mysbx_no_verb' -a diff -d 'Three-dot diff of the host HEAD against the session branch'
complete -c mysbx -f -n '__mysbx_no_verb' -a version -d 'Print the version'
complete -c mysbx -f -n '__mysbx_no_verb' -a help -d 'Print this help'
complete -c mysbx -f -n '__mysbx_no_verb' -a session -d 'The session group: list | destroy | hunk'
complete -c mysbx -f -n '__mysbx_no_verb' -a worktree -d 'The worktree group: list | diff | hunk'
complete -c mysbx -f -n '__mysbx_no_verb' -a status -d 'The one-command overview: init state, effective config, sessions, worktrees'
complete -c mysbx -f -n '__mysbx_no_verb' -a gvisor-load-image -d 'Load the gVisor agent container image into Podman (--force, --test, --image <ref>)'

# The session sub-verbs (workspace.md D7 — a closed group, not an open
# tree).
complete -c mysbx -f -n '__mysbx_expects_session_verb' -a list -d 'The sessions of the current repo (the clones/ registry)'
complete -c mysbx -f -n '__mysbx_expects_session_verb' -a destroy -d 'Remove the named session clone (guarded removal)'
complete -c mysbx -f -n '__mysbx_expects_session_verb' -a hunk -d 'The session diff in the interactive hunk viewer'

# The worktree sub-verbs (docs/design/worktree.md W1 — a closed
# group, like the session one).
complete -c mysbx -f -n '__mysbx_expects_worktree_verb' -a list -d 'The host workmux worktrees (the __worktrees registry)'
complete -c mysbx -f -n '__mysbx_expects_worktree_verb' -a diff -d 'Three-dot diff of the worktree branch against its base'
complete -c mysbx -f -n '__mysbx_expects_worktree_verb' -a hunk -d 'The same range in the interactive hunk viewer'

# The global flags (cli.md D9/D10: they precede the verb; the
# run-scoped ones also follow `run`). `--result`/`--timeout` are
# `run`-only but are offered pre-verb too: the dispatcher rejects the
# misuses with a message teaching the verb — a completion cannot know
# the verb that is not typed yet, and omitting them would teach less.
complete -c mysbx -n '__mysbx_no_verb' -l dry-run -d 'Print the exact bwrap invocation and exit; creates nothing'
complete -c mysbx -n '__mysbx_no_verb' -l verbose -d 'Print the run configuration before running'
complete -c mysbx -n '__mysbx_no_verb' -l multiplexer -x -a 'tmux workmux herdr aoe orca none' -d 'The multiplexer of THIS interactive run (bare form only)'
complete -c mysbx -n '__mysbx_no_verb' -l backend -x -a 'bubblewrap podman-gvisor' -d 'The backend of THIS run (overrides the config layers for one run)'
complete -c mysbx -n '__mysbx_no_verb' -l session -x -a '(__mysbx_sessions)' -d 'Run in the named session clone; the FIRST run creates it'
complete -c mysbx -n '__mysbx_no_verb' -l ro -r -d 'Bind one more host path read-only for THIS run; repeatable'
complete -c mysbx -n '__mysbx_no_verb' -l rw -r -d 'The same, read-write; every --ro before every --rw'
complete -c mysbx -n '__mysbx_no_verb' -l result -d 'With run: record the outcome in result.json instead of exec'
complete -c mysbx -n '__mysbx_no_verb' -l timeout -x -d 'With run --result: the budget in seconds (timed-out exits 124)'
complete -c mysbx -n '__mysbx_no_verb' -s h -l help -d 'Print this help'
complete -c mysbx -n '__mysbx_no_verb' -s V -l version -d 'Print the version'

# The flags after `run` (the same words the verb also accepts before
# it, minus `--multiplexer`: a one-shot never starts a session,
# cli.md D11).
complete -c mysbx -n '__mysbx_in_run' -l dry-run -d 'Print the exact bwrap invocation and exit; creates nothing'
complete -c mysbx -n '__mysbx_in_run' -l verbose -d 'Print the run configuration before running'
complete -c mysbx -n '__mysbx_in_run' -l backend -x -a 'bubblewrap podman-gvisor' -d 'The backend of THIS run (overrides the config layers for one run)'
complete -c mysbx -n '__mysbx_in_run' -l session -x -a '(__mysbx_sessions)' -d 'Run in the named session clone; the FIRST run creates it'
complete -c mysbx -n '__mysbx_in_run' -l ro -r -d 'Bind one more host path read-only for THIS run; repeatable'
complete -c mysbx -n '__mysbx_in_run' -l rw -r -d 'The same, read-write; every --ro before every --rw'
complete -c mysbx -n '__mysbx_in_run' -l result -d 'Record the outcome in result.json instead of exec'
complete -c mysbx -n '__mysbx_in_run' -l timeout -x -d 'The --result budget in seconds (timed-out exits 124)'
complete -c mysbx -f -n '__mysbx_in_run' -a '--' -d 'Everything after is the payload, verbatim'

# `init --approve-git-dirs` (the recovery for a config created without
# approvals).
complete -c mysbx -n '__mysbx_in_init' -l approve-git-dirs -d 'Additionally approve the git metadata dirs in an EXISTING config'

# The merge strategy flags (D6: `--no-ff` is the default).
complete -c mysbx -n '__mysbx_in_merge' -l no-ff -d 'Create a merge commit even if a fast-forward is possible (the default)'
complete -c mysbx -n '__mysbx_in_merge' -l ff -d 'Allow a fast-forward, creating no merge commit'
complete -c mysbx -n '__mysbx_in_merge' -l squash -d 'Produce a single squashed commit on the target branch'
complete -c mysbx -f -n '__mysbx_merge_named' -a '--' -d 'Everything after is git-merge arguments, verbatim'

# `session destroy NAME [--force]` (D7: --force overrides only the
# unmerged-work refusal).
complete -c mysbx -n '__mysbx_in_destroy' -l force -d 'Destroy even when the session branch holds unmerged work'

# `gvisor-load-image [--force|--test|--image <ref>]` (usage.txt): the
# flags of the Podman image verb, scoped to it like `session destroy`'s
# --force is scoped to its verb.
complete -c mysbx -n '__mysbx_in_gvisor_load_image' -l force -d 'Reload the image unconditionally'
complete -c mysbx -n '__mysbx_in_gvisor_load_image' -l test -d 'Report the state without loading (exit 0 only when the loaded image is current)'
complete -c mysbx -n '__mysbx_in_gvisor_load_image' -l image -x -d 'Image reference to load (overrides $MYSBX_GVISOR_IMAGE; the pinned tarball is still loaded)'

# The NAME position of the handoff verbs and of `session destroy` /
# `session hunk`: the existing sessions of the registry (a new NAME is
# still typable — a completion offers, it never forbids).
complete -c mysbx -f -n '__mysbx_expects_name' -a '(__mysbx_sessions)' -d 'Existing session (the first --session run creates one)'

# The NAME position of `worktree diff` / `worktree hunk`: the host
# worktree handles of the `__worktrees` registry (W2).
complete -c mysbx -f -n '__mysbx_expects_worktree_name' -a '(__mysbx_worktrees)' -d 'Existing host worktree handle (workmux add creates one)'

# The REMOTE of `push NAME [REMOTE]` (default: origin).
complete -c mysbx -f -n '__mysbx_expects_remote' -a '(__mysbx_remotes)' -d 'git remote of the host repo (default: origin)'
