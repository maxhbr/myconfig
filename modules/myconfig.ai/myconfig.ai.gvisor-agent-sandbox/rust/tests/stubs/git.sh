#!/bin/sh
# Recording stub: git. All invocations are recorded to $RECORD/git-<n>.argv
# (NUL-separated) and answered from markers under $RECORD.
RECORD="${RECORD:?stub git: RECORD not set}"
record() {
    _tool=$1
    shift
    until mkdir "$RECORD/.seqlock" 2>/dev/null; do sleep 1; done
    _seq=0
    if [ -f "$RECORD/seq" ]; then
        # The seq file has NO trailing newline, so `read` hits EOF (and
        # fails) AFTER assigning the value; validate it instead of
        # resetting it, or every call would overwrite <tool>-1.argv.
        read -r _seq <"$RECORD/seq" || :
        case $_seq in
            '' | *[!0-9]*) _seq=0 ;;
        esac
    fi
    _seq=$((_seq + 1))
    printf '%s' "$_seq" >"$RECORD/seq"
    printf '%s\0' "$@" >"$RECORD/$_tool-$_seq.argv"
    rmdir "$RECORD/.seqlock"
}
marker_has() {
    [ -f "$1" ] || return 1
    while IFS= read -r _m; do
        [ "$_m" = "$2" ] && return 0
    done <"$1"
    return 1
}
record git "$@"
wd=
case "$1" in
    -C)
        wd=$2
        shift 2
        ;;
    --git-dir=*) shift ;;
esac
sub=$1
shift
case "$sub" in
    rev-parse)
        case "$1" in
            --is-inside-work-tree)
                if marker_has "$RECORD/not-git" "$wd"; then exit 128; fi
                exit 0
                ;;
            --show-toplevel)
                # Walk up to the topmost dir containing .git, like real
                # git does, so a start from a subdirectory anchors at the
                # repository root.
                _d=$wd
                [ -n "$_d" ] || _d=$PWD
                while :; do
                    if [ -e "$_d/.git" ]; then
                        printf '%s\n' "$_d"
                        exit 0
                    fi
                    [ "$_d" = / ] && break
                    _d=$(dirname "$_d")
                done
                exit 128
                ;;
            --verify)
                if [ -f "$RECORD/base-sha" ]; then
                    cat "$RECORD/base-sha"
                    exit 0
                fi
                if [ -f "$RECORD/base-unresolvable" ]; then
                    exit 128
                fi
                # Only HEAD resolves by default; any other ref fails like
                # real git would for a ref it does not know.
                if [ "$2" = 'HEAD^{commit}' ]; then
                    printf '%s\n' BASE
                    exit 0
                fi
                exit 128
                ;;
        esac
        exit 0
        ;;
    init)
        [ "$1" = "--bare" ] && mkdir -p "$2"
        exit 0
        ;;
    clone)
        # git clone [--origin origin --no-hardlinks] <repo> <worktree>:
        # materialize the destination directory like a real clone would.
        _dst=
        for _a in "$@"; do _dst=$_a; done
        mkdir -p "$_dst"
        exit 0
        ;;
    checkout) exit 0 ;;
    remote) exit 0 ;;
    fetch)
        [ -f "$RECORD/fetch-fail" ] && exit 1
        exit 0
        ;;
    push)
        [ -f "$RECORD/push-fail" ] && exit 3
        exit 0
        ;;
    cat-file) exit 0 ;;
    rev-list)
        # rev-list --count <ref>: the stubbed session clone has no remote
        # refs, so the default is `origin/HEAD` unresolvable (the `-
        # fallback in list); a first line in $RECORD/ahead-count answers
        # the count instead.
        if [ -f "$RECORD/ahead-count" ]; then
            read -r _n <"$RECORD/ahead-count"
            printf '%s\n' "$_n"
            exit 0
        fi
        exit 128
        ;;
    show-ref)
        [ -f "$RECORD/branch-exists" ] && exit 0
        exit 1
        ;;
    worktree)
        case "$1" in
            add)
                shift
                [ "$1" = "-b" ] && shift 2
                mkdir -p "$1"
                exit 0
                ;;
            remove)
                shift
                [ "$1" = "--force" ] && shift
                rm -rf "$1"
                exit 0
                ;;
        esac
        exit 0
        ;;
    status)
        if marker_has "$RECORD/dirty" "$wd"; then
            printf '%s\n' "M file"
        fi
        if [ -f "$RECORD/status-branch" ]; then
            printf '%s\n' "## main"
        fi
        exit 0
        ;;
    symbolic-ref)
        [ -f "$RECORD/detached" ] && exit 1
        printf '%s\n' main
        exit 0
        ;;
    merge)
        [ -f "$RECORD/merge-fail" ] && exit 1
        exit 0
        ;;
    branch) exit 0 ;;
esac
exit 0
