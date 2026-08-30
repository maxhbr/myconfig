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
        read -r _seq <"$RECORD/seq" || _seq=0
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
    remote) exit 0 ;;
    fetch) exit 0 ;;
    cat-file) exit 0 ;;
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
