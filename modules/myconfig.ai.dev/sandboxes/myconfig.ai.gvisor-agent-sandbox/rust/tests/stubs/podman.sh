#!/bin/sh
# Recording stub: podman. Global args (--runtime=*, --cgroup-manager=*,
# --runtime-flag=*) are skipped, everything else dispatched. Container state
# lives in $RECORD/containers/<name>/: existence = the directory, running
# state = contents of the "running" file. The recorded argv includes the
# leading "podman" (argv[0]), matching the `last-command` the CLI writes.
RECORD="${RECORD:?stub podman: RECORD not set}"
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
record podman podman "$@"
sub=
while [ $# -gt 0 ]; do
    case $1 in
        --runtime=* | --cgroup-manager=* | --runtime-flag=*) shift ;;
        *)
            sub=$1
            shift
            break
            ;;
    esac
done
case "$sub" in
    info)
        [ -f "$RECORD/info-fail" ] && exit 125
        exit 0
        ;;
    image)
        [ "$1" = exists ] || exit 0
        [ -f "$RECORD/image-missing" ] && exit 1
        exit 0
        ;;
    container)
        [ "$1" = exists ] || exit 0
        [ -d "$RECORD/containers/$2" ] && exit 0
        exit 1
        ;;
    inspect)
        fmt=$2
        cname=$3
        case $fmt in
            *Running*)
                if [ -f "$RECORD/containers/$cname/running" ]; then
                    cat "$RECORD/containers/$cname/running"
                else
                    printf '%s\n' false
                fi
                ;;
            status:*)
                # podman does not interpret literal \n in --format: one line,
                # and podman appends a trailing newline after the output.
                # (This case must precede *State.Status*: the cmd_status
                # format contains {{.State.Status}} as well.)
                _st=stopped
                [ "$(cat "$RECORD/containers/$cname/running" 2>/dev/null)" = true ] && _st=running
                printf 'status:    %s\\npid:       %s\\nstarted:   %s\n' "$_st" 42 2025-01-01T00:00:00Z
                ;;
            *State.Status*)
                if [ "$(cat "$RECORD/containers/$cname/running" 2>/dev/null)" = true ]; then
                    printf '%s\n' running
                else
                    printf '%s\n' stopped
                fi
                ;;
            *) exit 0 ;;
        esac
        exit 0
        ;;
    run)
        name=
        while [ $# -gt 0 ]; do
            case $1 in
                --name)
                    name=$2
                    shift 2
                    ;;
                *) shift ;;
            esac
        done
        if [ -n "$name" ] && [ -z "$PODMAN_STUB_NO_REGISTER" ]; then
            mkdir -p "$RECORD/containers/$name"
            printf '%s\n' true >"$RECORD/containers/$name/running"
        fi
        [ -f "$RECORD/run-fail" ] && exit 125
        exit 0
        ;;
    stop)
        _last=
        for _a in "$@"; do _last=$_a; done
        if [ -n "$_last" ] && [ -d "$RECORD/containers/$_last" ]; then
            printf '%s\n' false >"$RECORD/containers/$_last/running"
        fi
        exit 0
        ;;
    rm)
        _last=
        for _a in "$@"; do _last=$_a; done
        rm -rf "$RECORD/containers/$_last"
        exit 0
        ;;
esac
exit 0
