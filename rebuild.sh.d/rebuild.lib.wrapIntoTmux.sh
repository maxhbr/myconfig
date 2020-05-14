. ./common.sh

wrapIntoTmux() {
    local script="$1"
    shift
    have "tmux" && {
        local TMUX_NAME="rebuild_sh"
        if test -z $TMUX && [[ $TERM != "screen" ]]; then
            logH2 "wrap into tmux ..."
            tmux has-session -t $TMUX_NAME 2>/dev/null && {
                logERR "already running somewhere"
                exit 1
            }
            tmux -2 new-session -s $TMUX_NAME \
                 "command echo \"... wrapped into tmux\"; NIX_PATH=\"$NIX_PATH\" $script $@; read -t 1 -n 10000 discard; read -n 1 -s -r -p \"Press any key to continue\"" \; \
                 set-option status-left "rebuild.sh"\; \
                 set-option status-right "started at $(date) "\; \
                 set set-titles-string "${TMUX_NAME}@tmux" \
                && exit 0
            logERR "tmux failed to start, running without tmux"
        fi
    } || logINFO "tmux not installed"
}

export -f wrapIntoTmux
