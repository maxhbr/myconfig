. ./common.sh

isBranchMaster() {
    if [[ "$(cd "$myconfigDir"; git rev-parse --abbrev-ref HEAD)" == "master" ]]; then
        return 0
    else
        return 1
    fi
}

handleGit() {
    if isBranchMaster; then
        logH1 "update config" ""
        if git diff-index --quiet HEAD --; then
            git fetch
            local UPSTREAM='@{u}'
            local LOCAL=$(git rev-parse @)
            local REMOTE=$(git rev-parse "$UPSTREAM")
            local BASE=$(git merge-base @ "$UPSTREAM")
            if [ "x$LOCAL" == "x$REMOTE" ]; then
                echo "... up-to-date"
            elif [ "x$LOCAL" == "x$BASE" ]; then
                echo "* pull ..."
                git pull --rebase || true
                logH1 "run" "updatet version of script"
                exec $0
            elif [ "x$REMOTE" == "x$BASE" ]; then
                logINFO "need to push"
            else
                logERR "diverged"
            fi
        else
            logINFO "git directory is unclean"
            if [[ "$1" != "--fast" ]]; then
                read -t 10 -r -p "commit state as \"update before rebuild.sh\"? [y/N] " response || true
                if [[ "${response,,}" =~ ^(yes|y)$ ]]; then
                    git commit -am "update before rebuild.sh"
                    handleGit
                fi
            fi
        fi
    else
        logINFO "git branch is not master, do not handle git"
    fi
}

handleGitPostExecution() {
    if isBranchMaster; then
        git diff --stat
        if ! git diff-index --quiet HEAD --; then
            read -r -p "commit state as \"update after rebuild.sh\"? [y/N] " response
            if [[ "${response,,}" =~ ^(yes|y)$ ]]; then
                git commit -am "update after rebuild.sh"
            fi
        fi
    fi
}

export -f isBranchMaster
export -f handleGit
export -f handleGitPostExecution
