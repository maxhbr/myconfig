home_git_commit() {
    local msg="$1"
    if [[ ! -z "$msg" ]]; then
        "$myconfigDir/scripts/home_git.sh" commit -am "$msg"
    fi
}

export -f home_git_commit
