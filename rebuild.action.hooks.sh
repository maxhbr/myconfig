. ./common.sh

handlePostExecutionHooks() {
    if [[ -d "$myconfigDir/misc/post_install_hooks" ]]; then
        find "$myconfigDir/misc/post_install_hooks" \
             -type f \
             -iname '*.sh' \
             -print \
             -exec {} \;
    fi
}

export -f handlePostExecutionHooks
