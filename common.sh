# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
if ! type logH1 &>/dev/null; then
    ###########################################################################
    ##  variables  ############################################################
    ###########################################################################
    export my_main_host='x1extremeG2'

    if [ -n "$BASH_VERSION" ]; then
        common_sh="${BASH_SOURCE[0]}"
    elif [ -n "$ZSH_VERSION" ]; then
        common_sh="${(%):-%N}"
    fi
    export myconfigDir="$(readlink -f $(dirname "$common_sh"))"
    export logsDir="${myconfigDir}/_logs/"
    mkdir -p "$logsDir"

    export nixpkgs="$myconfigDir/nixpkgs"

    nixosConfig="$myconfigDir/hosts/$(hostname)" # Why is this necessary?
    if [[ ! -d "$nixosConfig" ]]; then
        nixosConfig="$myconfigDir/nixos/host-minimal"
    fi

    NIX_PATH="nixpkgs=$nixpkgs:nixos-config=$nixosConfig"
    NIX_PATH_ARGS="-I '$(echo "$NIX_PATH" | sed "s/:/' -I '/g")'"
    export NIX_PATH
    export NIX_PATH_ARGS

    ###########################################################################
    ##  functions  ############################################################
    ###########################################################################

    have() { type "$1" &>/dev/null; }

    logH1() {
        local prefix=$1
        local text=$2
        >&2 echo
        >&2 echo "$(tput bold)****************************************************************************"
        >&2 echo "***$(tput sgr0) $prefix $(tput bold)$text$(tput sgr0)"
    }

    logH2() {
        local prefix=$1
        local text=$2
        >&2 echo "$(tput bold)***$(tput sgr0) $prefix $(tput bold)$text$(tput sgr0)"
    }

    logH3() {
        local prefix=$1
        local text=$2
        >&2 echo "*** $prefix $(tput bold)$text$(tput sgr0)"
    }

    logINFO() {
        local text=$1
        >&2 echo "$(tput setaf 6)$(tput bold)*** INFO: $text$(tput sgr0)"
    }

    logWARN() {
        local text=$1
        >&2 echo "$(tput setaf 3)$(tput bold)*** WARN: $text$(tput sgr0)"
    }

    logERR() {
        local text=$1
        >&2 echo "$(tput setaf 1)$(tput bold)*** ERR: $text$(tput sgr0)"
    }

    logDEBUG() {
        local text=$1
        >&2 echo "$(tput bold)*** DEBUG:$(tput sgr0) $text"
    }

    if [ -n "$BASH_VERSION" ]; then
        export -f have
        export -f logH1
        export -f logH2
        export -f logH3
        export -f logINFO
        export -f logERR
    elif [ -n "$ZSH_VERSION" ]; then
        export have
        export logH1
        export logH2
        export logH3
        export logINFO
        export logERR
    fi

    updateRefAndJson() {
        local repo="$1"
        local repoName=$(basename "$repo")
        local repoUser=$(dirname "$repo")
        local branch=${2:-master}
        local outBN=${3:-$repoName}
        local outRev=${outBN}.rev
        local outJson=${outBN}.json

        logH3 "update $outRev from $repo on branch" "$branch"
        local rev="$(nix-shell -p curl --command "curl -s \"https://api.github.com/repos/$repo/commits/${branch}\"" | nix-shell -p jq --command "jq -r '.sha'")"
        if [[ "$rev" != "null" ]]; then
            if ! grep -q $rev "$outRev" 2>/dev/null; then
                echo $rev > "$outRev"

                local url="https://github.com/$repo"
                local tarball="https://github.com/$repo/archive/${rev}.tar.gz"
                prefetchOutput=$(nix-prefetch-url --unpack --print-path --type sha256 $tarball)
                local hash=$(echo "$prefetchOutput" | head -1)
                local path=$(echo "$prefetchOutput" | tail -1)
                echo '{"url":"'$tarball'","rev": "'$rev'","sha256":"'$hash'","path":"'$path'","ref": "'$branch'", "url": "'$url'", "owner": "'$repoUser'", "repo": "'$repoName'"}' > "./$outJson"
                logINFO "... $(tput setaf 2)$(tput bold)updated$(tput sgr0) $outRev to rev=[$rev]"

                return 1 # was updated
            else
                logINFO "... $outRev file is already up to date, at rev=[$rev]"
            fi
        else
            logWARN "... failed to update $repo, potentially hit rate limiting of github"
        fi
    }

    updateShaFromURL() {
        local url="$1"
        local basename=$(basename "$url")
        local outJson=${2:-$basename}.json

        if curl --output /dev/null --silent --head --fail "$url"; then
            sha256="$(nix-prefetch-url --type sha256 "$url")"
            echo "{ \"sha256\": \"$sha256\", \"url\": \"$url\" }" | tee "$outJson"
        else
            echo "url=[$url] does not exist"
        fi
    }

    checkIfConnected() {
        if ! ping -c1 heise.de > /dev/null 2>&1; then
            logERR "not connected: ping heise.de failed"
            if ! wget -O - heise.de > /dev/null 2>&1; then
                logERR "not connected: wget heise.de failed"
                exit 1
            fi
        fi
    }

    if [ -n "$BASH_VERSION" ]; then
        export -f updateRefAndJson
        export -f updateShaFromURL
        export -f checkIfConnected
    elif [ -n "$ZSH_VERSION" ]; then
        export updateRefAndJson
        export updateShaFromURL
        export checkIfConnected
    fi
fi

