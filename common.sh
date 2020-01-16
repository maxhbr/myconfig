###########################################################################
##  variables  ############################################################
###########################################################################

# export nixStableChannel=nixos-unstable
# export nixStableChannel=nixos-19.09-small
export nixStableChannel=nixos-19.09

export COMMON_SH_WAS_SOURCED="true"

export my_main_host='x1extremeG2'
export my_user='mhuber'

export myconfigDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
export nixpkgsDir="$myconfigDir/nixpkgs"
if [[ -f "$nixpkgsDir/default.nix" ]]; then
    nixpkgs="$nixpkgsDir"
else
    nixpkgs="channel:$nixStableChannel"
fi

NIX_PATH="nixpkgs=$nixpkgs:nixos-config=$myconfigDir/default.nix:myuser=$my_user"
NIX_PATH_ARGS="-I nixpkgs=$nixpkgs -I nixos-config=$myconfigDir/default.nix -I myuser=$my_user"
export NIX_PATH
export NIX_PATH_ARGS

###########################################################################
##  functions  ############################################################
###########################################################################

have() { type "$1" &> /dev/null; }

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

export -f have
export -f logH1
export -f logH2
export -f logH3
export -f logINFO
export -f logERR

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

    if ! grep -q $rev "$outRev" 2>/dev/null; then
        echo $rev > "$outRev"

        local url="https://github.com/$repo"
        local tarball="https://github.com/$repo/archive/${rev}.tar.gz"
        prefetchOutput=$(nix-prefetch-url --unpack --print-path --type sha256 $tarball)
        local hash=$(echo "$prefetchOutput" | head -1)
        local path=$(echo "$prefetchOutput" | tail -1)
        echo '{"url":"'$tarball'","rev": "'$rev'","sha256":"'$hash'","path":"'$path'","ref": "'$branch'", "url": "'$url'", "owner": "'$repoUser'", "repo": "'$repoName'"}' > "./$outJson"
        logINFO "... updated $outRev to rev=[$rev]"
    else
        logINFO "... $outRev file is already up to date, at rev=[$rev]"
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

export -f updateRefAndJson
export -f updateShaFromURL
