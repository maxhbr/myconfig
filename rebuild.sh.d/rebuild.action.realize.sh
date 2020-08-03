. ./common.sh

getDeploymentNameFromHostname() {
    local hostname="$1"
    echo "myconfig-${hostname}"
}
getDeploymentFileFromHostname() {
    local hostname="$1"
    echo "$myconfigDir/hosts/${hostname}/nixops.nix"
}

setupNixopsDeployment() {
    local hostname="$1"; shift
    local FORCE_RECREATE=false
    if [[ "$1" == "--force-recreate" ]]; then
        FORCE_RECREATE=true
        shift
    fi

    local nixopsDeployment="$(getDeploymentNameFromHostname "$hostname")"
    local nixopsDeploymentFile="$(getDeploymentFileFromHostname "$hostname")"

    logH1 "setup nixops" "hostname=$hostname nixopsDeployment=$nixopsDeployment"

    if nixops list --deployment "$nixopsDeployment" | grep -q "$nixopsDeployment"; then
        if $FORCE_RECREATE; then
            nixops delete -d "$nixopsDeployment" --force
            nixops create -d "$nixopsDeployment" "$nixopsDeploymentFile"
        else
            nixops check -d "$nixopsDeployment" || true
        fi
    else
        nixops create -d "$nixopsDeployment" "$nixopsDeploymentFile"
    fi
}

generateDiffWithOld() {
    local newFile=$1
    local defaultSdiffArgs="-bBWs"
    local oldFile="${newFile}.old"
    if [[ -f "$oldFile" ]]; then
        (set +e; sdiff "${2:-$defaultSdiffArgs}" $oldFile $newFile |
             while read; do
                 local line="$REPLY"
                 case $line in
                     *'<'* ) echo "$(tput setaf 1)$line$(tput sgr0)" ;;
                     *'>'* ) echo "$(tput setaf 2)$line$(tput sgr0)" ;;
                     *'|'* ) echo "$(tput setaf 3)$line$(tput sgr0)" ;;
                     *)      echo "$line" ;;
                 esac
             done)
        rm "$oldFile"
    fi
    mv $newFile $oldFile
}

runOnHost() {
    local targetHost="$1"
    local cmd="$2"
    local nixopsDeployment="$(getDeploymentNameFromHostname "$targetHost")"
    nixops ssh \
           --deployment "$nixopsDeployment" \
           "$targetHost" \
           -- "$cmd"
}

generateStats() {
    local targetHost="$1"

    { local newFile="$logsDir/currentGenerations-${targetHost}";
      runOnHost "$targetHost" "nix-env --list-generations --profile /nix/var/nix/profiles/system" \
                > "$newFile";
      generateDiffWithOld "$newFile"; } || true

    { local newFile="$logsDir/currentSystemDeps-${targetHost}-current-system";
      runOnHost "$targetHost" "nix-store -qR /run/current-system/" |
          sed 's/^[^-]*-//g' |
          sort -u \
               > "$newFile";
      generateDiffWithOld "$newFile"; } || true

    { local newFile="$logsDir/currentSystemDeps-${targetHost}-.nix-profile";
      runOnHost "$targetHost" "nix-store -qR /home/mhuber/.nix-profile" |
          sed 's/^[^-]*-//g' |
          sort -u \
               > "$newFile";
      generateDiffWithOld "$newFile"; } || true

    { local newFile="$logsDir/currentDiskUsage-${targetHost}";
      runOnHost "$targetHost" "df -h --output=\"pcent,used\" /" \
                > "$newFile";
      generateDiffWithOld "$newFile" "-bBW"; } || true

    { local newFile="$logsDir/currentSystemDerivationDiskUsage-${targetHost}";
      runOnHost "$targetHost" "nix path-info -hS /run/current-system/" \
                > "$newFile";
      generateDiffWithOld "$newFile"; } || true
}

realize() {
    local targetHost="$1"; shift
    local FORCE_RECREATE=false
    local IS_LOCAL_HOST=false
    local DRY_RUN=false
    if [[ "$1" == "--force-recreate" ]]; then
        FORCE_RECREATE=true
        shift
    fi
    if [[ "$1" == "--is-local-host" ]]; then
        IS_LOCAL_HOST=true
        shift
    fi
    if [[ "$1" == "--dry-run" ]]; then
        DRY_RUN=true
        # don't do shift
    fi

    local nixopsDeployment="$(getDeploymentNameFromHostname "$targetHost")"


    setupNixopsDeployment "$targetHost" $($FORCE_RECREATE && echo "--force-recreate")

    ############################################################################
    # dirty fix due to driver incompatibilities:
    if [[ "$targetHost" == "workstation" ]]; then

        cat <<EOF
TODO: workstation should use same nixpkgs as other hosts
      Problem: mesa in unstable does not work in some games
EOF

        export nixpkgs="https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz"
        # export nixpkgs="https://github.com/NixOS/nixpkgs/archive/nixos-unstable-small.tar.gz"
        export NIX_PATH="nixpkgs=$nixpkgs:nixos-config=$nixosConfig"
        export NIX_PATH_ARGS="-I nixpkgs=$nixpkgs -I nixos-config=$nixosConfig"
    fi
    ############################################################################

    logH1 "deploy" "targetHost=$targetHost args=$args jobCountArgs=$jobCountArgs"
    logINFO "FORCE_RECREATE=$FORCE_RECREATE IS_LOCAL_HOST=$IS_LOCAL_HOST DRY_RUN=$DRY_RUN"
    if $IS_LOCAL_HOST; then
        if ! $DRY_RUN; then
            local mimeapps_list="$HOME/.config/mimeapps.list"
            if [[ -f "$mimeapps_list" && ! -L "$mimeapps_list" ]]; then
                logINFO "remove $mimeapps_list"
                (set -x;
                 rm "$mimeapps_list")
            fi
        fi
    fi
    (set -x;
     nixops deploy \
            $NIX_PATH_ARGS \
            --show-trace --keep-failed \
            --fallback \
            --deployment "$nixopsDeployment" \
            "$@" \
            --include "$targetHost")
}

export -f runOnHost
export -f realize
export -f generateStats
