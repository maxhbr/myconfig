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
    local hostname="$1"
    local nixopsDeployment="$(getDeploymentNameFromHostname "$hostname")"

    logH1 "setup nixops" "hostname=$hostname nixopsDeployment=$nixopsDeployment"

    if nixops list --deployment "$nixopsDeployment" | grep -q "$nixopsDeployment"; then
        nixops check -d "$nixopsDeployment" || true
    else
        nixops create -d "$nixopsDeployment" "$(getDeploymentFileFromHostname "$hostname")"
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
    local targetHost="$1"
    shift
    local nixopsDeployment="$(getDeploymentNameFromHostname "$targetHost")"

    setupNixopsDeployment "$targetHost"

    ############################################################################
    # dirty fix due to driver incompatibilities:
    if [[ "$targetHost" == "workstation" ]]; then

        cat <<EOF
TODO: workstation should use same nixpkgs as other hosts
      Problem: mesa in unstable does not work in some games
EOF

        export nixpkgs="https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz"
        export NIX_PATH="nixpkgs=$nixpkgs:nixos-config=$nixosConfig"
        export NIX_PATH_ARGS="-I nixpkgs=$nixpkgs -I nixos-config=$nixosConfig"
    fi
    ############################################################################

    logH1 "deploy" "targetHost=$targetHost args=$args jobCountArgs=$jobCountArgs"
    (set -x;
     nixops deploy \
            $NIX_PATH_ARGS \
            --show-trace --keep-failed \
            --fallback \
            --deployment "$nixopsDeployment" \
            "$@" \
            --include "$targetHost")
}

export -f realize
export -f generateStats
