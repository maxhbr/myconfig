###########################################################################
##  variables  ############################################################
###########################################################################

# nixStableChannel=nixos-19.03-small
nixStableChannel=nixos-19.03

export myconfigDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
export nixConfigDir="$myconfigDir/nix"
export nixpkgsDir="$nixConfigDir/nixpkgs"
export nixpkgsUnstableDir="$nixConfigDir/nixpkgs-unstable"
export overlaysDir="$nixConfigDir/overlays"
export nixosConfigDir="$myconfigDir/nixos"
if [[ -f "$nixpkgsDir/default.nix" ]]; then
    nixpkgs="$nixpkgsDir"
else
    nixpkgs="channel:$nixStableChannel"
fi

NIX_PATH="nixpkgs=$nixpkgs:nixpkgs-overlays=$overlaysDir:nixos-config=$nixosConfigDir:myconfig=$myconfigDir"
NIX_PATH_ARGS="-I nixpkgs=$nixpkgs -I nixpkgs-overlays=$overlaysDir -I nixos-config=$nixosConfigDir -I myconfig=$myconfigDir"
export NIX_PATH
export NIX_PATH_ARGS

###########################################################################
##  function  #############################################################
###########################################################################

have() { type "$1" &> /dev/null; }

logH1() {
    local prefix=$1
    local text=$2
    echo
    echo "$(tput bold)****************************************************************************"
    echo "***$(tput sgr0) $prefix $(tput bold)$text$(tput sgr0)"
}

logH2() {
    local prefix=$1
    local text=$2
    echo "$(tput bold)***$(tput sgr0) $prefix $(tput bold)$text$(tput sgr0)"
}

logH3() {
    local prefix=$1
    local text=$2
    echo "*** $prefix $(tput bold)$text$(tput sgr0)"
}

logINFO() {
    local text=$1
    echo "$(tput setaf 6)$(tput bold)*** INFO: $text$(tput sgr0)"
}

logWARN() {
    local text=$1
    echo "$(tput setaf 3)$(tput bold)*** WARN: $text$(tput sgr0)"
}

logERR() {
    local text=$1
    echo "$(tput setaf 1)$(tput bold)*** ERR: $text$(tput sgr0)"
}

export -f have
export -f logH1
export -f logH2
export -f logH3
export -f logINFO
export -f logERR
