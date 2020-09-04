#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nixops

cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")"
. ./common.sh
. ./rebuild.sh.d/rebuild.action.nixfmtall.sh
nixfmtall -print
