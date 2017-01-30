#!/usr/bin/env bash
set -e

# nix-env remove old generations ##########################################
type "nix-env" &> /dev/null && {
    nix-channel --update
    nix-env --upgrade
}
