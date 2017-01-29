#!/usr/bin/env bash

# nix-env remove old generations ##########################################
type "nix-env" &> /dev/null && {
  echo "* nix-env --delete-generations 30d ..."
  sudo nix-env --delete-generations 30d
}
