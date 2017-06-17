#!/usr/bin/env bash

type "nix-env" &> /dev/null && {
  echo "* nix-env --delete-generations 30d ..."
  nix-env --delete-generations 30d
  sudo nix-env --delete-generations 30d
}
