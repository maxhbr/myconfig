#!/usr/bin/env bash

set -e

if [ ! -f /etc/nixos/hardware-configuration.nix ]; then
    nixos-generate-config
fi

if [ $1 ]; then
    if [ ! -f "machines/$1.nix" ]; then
        echo "machine file not present"
        exit 1
    fi
    echo -n $1 > /etc/nixos/hostname
    cksum /etc/machine-id \
        | while read c rest; do printf "%x" $c; done > /etc/nixos/hostid
else
  echo "needs hostname as argument"
  exit 1
fi
