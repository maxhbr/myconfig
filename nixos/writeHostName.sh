#!/usr/bin/env bash
# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e

if [ ! -d /etc/nixos ]; then
    mkdir -p /etc/nixos
fi


if [ ! -f /etc/nixos/hardware-configuration.nix ]; then
    nixos-generate-config
fi

if [ $1 ]; then
    echo -n $1 > /etc/nixos/hostname
    cksum /etc/machine-id \
        | while read c rest; do printf "%x" $c; done > /etc/nixos/hostid
else
  echo "needs hostname as argument"
  exit 1
fi
