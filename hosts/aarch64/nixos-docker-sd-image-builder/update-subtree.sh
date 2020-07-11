#!/usr/bin/env bash

cd ../../..
git subtree pull \
    --prefix=hosts/aarch64/nixos-docker-sd-image-builder \
    https://github.com/Robertof/nixos-docker-sd-image-builder master
