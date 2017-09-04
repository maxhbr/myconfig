#!/usr/bin/env bash

awk -F"[][]" '/dB/ { print $2 }' <(amixer sset $@)
