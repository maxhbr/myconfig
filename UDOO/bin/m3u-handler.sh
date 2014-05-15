#!/usr/bin/env sh
mpc clear
cat $1 | sed -e '/^#/D' | mpc add
mpc play
