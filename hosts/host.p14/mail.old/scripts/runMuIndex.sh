#!/usr/bin/env bash

set -ex
cd $(dirname "$0")
pkill -2 -u $UID mu
sleep 1
mu index #--pull
