#!/usr/bin/env bash

set -ex
cd $(dirname "$0")
mkdir -p mail tng
mbsync -c config/mbsyncrc -a
