#!/usr/bin/env bash

set -euo pipefail

exec nix run "$(dirname "$(readlink -f "$0")")"#fmt "$@"