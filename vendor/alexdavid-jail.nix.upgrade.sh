#!/usr/bin/env bash
set -euo pipefail

git subtree pull --prefix=vendor/alexdavid-jail.nix https://git.sr.ht/~alexdavid/jail.nix main
