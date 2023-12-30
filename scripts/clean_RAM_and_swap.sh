#!/usr/bin/env bash
set -euo pipefail

sync
echo 1 | sudo tee /proc/sys/vm/drop_caches
sudo swapoff -a

if [[ $# -gt 0 && "$1" == "--no-swap" ]]; then
  echo "do not enable swap again"
else
  echo "wait 30s and then enable swap again (pass --no-swap to skip that)"
  sleep 30
  sudo swapon -a
fi
