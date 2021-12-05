#!/usr/bin/env bash
set -euo pipefail

sync
echo 1 | sudo tee /proc/sys/vm/drop_caches
sudo swapoff -a
sleep 30
sudo swapon -a
