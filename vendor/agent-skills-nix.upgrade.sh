#!/usr/bin/env bash
set -euo pipefail

git subtree pull --prefix=vendor/agent-skills-nix https://github.com/Kyure-A/agent-skills-nix master
