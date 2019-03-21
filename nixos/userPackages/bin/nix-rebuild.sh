#!/usr/bin/env bash
# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

exec nix-env --set -f "$(readlink -f "$(dirname ${BASH_SOURCE[0]})/../")" --argstr name "$(whoami)-user-env-$(date -I)"
