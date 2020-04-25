#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nixops jq
# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e
cd "$( dirname "${BASH_SOURCE[0]}" )"
common="./common.sh"; until [ -f "$common" ]; do common="./.${common}"; done
. "$common"

deployment=vserver-nixops

nixops info -d $deployment || nixops create -d $deployment ./vserver-nixops.nix

nixops deploy \
       -d $deployment \
       --force-reboot

nixops check -d $deployment

nixops ssh-for-each -d vserver-nixops upg-pull

