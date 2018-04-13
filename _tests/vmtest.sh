#!/usr/bin/env nix-shell
#! nix-shell -i bash -p expect
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# documentation: https://www.tcl.tk/man/expect5.31/expect.1.html
# run with:
#   $ ./_tests/vmtest.sh ./result/bin/run-myconfig-vm-vm

qumuCmd=$(readlink -f $1)
expectFile=$(mktemp)

cat <<SCRIPT > "$expectFile"
set timeout 150
set log [lindex $argv 0]
spawn $1

# #Login process
# expect “login: “
# #Enter username
# send “user\r”
# #Enter Password
# expect “Password: “
# send “user\r”

expect "myconfig-vm% "
send "uname -a\r"
expect -re ".*NixOS.*"

expect "myconfig-vm% "
send "sudo shutdown -h now\r"
SCRIPT

expect -f $expectFile

rm $expectFile
