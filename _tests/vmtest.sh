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
set timeout -1
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

# expect "myconfig-vm% "
# send "git clone https://github.com/maxhbr/myconfig ~/myconfig\r"
# expect "myconfig-vm% "
# send "echo -n HOSTNAME | sudo tee /etc/nixos/hostname\r"
# expect "myconfig-vm% "
# send "cksum /etc/machine-id | while read c rest; do printf \"%x\" $c; done | sudo tee /etc/nixos/hostid\r"
# expect "myconfig-vm% "
# send "~/myconfig/dotfiles/default.sh\r"
# expect "myconfig-vm% "
# send "~/myconfig/xmonad/default.sh\r"


expect "myconfig-vm% "
send "sudo shutdown -h now\r"
SCRIPT

expect -f $expectFile

rm $expectFile
