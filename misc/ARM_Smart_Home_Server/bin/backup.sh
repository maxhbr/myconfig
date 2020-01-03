#!/usr/bin/env bash

out="$HOME/opt_backup_$(date +"%m-%d-%y").tar.gz"
sudo tar -zcvf "$out" "/opt" "/etc/wireguard"
sudo chown "$(id -u $USER)":"$(id -g $USER)" "$out"
du -h "$out"
