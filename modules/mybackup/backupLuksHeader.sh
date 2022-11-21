#!/usr/bin/env bash

# https://wiki.gentoo.org/wiki/Full_Encrypted_Btrfs/Native_System_Root_Guide#Backup_LUKS_headers

DEST=$1

help() {
    cat <<EOF
Usage:
$(basename $0) <destination path>

(needs to be ran as root)
EOF
}

if [ "$(id -u)" -ne "0" ]; then
    echo "you should run this script as root"
    echo
    help
    exit 1
fi

if [ -z "$1" ]; then
    help
    exit 1
fi

set -x

cryptsetupCmd="cryptsetup"

backupHeadersOfDisk() {
    local DISK="$1"
	  TYPE=$(blkid -o value -s TYPE $DISK)

	  if [ -n "$TYPE" ] && [ "crypto_LUKS" == "$TYPE" ]; then
		    UUID=$(blkid -o value -s UUID $DISK)
		    $cryptsetupCmd luksHeaderBackup "${DISK}" --header-backup-file "${DEST}/${UUID}.img"
	  fi
}

for DISK in $(lsblk -l -o PATH); do
    if [[ -e "$DISK" ]]; then
        backupHeadersOfDisk "$DISK"
    fi
done
