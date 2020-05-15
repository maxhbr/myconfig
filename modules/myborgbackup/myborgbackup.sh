#!/usr/bin/env nix-shell
#! nix-shell -i bash -p borgbackup
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# TODO:
# - via SSH?
# - test prune

# see:
# - https://borgbackup.readthedocs.io/en/stable/
# - https://thomas-leister.de/server-backups-mit-borg/

set -e

if [ "$(id -u)" == "0" ]; then
    echo "has to be called as __non__ root!"
    exit 1
fi

help() {
    echo "run as:"
    echo "    $0 [-h] [-s] [-i|-b|-p]"
    # man borg
}

################################################################################
# parse input
usessh=0
doinit=0
dobackup=0
doborgprune=0
doborgmount=0
while getopts "h?sibpm" opt; do
    case "$opt" in
        h|\?)
            help
            exit 0
            ;;
        s) usessh=1;;
        i) doinit=1;;
        b) dobackup=1;;
        p) doborgprune=1;;
        m) doborgmount=1;;
    esac
done

if [[ $doinit -eq 0 ]] &&
       [[ $dobackup -eq 0 ]] &&
       [[ $doborgprune -eq 0 ]] &&
       [[ $doborgmount -eq 0 ]]; then
    help
    exit 0
fi

################################################################################

borgCmd="borg"

# UUID="1c0be23b-7537-4a82-8af1-744d21c3e6bd"
# UUID="3294d03a-f08b-433c-8f04-9cc2a3e9dc10"
UUID="75a54433-6d3a-46cf-bf60-bd889d99ed10"
if [[ $usessh -eq 0 ]]; then
    backupmount="/mnt/backup"
else
    backupmount="pi:/mnt/backup"
fi
backupdir="${backupmount}/borgbackup"
repository="${backupdir}/$(hostname).borg"
repositoryWork="${backupdir}/$(hostname)-work.borg"
logdir="${backupdir}/_logs"
backupprefix="$(hostname)-"
backupname="${backupprefix}$(date +%Y-%m-%d_%H:%M:%S)"
logfile="$logdir/$backupname.log"

borgInitCmd="$borgCmd \
    init \
        --encryption none"
borgCreateCmd="$borgCmd \
    create \
        --stats \
        --verbose \
        --progress \
        --filter AME \
        --show-rc \
        --one-file-system \
        --exclude-caches \
        --compression lz4"
borgPruneCmd="$borgCmd \
    prune \
        --stats \
        --verbose \
        --list \
        --show-rc \
        --keep-within=2d --keep-daily=7 --keep-weekly=4 --keep-monthly=6 \
        --prefix \"backupprefix\""

################################################################################
# functions
have() { type "$1" &> /dev/null; }

myMountBackupHDD() {
    set -x
    echo "0 0 0" | sudo tee /sys/class/scsi_host/host*/scan || true
    sleep 1
    sudo mkdir -p $backupmount
    sudo umount /dev/disk/by-uuid/$UUID || true
    sudo mount /dev/disk/by-uuid/$UUID $backupmount
    sudo mkdir -p "$backupdir"
    sudo chown -c $USER "$backupdir"
    mkdir -p "$logdir"
    set +x
}

myUmountBackupHDD() {
    set -x
    sync
    sudo umount "$backupmount" || echo "umount failed... continue"
    # sudo hdparm -Y /dev/disk/by-uuid/$UUID
    set +x
}

myInitialize() {
    echo "initialize the repository"
    set -x
    $borgInitCmd "$repository"
    [[ -d ~/TNG ]] && $borgInitCmd "$repositoryWork"
    set +x
}

myBackup() {
    echo "do the backup"
    T=$(mktemp -d)
    set -x
    cat <<EXCLUDE > "${T}/_excludes"
/home/docker
/home/*/tmp/
/home/*/Downloads/
/home/*/TNG/
/home/*/.cache/
/home/*/Bilder/workspace/
/home/*/Desktop/games/
/home/*/.local/share/Steam/
/home/*/.wine/
/home/*/.cache/
/home/*/.thumbnails/
/home/*/.nox/
/home/*/.m2/
/home/*/.compose-cache/
/home/*/.config/GIMP/*/backups
*/.Trash*/
/nix/
/tmp/
/var/lib/docker/
/var/cache/
/var/tmp/
/home/*/VirtualBox VMs/
/home/*/Desktop/
EXCLUDE
    cat <<EXCLUDE > "${T}/_tng.excludes"
*/PIP/_*
EXCLUDE
    $borgCreateCmd \
         --exclude-from "${T}/_excludes" \
         "${repository}::${backupname}" \
         /home/mhuber/
    [[ -d ~/TNG ]] && \
        $borgCreateCmd \
            --exclude-from "${T}/_tng.excludes" \
            "${repositoryWork}::${backupname}" \
            /home/mhuber/TNG/
    set +x
    rm -r "$T"
}

myBorgprune() {
    echo "do the borg prune"
    set -x
    $borgPruneCmd "$repository"
    [[ -d ~/TNG ]] && \
        $borgPruneCmd "$repositoryWork"
    set +x
}

myBorgmount() {
    echo "do the borg mount"
    set -x
    mkdir -p "${backupdir}/mnt"
    $borgCmd mount -f "$repository" "${backupdir}/mnt"
    mkdir -p "${backupdir}/mnt-work"
    $borgCmd mount -f "$repositoryWork" "${backupdir}/mnt-work"
    set +x
}

################################################################################
# prepare and mount
if [[ $usessh -eq 0 ]]; then
    if [[ ! -d $backupdir ]]; then
        myMountBackupHDD
    fi
fi

################################################################################
# setup log
if [[ $usessh -eq 0 ]]; then
    # TODO: logging for ssh
    mkdir -p "$logdir"
    echo -e "\n\n\n\n\n\n\n" >> $logfile
    exec &> >(tee -a $logfile)
fi

################################################################################
# run
if [[ $doinit -eq 1 ]]; then
    myInitialize
fi

if [[ $dobackup -eq 1 ]]; then
    myBackup
fi

if [[ $doborgprune -eq 1 ]]; then
    myBorgprune
fi

if [[ $doborgmount -eq 1 ]]; then
    myBorgmount
fi

################################################################################
# cleanup
if [[ $usessh -eq 0 ]]; then
    myUmountBackupHDD
fi

