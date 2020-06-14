#!/usr/bin/env bash
# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
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
    echo "    $0 [-h] [-t TARGET] [-i|-b|-p]"
}

homedir="$HOME/.myborgbackup"
mkdir -p "$homedir"
passphraseFile="${homedir}/borg-passphrase"

################################################################################
# parse input
target=""
targetWasMounted=false
usessh=false
doinit=false
dobackup=false
doborgprune=false
doborgmount=false
encryption=none
while getopts "h?ibpmet:" opt; do
    case "$opt" in
        h|\?)
            help
            exit 0
            ;;
        t) target="$OPTARG";;
        i) doinit=true;;
        b) dobackup=true;;
        p) doborgprune=true;;
        m) doborgmount=true;;
        e) encryption=repokey; export BORG_PASSCOMMAND="cat $passphraseFile" ;;
    esac
done

if $doinit &&
       $dobackup &&
       $doborgprune &&
       $doborgmount; then
    help
    exit 0
fi

if [[ -z "$target" ]]; then
    echo "target has to be set via -t TARGET"
    exit 1
elif [[ $target == *":"* ]]; then
    usessh=true
else
    if [[ $target == "/dev/"* ]]; then
        set -x
        echo "0 0 0" | sudo tee /sys/class/scsi_host/host*/scan || true
        sleep 1
        sudo mkdir -p "/mnt/backup"
        sudo umount $target || true
        sudo mount $target "/mnt/backup"
        set +x
        target="/mnt/backup"
        targetWasMounted=true
    fi

    set -x
    if [[ "$(df --output=source / | tail -1)" == "$(df --output=source "$target" | tail -1)" ]]; then
        echo "same filesystem"
        exit 3
    fi
fi

################################################################################

borgCmd="borg"

backupdir="${target}/borgbackup"
repository="${backupdir}/$(hostname).borg"
repositoryWork="${backupdir}/$(hostname)-work.borg"
logdir="${homedir}/_logs"
backupprefix="$(hostname)-"
backupname="${backupprefix}$(date +%Y-%m-%d_%H:%M:%S)"
logfile="$logdir/$backupname.log"

if [[ $usessh != true ]]; then
    sudo mkdir -p "$backupdir"
    sudo chown -c $USER "$backupdir"
fi

################################################################################

myKeyExport() {
    local repository="$1"
    local target="$2"
    local borgKeyExportCmd="$borgCmd \
            key export"

    $borgKeyExportCmd "$repository" "$target.key"
    $borgKeyExportCmd --paper "$repository" "$target.key.paper"
    $borgKeyExportCmd --qr-html "$repository" "$target.key.html"
}

myInitialize() {
    local borgInitCmd="$borgCmd \
            init \
                --encryption $encryption"
    echo "initialize the repository"
    set -x

    if [[ "$encryption" != "none" ]]; then
        if [[ ! -f "$passphraseFile" ]]; then
            head -c 32 /dev/urandom | base64 -w 0 > "$passphraseFile"
            chmod 400 "$passphraseFile"
        fi
    fi

    $borgInitCmd "$repository"
    [[ -d ~/TNG ]] && $borgInitCmd "$repositoryWork"

    if [[ "$encryption" != "none" ]]; then
        myKeyExport "$repository" "$HOME/.myborgbackup/priv"
        [[ -d ~/TNG ]] && myKeyExport "$repositoryWork"  "$HOME/.myborgbackup/work"
    fi

    set +x
}

myBackup() {
    local borgCreateCmd="$borgCmd \
            create \
                --stats \
                --verbose \
                --progress \
                --filter AME \
                --show-rc \
                --one-file-system \
                --exclude-caches \
                --compression lz4"

    echo "do the backup"
    set -x
    cat <<EXCLUDE > "${homedir}/_excludes"
*/.Trash*/
*/.cache/
*/.compose-cache/
*/.config/GIMP/*/backups
*/.local/share/
*/.local/share/Steam/
*/.m2/
*/.nox/
*/.thumbnails/
*/.wine/
*/Bilder/workspace/
*/Desktop/
*/Desktop/games/
*/Downloads/
*/TNG/
*/VirtualBox VMs/
*/tmp/
*.pyc
*.ARW
*.iso
*.img
*.ova
EXCLUDE
    cat <<EXCLUDE > "${homedir}/_tng.excludes"
*/PIP/_*
EXCLUDE
    $borgCreateCmd \
            --exclude-from "${homedir}/_excludes" \
         "${repository}::${backupname}" \
         /home/mhuber/
    [[ -d ~/TNG ]] && \
        $borgCreateCmd \
            --exclude-from "${homedir}/_tng.excludes" \
            "${repositoryWork}::${backupname}" \
            /home/mhuber/TNG/
    set +x
}

myBorgprune() {
    local borgPruneCmd="$borgCmd \
            prune \
                --stats \
                --verbose \
                --list \
                --show-rc \
                --keep-within=2d --keep-daily=7 --keep-weekly=4 --keep-monthly=6 \
                --prefix \"backupprefix\""
    echo "do the borg prune"
    set -x
    $borgPruneCmd "$repository"
    [[ -d ~/TNG ]] && \
        $borgPruneCmd "$repositoryWork"
    set +x
}

################################################################################
# setup log
mkdir -p "$logdir"
echo -e "\n\n\n\n\n\n\n" >> $logfile
exec &> >(tee -a $logfile)

################################################################################
# run
if $doinit; then
    myInitialize
fi

if $dobackup; then
    myBackup
fi

if $doborgprune; then
    myBorgprune
fi

if $doborgmount; then
    myBorgmount
fi

################################################################################
if $targetWasMounted; then
    sudo umount "$target" || echo "umount failed... continue"
fi

