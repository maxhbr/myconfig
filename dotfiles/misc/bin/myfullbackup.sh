#!/bin/sh
#
# Written by Maximilian-Huber.de
#
# initially create $STATUSFILE with the content "0"
#   $ echo "0" > $STATUSFILE
#
# Last modified: So Sep 27, 2015  08:48
#
set -ex

if [ "$(id -u)" != "0" ]; then
  exit 0
fi

have() { type "$1" &> /dev/null; }
have rsnapshot || {
  echo "rsnapshot has to be installed"
  exit 1
}

###############################################################################
# config
# UUID="6907a27a-063b-48fb-86eb-7b725c12bdf9" # arch
UUID="3294d03a-f08b-433c-8f04-9cc2a3e9dc10" # nixos
BACKUPMOUNT="/mnt/backup/"
BACKUPDIR="${BACKUPMOUNT}$(hostname)/"
STATUSFILE="${BACKUPDIR}statusfile"

rsnapshot_cfg() { #{{{
    cat <<CONFIG
# this file is overwritten at every run
# This file requires tabs between the elements
config_version	1.2
snapshot_root	${BACKUPDIR}
no_create_root	1

#cmd_cp		$(which cp)
cmd_rm		$(which rm)
cmd_rsync	$(which rsync)
#cmd_ssh	/usr/bin/ssh
cmd_logger	$(which logger)
#cmd_du		$(which du)
#cmd_rsnapshot_diff	$(which rsnapshot-diff)
#cmd_preexec	/path/to/preexec/script
#cmd_postexec	/path/to/postexec/script

#interval	hourly	6
interval	daily	7
interval	weekly	4
interval	monthly	3

verbose		2
loglevel	3
#logfile	/var/log/rsnapshot
lockfile	/var/run/rsnapshot.pid
#rsync_short_args	-a
#rsync_long_args	--delete --numeric-ids --relative --delete-excluded
#ssh_args	-p 22
#du_args	-csh
one_fs		1

#include_file	/path/to/include/file
exclude_file	${BACKUPDIR}exclude_file

#link_dest	0
#sync_first	0
#use_lazy_deletes	0
#rsync_numtries 0

### BACKUP POINTS / SCRIPTS ###
backup	/home/					localhost/
backup	/etc/					localhost/
#backup	/usr/local/				localhost/
#backup	/var/log/rsnapshot		localhost/
backup	/root/					localhost/
backup	/var/					localhost/
CONFIG
    if [ -d /nix/var ]; then
        echo "backup	/nix/var/					localhost/"
    fi
} #}}}

exclude_file() {
cat <<EXCLUDE
/home/docker
${HOME}/tmp/
${HOME}/Downloads/
${HOME}/.cache/
${HOME}/Bilder/workspace/
${HOME}/Desktop/games/
${HOME}/.local/share/Steam/
${HOME}/.wine/
/tmp/
/var/lib/docker/
/var/cache/
EXCLUDE
}

###############################################################################
# mount disc
echo "0 0 0" >/sys/class/scsi_host/host1/scan
sleep 1
sudo mkdir -p ${BACKUPMOUNT}
umount /dev/disk/by-uuid/$UUID || continue
mount /dev/disk/by-uuid/$UUID ${BACKUPMOUNT}

rootId=$(stat -c%d /)
mountId=$(stat -c%d "${BACKUPMOUNT}")
if (( rootId == mountId )); then
  echo "nothing mounted at $BACKUPMOUNT"
  exit 1
fi

mkdir -p $BACKUPDIR

if [[ ! -f $STATUSFILE ]]; then
  echo "no statusfile found, generate one with:"
  echo "    echo \"0\" > $STATUSFILE"
  exit 1
fi

###############################################################################
# write rsnapshot config to files
BACKUPNR=`cat $STATUSFILE`
rsnapshot_cfg >${BACKUPDIR}rsnapshot_cfg
exclude_file >${BACKUPDIR}exclude_file

###############################################################################
# do backup
echo "starting backups (Nr. $BACKUPNR)..."
echo

rsnapshot -c ${BACKUPDIR}rsnapshot_cfg $1 daily
if (test $[$BACKUPNR % 7] -eq 0); then
  rsnapshot -c ${BACKUPDIR}rsnapshot_cfg $1 weekly
  if (test $[$BACKUPNR % 28] -eq 0); then
    rsnapshot -c ${BACKUPDIR}rsnapshot_cfg $1 monthly
  fi
fi

echo `expr $BACKUPNR + 1` > $STATUSFILE

have pacman && {
    pacman -Qeq > ${BACKUPDIR}daily.0/pacmanPakete.txt
}

have nix && {
    nix-store -q --references /var/run/current-system/sw | cut -d'-' -f2- > ${BACKUPDIR}daily.0/nixPakete.txt
    nix-channel --list > ${BACKUPDIR}daily.0/nixChannel.txt
}

###############################################################################
# unmount
sync
umount $BACKUPMOUNT
# hdparm -Y /dev/disk/by-uuid/$UUID

if [ "$1" = "suspend" ]; then
    systemctl suspend
fi

# vim: set noexpandtab :
