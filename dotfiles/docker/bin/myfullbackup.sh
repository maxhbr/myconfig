#!/usr/bin/env bash
#
# Written by Maximilian-Huber.de

set -e

type "docker" &> /dev/null || {
    echo "this script needs docker to be installed within \$PATH"
    exit 1
}
DATE=`date +%Y-%m-%d`

SOURCE="/source"
TARGET="/target"

###############################################################################
# config                                                                      #
case "$(hostname)" in
    mobile)
        UUID="3294d03a-f08b-433c-8f04-9cc2a3e9dc10" # nixos
        ;;
    arch)
        UUID="6907a27a-063b-48fb-86eb-7b725c12bdf9" # arch
        ;;
    *)
        echo "this script is not configured for this host"
        exit 1
        ;;
esac

rsnapshot_cfg() { #{{{
    cat <<CONFIG
# this file is overwritten at every run
# This file requires tabs between the elements
config_version	1.2
snapshot_root	${TARGET}
no_create_root	1

#cmd_cp		
#cmd_rm		
cmd_rsync	/usr/bin/rsync
#cmd_ssh	/usr/bin/ssh
#cmd_logger	
#cmd_du		
#cmd_rsnapshot_diff	

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
exclude_file	${TARGET}/exclude_file

#link_dest	0
#sync_first	0
#use_lazy_deletes	0
#rsync_numtries 0

### BACKUP POINTS / SCRIPTS ###
backup	${SOURCE}/home/					localhost/
backup	${SOURCE}/etc/					localhost/
#backup	${SOURCE}/usr/local/				localhost/
backup	${SOURCE}/root/					localhost/
backup	${SOURCE}/var/					localhost/
CONFIG
    if [ -d /nix/var ]; then
        echo "backup	${SOURCE}/nix/var/					localhost/"
    fi
} #}}}

exclude_file() {
cat <<EXCLUDE
${SOURCE}/home/docker
${SOURCE}${HOME}/tmp/
${SOURCE}${HOME}/Downloads/
${SOURCE}${HOME}/.cache/
${SOURCE}${HOME}/Bilder/workspace/
${SOURCE}${HOME}/Desktop/games/
${SOURCE}${HOME}/.local/share/Steam/
${SOURCE}${HOME}/.wine/
${SOURCE}/tmp/
${SOURCE}/var/lib/docker/
${SOURCE}/var/cache/
${SOURCE}/mnt/
EXCLUDE
}

# end config                                                                  #
###############################################################################

BACKUPMOUNT="/mnt/backup"
BACKUPDIR="${BACKUPMOUNT}/$(hostname)"
STATUSFILE="${BACKUPDIR}/statusfile"
LOGFILE="${BACKUPDIR}/${DATE}_logfile.log"

set -x

###############################################################################
# mount disc                                                                  #
echo "0 0 0" | sudo tee /sys/class/scsi_host/host1/scan
sleep 1
sudo mkdir -p ${BACKUPMOUNT}
rootId=$(stat -c%d /)
mountId=$(stat -c%d "${BACKUPMOUNT}")
if (( rootId == mountId )); then
    mount /dev/disk/by-uuid/$UUID ${BACKUPMOUNT}

    mountId=$(stat -c%d "${BACKUPMOUNT}")
    if (( rootId == mountId )); then
        echo "nothing mounted at $BACKUPMOUNT"
        exit 1
    fi
fi
mkdir -p $BACKUPDIR

# end mount disc                                                              #
###############################################################################

exec &> >(sudo tee -a "$LOGFILE")
echo
echo
echo "$(date)"

###############################################################################
# write config files                                                          #

rsnapshot_cfg | sudo tee ${BACKUPDIR}/rsnapshot_cfg
exclude_file | sudo tee ${BACKUPDIR}/exclude_file

# end write config files                                                      #
###############################################################################

###############################################################################
# generate docker image                                                       #
sudo docker rm --force myfullbackup >/dev/null 2>&1 || continue

sudo docker build -t myfullbackup --rm=true --force-rm=true - <<EOF
FROM ubuntu:latest
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update && apt-get install -y \
  rsnapshot

VOLUME /source
VOLUME /target

CMD rsnapshot -V -c /target/rsnapshot_cfg daily
EOF
# end generate docker image                                                   #
###############################################################################

###############################################################################
# run                                                                         #
if [[ -f $STATUSFILE ]]; then
  BACKUPNR="$(cat $STATUSFILE)"
else
  BACKUPNR=0
fi

LDIFS=$IFS; IFS=','
for i in daily,1 weekly,7 monthly,28; do
  set -- $i;
  if (test $[$BACKUPNR % $2] -eq 0); then
    sudo docker run -ti \
                --volume /:/source \
                --volume $BACKUPDIR:/target \
                --name=myfullbackup \
                myfullbackup \
                rsnapshot -V -c /target/rsnapshot_cfg $1
  fi
done
IFS=$OLDIFS
echo -n "$(expr $BACKUPNR + 1)" | sudo tee $STATUSFILE

###############################################################################

type "pacman" &> /dev/null && {
    pacman -Qeq > ${BACKUPDIR}/daily.0/pacmanPakete.txt
}

type "nix" &> /dev/null && {
    nix-store -q --references /var/run/current-system/sw | cut -d'-' -f2- > ${BACKUPDIR}/daily.0/nixPakete.txt
    nix-channel --list > ${BACKUPDIR}/daily.0/nixChannel.txt
}
# end run                                                                     #
###############################################################################

###############################################################################
# unmount disc                                                                #
sync
sudo umount $BACKUPMOUNT
type "hdparm" &> /dev/null && {
  sudo hdparm -Y /dev/disk/by-uuid/$UUID
}
# end unmount disc                                                            #
###############################################################################

if [ "$1" = "suspend" ]; then
    sudo systemctl suspend
fi

# vim: set noexpandtab :
