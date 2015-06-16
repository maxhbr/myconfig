#!/usr/bin/env bash
#
# This needs extensivly many disk space.
#
# stolen from:
#  - https://wiki.libav.org/Encoding/h264
#  - https://isenmann.wordpress.com/2011/03/22/deshaking-videos-with-linux/

fn="${1%.*}"
if [[ ! -e "${1}.trf" ]]; then
    transcode -J stabilize -i "$1"
fi
transcode -J transform=crop=1:optzoom=0 -i "$1" -y raw -o "${fn}-stab.avi"
avconv -i "${fn}-stab.avi" -c:v h264 -c:a copy -crf 20 "${fn}-stab-tny.mp4"
mv "${fn}-stab-tny.mp4" "${fn}-stab.avi"
