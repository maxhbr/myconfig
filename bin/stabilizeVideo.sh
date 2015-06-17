#!/usr/bin/env bash
#
# This needs extensivly many disk space.
#
# stolen from:
#  - https://wiki.libav.org/Encoding/h264
#  - https://isenmann.wordpress.com/2011/03/22/deshaking-videos-with-linux/

if [[ "$2" ]]; then
  if [[ -f "$2/$1" ]]; then
    cp "$2/$1" ./
  fi
fi

fn="${1%.*}"
if [[ -e "${1}.trf" ]]; then
  mv "${1}.trf" "${1}.trf.alt"
fi
transcode -J stabilize -i "$1"
transcode -J transform=crop=1:optzoom=0 -i "$1" -y raw -o "${fn}-stab.avi"
avconv -i "${fn}-stab.avi" -c:v h264 -c:a copy -crf 20 "${fn}-stab-tny.mp4"
mv "${fn}-stab-tny.mp4" "${fn}-stab.avi"

if [[ "$2" ]]; then
  if [[ -d "$2" ]]; then
    cp "${fn}-stab.avi" "$2"
  fi
fi
