#!/usr/bin/env bash
#
# This needs extensivly many disk space. The temorary files are created in the
# folder, from which this script is called
#
# stolen from:
#  - https://wiki.libav.org/Encoding/h264
#  - https://isenmann.wordpress.com/2011/03/22/deshaking-videos-with-linux/
#
# stabilizeVideo.sh path/to/fileToStabilize
#
if [ ! -e "${1}" ]; then
    echo 'file not found' ; exit 1
fi

bn=$(basename $1)
fn="${1%.*}"
dn=$(dirname $1)
td="./"

if [ ! -e "${td}/${bn}" ]; then
    cp "$1" "${td}/${bn}" \
        || { echo 'copy of file failed' ; exit 1; }
fi

## Move old 'trf' file out of the way
if [ -e "${td}/${1}.trf" ]; then
    mv "${td}/${1}.trf" "${td}/${1}.trf.alt"
fi

################################################################################
## Do all the work
time transcode -threads 4 -J stabilize \
          -i "${td}/$1" \
    || { echo 'first pass failed' ; exit 1; }
time transcode -threads 4 -J transform=crop=1:optzoom=0 \
          -i "${td}/$1" -y raw \
          -o "${td}/${fn}-stab-large.avi" \
    || { echo 'second pass failed' ; exit 1; }
time avconv -threads auto \
       -i "${td}/${fn}-stab-large.avi" -c:v h264 -c:a copy -crf 20 \
       "${td}/${fn}-stab.mp4" \
    || { echo 'transcoding failed' ; exit 1; }
################################################################################
rm "${td}/${fn}-stab-large.avi"

## Copy stabilized file
cp "${td}/${fn}-stab.mp4" "${dn}/${fn}-stab.mp4"
