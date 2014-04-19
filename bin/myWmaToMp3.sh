#!/bin/sh

#Rip with Mplayer / encode with LAME
for track in *.wma; do
  ffmpeg -y -i "$track" -f wav - | \
    lame --cbr -b 321 - "`basename "$track"`.mp3"
done
