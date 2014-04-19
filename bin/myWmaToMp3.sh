#!/bin/sh

#Rip with Mplayer / encode with LAME
for track in *.wma; do ffmpeg -y -i "$track" -f wav - | lame --cbr -b 320 - "$track.mp3"; done

#convert file names
for i in *.wma; do mv "$i" "`basename "$i" .wma`.mp3"; done
