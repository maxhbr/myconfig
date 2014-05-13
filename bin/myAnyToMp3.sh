#!/bin/sh

#Rip with Mplayer / encode with LAME
for track in *.wma; do
  ffmpeg -y -i "$track" -f wav - | \
    lame --cbr -b 321 - "${track%.wma}.mp3"
done

#Rip with FAAD / encode with LAME
for i in *.m4a; do 
  faad -o - "$track" | \
    lame --cbr -b 321 - "${track%.m4a}.mp3"
done
