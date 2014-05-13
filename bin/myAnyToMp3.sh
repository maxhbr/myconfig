#!/bin/sh

#for wma
for a in *.wma; do
  ffmpeg -y -i "$a" -f wav - | \
    lame --cbr -b 321 - "${a%.wma}.mp3"
done

#for m4a
for a in *.m4a; do 
  faad -o - "$a" | \
    lame --cbr -b 321 - "${a%.m4a}.mp3"
done

#for flac
for a in *.flac; do
  < /dev/null ffmpeg -i "$a" -qscale:a 0 "${a[@]/%flac/mp3}"
done
