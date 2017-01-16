#!/bin/sh

DOW="/home/hubi/Downloads"
OLD="/home/hubi/Downloads/old"
DELAY=7

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")
FIND=`find $DOW -maxdepth 1  -mtime +$DELAY -type f`
for f in $FIND; do
  DATE=$(stat -c%y $f | awk '{print $1; }')
  mv "$f" "$OLD/$DATE-$(basename $f)"
done
IFS=$SAVEIFS
