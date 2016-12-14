#!/usr/bin/env bash
# stolen from: http://naleid.com/blog/2012/01/17/finding-and-purging-big-files-from-git-history

ALLFILESHAS=$(mktemp -p /tmp/)
BIGOBJECTS=$(mktemp -p /tmp/) 
git rev-list --objects --all \
  | sort -k 2 > $ALLFILESHAS
git gc && git verify-pack -v .git/objects/pack/pack-*.idx \
  | egrep "^\w+ blob\W+[0-9]+ [0-9]+ [0-9]+$" \
  | sort -k 3 -n -r > $BIGOBJECTS

for SHA in `cut -f 1 -d\  < $BIGOBJECTS`; do
  echo $(grep $SHA $BIGOBJECTS) $(grep $SHA $ALLFILESHAS) | awk '{print $1,$3,$7}' >> bigtosmall.txt
done;
