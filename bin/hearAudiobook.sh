#!/usr/bin/env bash
#
# hearAudiobook.sh [-e=ext] [-p=player] [-c=count] [folder]
#
# Known problems:
#  * does not work woll with mplayer-resumer, since the return code is 0 when
#    quitted and saved
#  * globbing does not match for multiple file extensions (find does not sort
#    the files correctly)
#
# written by: maximilian-huber.de (2015)

mCount="0"                     # maximal conut of played tracks
count="0"                      # current number of track
player="mplayer"               # the used player
path="."                       # the default path
# ext="\(mp3\|m4b\|wav\)"
ext="m4b"                      # the extension(s)
for i in "$@"; do
    case $i in
        -e=*|--extensions=*)
            ext="${i#*=}"
            shift ;;
        -p=*|--player=*)
            player="${i#*=}"
            shift ;;
        -c=*|--count=*)
            mCount="${i#*=}"
            shift ;;
        *)
            [[ -e $i ]] && path=$i
            shift ;;
    esac
done

################################################################################

pushd "$path" >/dev/null || exit 1
# find . -type f  -regex ".*\.$extension" -print0 | sort | while IFS= read -r -d $'\0' f; do
for f in *.$ext; do
    if [ ! -e ".$(basename "${f// /_}").done" ] || [ -e ".$(basename "$f").resumer" ]; then
        let "count += 1"
        [[ ! "$mCount" -eq "0" ]] && echo "$count / $mCount: $f" \
                || echo "$count: $f"
        $player "$f" 2> /dev/null && touch ".$(basename "${f// /_}").done" || exit 0
        [[ "$count" -eq "$mCount" ]] && exit 0
    fi
done
popd >/dev/null
