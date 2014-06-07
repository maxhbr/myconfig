#!/bin/sh
# from: https://wiki.archlinux.org/index.php/Rip_Audio_CDs
# This script needs mp3splt, cdrtools, vorbis-tools

CREATE_OGG="true";
CREATE_FLAC="false";
CREATE_MKA="false";

WORKING_DIRECTORY=$(pwd)

function usage() 
{ 
  echo "Usage: $0 [OUTPUT_DIRECTORY] device";
  echo "eg.: $0 my_audio_cd /dev/sr0";
  exit;
}

function no_cdtext()
{
  echo "CD doesn't contain cd-text information";
  echo "Please specify output directory";
  echo "$0 OUTPUT_DIRECTORY device";
  exit;
}

function remove_quotes() {
  local OUTPUT_STRING
  # remove quotes
  OUTPUT_STRING=$(echo "$1" | sed "s/^\([\"']\)\(.*\)\1\$/\2/g")
  # remove selected special characters
  echo "$OUTPUT_STRING" | sed "s/\(.*\)[\\]\(.*\)/\1\2/g"
}

# store command line parameters
if  [[ "$#" = "2" ]] ; then
  CDROM=$2;
  OUTPUT_DIRECTORY=$1;
elif [[ "$#" = "1" ]] ; then
  CDROM=$1;
else 
  usage;
fi

# retrieve CD information
echo "Retrieve CD information"
CD_INFO=$(cdda2wav -J -g /dev/sr0 2>&1)

# test if CD-Text is available
if [[ "detected" == $(echo "${CD_INFO}" | awk '/^CD-Text.*/ {print $2}') ]]; then
  echo "CD-Text detected"
  CD_TEXT_DETECTED=true
  ALBUM_INFORMATION=$(echo "${CD_INFO}" | grep ^"Album title")
  # remove characters from beginning of string
  ALBUM_INFORMATION=$(echo "${ALBUM_INFORMATION#*Album title: }")
  ALBUM_TITLE=$(echo "${ALBUM_INFORMATION% from *}")
  ALBUM_TITLE=$(remove_quotes "${ALBUM_TITLE}")
  ALBUM_ARTIST=$(echo "${ALBUM_INFORMATION#* from }")
  ALBUM_ARTIST=$(remove_quotes "${ALBUM_ARTIST}")
fi

# check if cd has cdtext
if [[ true == ${CD_TEXT_DETECTED} ]]; then
  if [[ "2" = "$#" ]]; then
    echo "CD-text found"
    echo "Overriding given output directory name"
  fi
  mkdir -p "./${ALBUM_ARTIST}/${ALBUM_TITLE}"
  cd "./${ALBUM_ARTIST}/${ALBUM_TITLE}"
  OUTPUT_DIRECTORY="${ALBUM_ARTIST}/${ALBUM_TITLE}"
  OUTPUT_FILENAME="${ALBUM_TITLE}"
else
  if [[ "$#" != "2" ]]; then
    no_cdtext
  else
    mkdir "${OUTPUT_DIRECTORY}"
    cd "${OUTPUT_DIRECTORY}"
    OUTPUT_FILENAME="${OUTPUT_DIRECTORY}"
  fi
fi

cdda2wav -cuefile -paranoia -t all dev=${CDROM} "${OUTPUT_FILENAME}.wav" &> "${OUTPUT_FILENAME}.log"

if [[ "true" = ${CREATE_OGG} ]]; then
  mkdir ogg
  pushd ogg
  oggenc -o "${OUTPUT_FILENAME}.ogg" -q 6 "../${OUTPUT_FILENAME}.wav"
  oggsplt -c "../${OUTPUT_FILENAME}.cue" -o "@N - @t" "${OUTPUT_FILENAME}.ogg"
  popd
fi

if [[ "true" = ${CREATE_FLAC} ]]; then
  mkdir flac
  pushd flac
  cd flac  # -5 -V -T "ARTIST=%a" -T "ALBUM=%g" -T "DATE=%y" -T "GENRE=%m" --tag-from-file=CUESHEET="%a - %g.cue" --cuesheet="%a - %g.cue" %s
  flac --cuesheet="../${OUTPUT_FILENAME}.cue" --tag-from-file=CUESHEET="../${OUTPUT_FILENAME}.cue" --output-name="./${OUTPUT_FILENAME}.flac" --best "../${OUTPUT_FILENAME}.wav"
  popd
fi

if [[ "true" = ${CREATE_MKA} ]]; then
  mkdir mka
  pushd mka
  # -5 -V -T "ARTIST=%a" -T "ALBUM=%g" -T "DATE=%y" -T "GENRE=%m" --tag-from-file=CUESHEET="%a - %g.cue" --cuesheet="%a - %g.cue" %s
  flac --output-name="./${OUTPUT_FILENAME}.flac" --best "../${OUTPUT_FILENAME}.wav"
  mkvmerge -q -o "${OUTPUT_FILENAME}.flac.mka" "${OUTPUT_FILENAME}.flac" --attachment-mime-type text/plain --attachment-description "cdda2wav Log" --attach-file "../${OUTPUT_FILENAME}.log" --attachment-mime-type text/plain --attachment-description "Original CUE Sheet" --attach-file "../${OUTPUT_FILENAME}.cue" --title "${ALBUM_TITLE}" --chapters "../${OUTPUT_FILENAME}.cue"
  rm "./${OUTPUT_FILENAME}.flac"
  popd
fi

# rm "${WORKING_DIRECTORY}/${OUTPUT_DIRECTORY}/${OUTPUT_FILENAME}.{cddb,cdindex,cdtext,wav}"
