#!/usr/bin/env nix-shell
#! nix-shell -i bash

set -e

# this matches the settings in
#  - darktable:
#      $(FILE_FOLDER)/1_processed/../2_final/../0_converted/$(EXIF_YEAR)$(EXIF_MONTH)$(EXIF_DAY)_$(FILE_NAME)_dt
#  - rawtherapee:
#      TODO

if [[ "$1" && -d "$1" ]]; then
    find "$1" \
         -mtime -1 \
         -exec cp "{}" $(pwd) \;
fi

mkdir -p \
      0_converted \
      1_processed \
      2_final

if ! pgrep -x "gthumb" > /dev/null; then
    gthumb ./ &disown
fi
