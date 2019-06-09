#!/usr/bin/env nix-shell
#! nix-shell -i bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e

# this matches the settings in
#  - darktable:
#      $(FILE_FOLDER)/../2_processed/../0_processed/$(EXIF_YEAR)$(EXIF_MONTH)$(EXIF_DAY)_$(FILE_NAME)_dt
#  - rawtherapee:
#      TODO

mkdir -p \
      0_raw \
      1_converted \
      2_processed

if [[ "$1" && -d "$1" ]]; then
    find "$1" \
         -type f \
         -mtime -1 \
         -exec cp "{}" "$(pwd)/0_raw" \;

    if ! pgrep -x "gthumb" > /dev/null; then
        gthumb ./0_raw &disown
    fi
fi
