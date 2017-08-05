#!/usr/bin/env bash
# needs imagemagic

set -e
output_dir="$HOME/_screenshots"
old_dir="$output_dir/_old"
output="$output_dir/$(date +%Y-%m-%d_%H:%M:%S).png"
mkdir -p "$output_dir"
mkdir -p "$old_dir"

set -x
find "$output_dir" -maxdepth 1 -mtime +10 -type f -print -exec mv {} "$old_dir" \;
import "$output"
