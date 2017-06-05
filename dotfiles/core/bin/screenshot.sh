#!/usr/bin/env bash
# needs imagemagic
set -e
output_dir="$HOME/_screenshots"
mkdir -p "$output_dir"
output="$output_dir/$(date +%Y-%m-%d_%H:%M:%S).png"
import "$output"
echo "The file $output was created"
