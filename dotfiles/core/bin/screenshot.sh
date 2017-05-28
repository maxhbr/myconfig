#!/usr/bin/env bash
# needs imagemagic
set -e
mkdir -p "$HOME/_screenshots"
output="$HOME/_screenshot/$(date +%Y-%m-%d_%H:%M:%S).png"
import "$output"
echo "The file $output was created"
