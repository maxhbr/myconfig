#!/usr/bin/env bash
# needs imagemagic
set -e
output="$HOME/$(date +%Y-%m-%d_%H:%M:%S).png"
import "$output"
echo "The file $output was created"
