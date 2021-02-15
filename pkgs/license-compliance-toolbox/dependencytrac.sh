#!/usr/bin/env bash
set -e

dir="$HOME/.dependencytrac"
mkdir -p "$dir"
cd "$dir"
curl -LO https://dependencytrack.org/docker-compose.yml
if [[ "$1" == "--rm" ]]; then
    docker-compose down || true
fi
docker-compose up
