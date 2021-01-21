#!/usr/bin/env bash

dir="$HOME/.dependencytrac"
mkdir -p "$dir"
cd "$dir"
curl -LO https://dependencytrack.org/docker-compose.yml
docker-compose up
