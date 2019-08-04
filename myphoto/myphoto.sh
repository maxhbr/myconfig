#!/usr/bin/env bash

set -e

root="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
stackyaml="$root/stack.yaml"


if [[ "$1" == "init" ]]; then
    shift
    if [[ "$1" == "--" ]]; then
        shift
    fi
    set -x
    exec "$root/unported/initFotoDir.sh" "$@"
fi

stack --stack-yaml "$stackyaml"\
      build

echo "run:..."
stack --stack-yaml "$stackyaml" \
      exec -- myphoto-exe \
      "$@"
