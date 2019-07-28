#!/usr/bin/env bash

set -e

root="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
stackyaml="$root/stack.yaml"


if [[ "$1" == "init" ]]; then
    shift
    if [[ "$1" == "--" ]]; then
        shift
    fi
    exec "$root/unported/initFotoDir.sh" "$@"
fi

stack --stack-yaml "$stackyaml"\
      build
stack --stack-yaml "$stackyaml" \
      exec -- myphoto-exe \
      "$@"
