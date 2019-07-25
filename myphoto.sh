#!/usr/bin/env bash

set -e

root="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
stackyaml="$root/stack.yaml"

stack --stack-yaml "$stackyaml"\
      build
stack --stack-yaml "$stackyaml" \
      exec -- myphoto-exe \
      "$@"
