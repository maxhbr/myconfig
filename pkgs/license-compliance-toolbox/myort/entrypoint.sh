#!/usr/bin/env bash

set -e
if [[ -n $USER ]]; then
  echo -e "\n$USER:x:$(id -u):$(id -g)::/:/bin/bash" >> /etc/passwd
fi
if [[ -d /workdir-enc ]]; then
  trap 'kill -TERM \$PID; wait \$PID; fusermount -u /workdir' TERM INT
  echo "Mounting /workdir-enc to /workdir..."
  gocryptfs -extpass /opt/ort-password.sh /workdir-enc /workdir
fi
set +e

/opt/ort/bin/ort "$@" &
PID=$!
wait $PID
wait $PID
EXIT_STATUS=$?

if [[ -d /workdir-enc ]]; then
  fusermount -u /workdir
fi

exit $EXIT_STATUS
