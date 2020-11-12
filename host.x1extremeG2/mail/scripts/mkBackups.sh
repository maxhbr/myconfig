#!/usr/bin/env bash

set -ex

cd "$(dirname "$0")"
backupdir="$(pwd)/backups"
mkdir -p "$backupdir"

# TODO:
# - save Mails via
#   - IMAP
#   - Maildir
# - save Mu
# - save configuration
# - save gpg stuff
# - save passwords

notmuch dump > "$backupdir/notmuch.dump"
# restore with:
# $ notmuch restore "$backupdir/notmuch.dump"

