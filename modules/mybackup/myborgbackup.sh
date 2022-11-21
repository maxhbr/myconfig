#!/usr/bin/env bash
# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -euo pipefail

borgCmd="borg"
borgMountDir="/mnt/borgMountDir"

show_help() {
    >&2 cat <<EOF
run as:
    $0 b[ackup]
         -n ALIAS

    $0 i[nit]
         -n ALIAS  -- alias for the backup
         -s SOURCE -- what to backup, e.g. $HOME
         -t TARGET -- e.g.
                        - /dev/disk/by-uuid/...
                        - /mnt/backup
                        - backup@nas:/mnt/backup/
         [-e]      -- enable encryption

    $0 [-h]
EOF
}

handleTarget() {
    local target="$1"

    if [[ $target != *":"* ]]; then
        if [[ $target == "/dev/"* ]]; then
            echo "0 0 0" | sudo tee /sys/class/scsi_host/host*/scan || true
            sudo mkdir -p "$borgMountDir"
            sudo umount $target || true
            sudo mount $target "$borgMountDir"
        fi

        if [[ "$(df --output=source / | tail -1)" == "$(df --output=source "$target" | tail -1)" ]]; then
            >&2 echo "backup should be made to another filesystem or to ssh, \$target=$target does not satisfy that"
            exit 1
        fi
    fi

    echo "$target"
}

writeExcludes() {
    local excludes="$1"
    cat <<EOF >"$excludes"
*.ARW
*.img
*.iso
*.ova
*.pyc
*/.Trash*/
*/.cache/
*/.compose-cache/
*/.config/GIMP/*/backups
*/.gem/
*/.gradle/
*/.local/share/
*/.m2/
*/.nox/
*/.npm/
*/.ort/
*/.thumbnails/
*/.wine/
*/Bilder/workspace/
*/Downloads/
*/PIP/_*
*/TNG/
*/VirtualBox VMs/
*/tmp/
EOF
}

doInitPreperations() {
    >&2 echo "## doInitPreperations..."
    local home="$1"
    shift

    if [[ -d "$home" ]]; then
        >&2 echo "the folder \$home=$home already exists, probably already initialized"
        exit 1
    fi

    local target=""
    local source=""
    local encryption=none

    while [[ $# -gt 0 && "${1:-}" != "--" ]]; do
        opt="$1"
        case $opt in
            -t) target="$2"
                shift
                shift
                ;;
            -s) source="$2"
                shift
                shift
                ;;
            -e) encryption="repokey"
                shift
                ;;
            *) POSITIONAL+=("$1")
               shift
               ;;
        esac
    done

    local prefix="$(hostname)-"
    local repository=""
    if [[ $target == "/dev/"* ]]; then
        repository="${borgMountDir}/${prefix}${name}.borg"
    else
        repository="${target}/${prefix}${name}.borg"
    fi
    local excludes="$home/excludes"

    mkdir -p "$home"
    cat <<EOF | tee "$home/init"
export name="$name"
export target="$target"
export prefix="$prefix"
export repository="$repository"
export source="$source"
export encryption="$encryption"
export excludes="$excludes"
EOF
    writeExcludes "$excludes"
    if [[ "$encryption" != "none" ]]; then
        local passphraseFile="$home/passphrase"

        if [[ ! -f "$passphraseFile" ]]; then
            head -c 32 /dev/urandom | base64 -w 0 > "$passphraseFile"
            chmod 400 "$passphraseFile"
        fi

        echo "export BORG_PASSCOMMAND=\"cat '$passphraseFile'\"" | tee -a "$home/init"
    fi
}

doInitActually() {
    >&2 echo "## doInitActually..."
    set -x
    $borgCmd \
        init \
        --encryption "$encryption" \
        "$repository"

    if [[ "$encryption" != "none" ]]; then
        $borgCmd key export "$repository" "$home/keyExport.txt"
        $borgCmd key export --paper "$repository" "$home/keyExport.paper"
        $borgCmd key export --qr-html "$repository" "$home/keyExport.html"
    fi
    set +x
}

getBackupName() {
    echo "$(hostname)-$(date +%Y-%m-%d_%H:%M:%S)"
}

doBackup() {
    >&2 echo "## doBackup..."
    local home="$1"
    shift

    source "$home/init"

    set -x
    $borgCmd \
        create \
        --stats \
        --verbose \
        --progress \
        --filter AME \
        --show-rc \
        --one-file-system \
        --exclude-caches \
        --compression lz4 \
        --exclude-from "$excludes" \
        "${repository}::$(getBackupName)" \
        "${source}"
    set +x
}

doPrune() {
    >&2 echo "## doPrune..."
    local home="$1"
    shift

    source "$home/init"

    set -x
    $borgCmd \
        prune \
        --stats \
        --verbose \
        --list \
        --show-rc \
        --keep-within=2d --keep-daily=7 --keep-weekly=4 --keep-monthly=6 \
        --prefix "$prefix" \
        "$repository"
    set +x
}

doMount() {
    >&2 echo "## doMount..."
    local home="$1"
    shift

    set -x
    $borgCmd \
        mount \
        --foreground \
        $repository \
        $(mktemp -d)
    set +x
}

main() {
    >&2 echo "# main..."
    local name=""
    local doInit=false
    local doBackup=false
    local doPrune=false
    local doMount=false

    while [[ $# -gt 0 && "${1:-}" != "--" ]]; do
        opt="$1"
        case $opt in
            -h) show_help
                exit 0
                ;;
            -n) name="$2"
                shift
                shift
                ;;
            init|i) doInit=true
                  shift
                  ;;
            backup|b) doBackup=true
                    shift
                    ;;
            prune) doPrune=true
                   shift
                   ;;
            mount) doMount=true
                   shift
                   ;;
            *) POSITIONAL+=("$1")
               shift
               ;;
        esac
    done

    if [[ -z "$name" ]]; then
        >&2 echo "a name is required"
       exit 1
    fi

    local base="$HOME/.myborgbackup"
    mkdir -p "$base"
    local home="$base/$name"
    local logfile="$home/logfile"

    ############################################################################
    # Run

    if $doInit; then
        doInitPreperations "$home" "${POSITIONAL[@]}"
    fi

    if [[ ! -d "$home" ]]; then
        >&2 echo "\$home=$home not yet initialized"
        exit 1
    fi

    echo -e "\n\n\n\n\n\n\n" >> $logfile
    exec &> >(tee -a $logfile)

    source "$home/init"
    export target="$(handleTarget "$target")"

    if $doInit; then
        doInitActually "$home" "${POSITIONAL[@]}"
    fi

    borg info "$repository" --last 1

    if $doBackup; then
        doBackup "$home" "${POSITIONAL[@]}"
        date >> "$home/dates"
    fi

    if $doPrune; then
        doPrune "$home" "${POSITIONAL[@]}"
    fi

    if $doMount; then
        doMount "$home" "${POSITIONAL[@]}"
    fi

    if [[ -d "$target" ]]; then
        if [[ "$(df --output=source / | tail -1)" == "$(df --output=source "$target" | tail -1)" ]]; then
            sudo umount "$target" || echo "umount failed... continue"
        fi
    fi
}

time main "$@"
