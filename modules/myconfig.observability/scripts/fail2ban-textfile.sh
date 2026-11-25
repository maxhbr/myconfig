#!/usr/bin/env bash
# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -euo pipefail
shopt -s extglob

client=${1:?expected fail2ban-client path}
directory=${2:?expected textfile directory}

# Require exactly one matching line: an empty "Jail list:" is valid,
# but a missing line must not turn a broken response into a healthy scrape.
field() {
    local pattern='^[|`[:blank:]-]*'"$1"':[[:blank:]]*(.*)$'
    local line value
    while IFS= read -r line; do
        [[ $line =~ $pattern ]] || continue
        [[ ! ${value+x} ]] || return 1
        value=${BASH_REMATCH[1]}
    done <<<"$2"
    [[ ${value+x} ]] || return 1
    printf '%s\n' "${value%%+([[:blank:]])}"
}

counts=('Currently banned' 'Total banned' 'Total failed')
families=(currently_banned total_banned total_failed)
types=(gauge counter counter)
help=('Number of IPs currently banned in this jail.'
    'Total number of bans handed out by this jail.'
    'Total number of failures observed by this jail.')
samples=()

collect() {
    local status list count jail label i value
    local -a jails
    local -A seen=()
    status=$("$client" status) || return 1
    list=$(field 'Jail list' "$status") || return 1
    count=$(field 'Number of jail' "$status") || return 1
    IFS=, read -ra jails <<<"$list"
    [[ $count == "${#jails[@]}" && $list != *, ]] || return 1

    for jail in "${jails[@]}"; do
        jail=${jail##+([[:blank:]])}
        jail=${jail%%+([[:blank:]])}
        [[ -n $jail && ! $jail =~ [[:cntrl:]] ]] || return 1
        [[ ! ${seen[$jail]+x} ]] || return 1
        seen[$jail]=1
        status=$("$client" status "$jail") || return 1
        label=${jail//\\/\\\\}
        label=${label//\"/\\\"}
        for i in "${!counts[@]}"; do
            value=$(field "${counts[i]}" "$status") || return 1
            [[ $value =~ ^[0-9]+$ ]] || return 1
            samples[i]+="fail2ban_jail_${families[i]}{jail=\"$label\"} $value"$'\n'
        done
    done
    return 0
}

if collect; then up=1; else up=0; fi

# Stage beside the destination, outside node_exporter's *.prom glob.
stage=$(mktemp -- "$directory/.fail2ban.XXXXXXXX")
trap 'rm -f -- "$stage"' EXIT
{
    printf '# HELP fail2ban_up Whether all fail2ban jail statistics were collected.\n'
    printf '# TYPE fail2ban_up gauge\nfail2ban_up %s\n' "$up"
    if ((up)); then
        for i in "${!samples[@]}"; do
            printf '# HELP fail2ban_jail_%s %s\n' "${families[i]}" "${help[i]}"
            printf '# TYPE fail2ban_jail_%s %s\n' "${families[i]}" "${types[i]}"
            printf '%s' "${samples[i]}"
        done
    fi
} >"$stage"
chmod 0644 "$stage"
mv -fT -- "$stage" "$directory/fail2ban.prom"
