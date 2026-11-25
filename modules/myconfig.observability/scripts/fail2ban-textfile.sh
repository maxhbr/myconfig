#!/usr/bin/env bash
# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Emit fail2ban jail counters as Prometheus textfile metrics for the
# node_exporter textfile collector.
#
# fail2ban has no built-in Prometheus endpoint, and its own counters are
# monotonic across restarts only because fail2ban persists them in
# /var/lib/fail2ban/fail2ban.sqlite3. We therefore scrape `fail2ban-client
# status <jail>` and re-publish the two monotonic counters under _total
# names (Prometheus convention) plus per-jail info metrics.
#
# `Currently banned` is a gauge, so it is published as-is. `Total banned`
# and `Total failed` are the persisted totals and are published as
# counters; node_exporter's textfile collector does not compute rates for
# them, so the dashboard uses increase()/rate() over the raw values.
set -euo pipefail

@fail2banClient@ status >/dev/null

{
    echo "# HELP fail2ban_up Whether the fail2ban server answered."
    echo "# TYPE fail2ban_up gauge"
    printf 'fail2ban_up 1\n'

    # `fail2ban-client status` (no jail) lists the jails in the only
    # bracket-enclosed line; each jail is comma+space separated.
    jails="$(
        @fail2banClient@ status |
            sed -n 's/^.*Jail list:[[:space:]]*\(.*\)$/\1/p' |
            tr ',' '\n' |
            sed 's/^[[:space:]]*//;s/[[:space:]]*$//' |
            grep -v '^$' || true
    )"

    for jail in $jails; do
        out="$(@fail2banClient@ status "$jail")"

        num() { sed -n "s/^.*$1:[[:space:]]*\([0-9][0-9]*\).*$/\1/p" <<<"$out"; }

        printf '# HELP fail2ban_jail_currently_banned Number of IPs currently banned in this jail.\n'
        printf '# TYPE fail2ban_jail_currently_banned gauge\n'
        printf 'fail2ban_jail_currently_banned{jail="%s"} %s\n' "$jail" "$(num 'Currently banned')"

        printf '# HELP fail2ban_jail_total_banned Total number of bans handed out by this jail.\n'
        printf '# TYPE fail2ban_jail_total_banned counter\n'
        printf 'fail2ban_jail_total_banned{jail="%s"} %s\n' "$jail" "$(num 'Total banned')"

        printf '# HELP fail2ban_jail_total_failed Total number of failed authentications observed by this jail.\n'
        printf '# TYPE fail2ban_jail_total_failed counter\n'
        printf 'fail2ban_jail_total_failed{jail="%s"} %s\n' "$jail" "$(num 'Total failed')"
    done
} > "$1"
