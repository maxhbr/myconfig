#!/usr/bin/env bash
set -euo pipefail

collector=$(dirname -- "${BASH_SOURCE[0]}")/../modules/myconfig.observability/scripts/fail2ban-textfile.sh
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
client=$work/fail2ban-client
metrics=$work/fail2ban.prom

cat >"$client" <<'FIXTURE'
#!/usr/bin/env bash
set -eu
[[ $1 == status ]] || exit 2
if (($# == 1)); then
    [[ $scenario != status_failure ]] || exit 1
    if [[ $scenario == empty ]]; then
        printf 'Status\n|- Number of jail:\t0\n`- Jail list:\t\n'
    else
        printf 'Status\n|- Number of jail:\t2\n`- Jail list:\tsshd, web\\"auth\n'
    fi
    exit 0
fi

case $2 in
    sshd) failed=30 banned=2 total=12 ;;
    'web\"auth')
        [[ $scenario != jail_failure ]] || exit 1
        failed=40 banned=1 total=23
        [[ $scenario != malformed ]] || total=23a
        ;;
    *) exit 2 ;;
esac
printf 'Status for the jail: %s\n' "$2"
printf '|- Filter\n|  |- Currently failed:\t0\n|  |- Total failed:\t%s\n|  `- File list:\t/example\n' "$failed"
printf '`- Actions\n   |- Currently banned:\t%s\n   |- Total banned:\t%s\n   `- Banned IP list:\t\n' "$banned" "$total"
FIXTURE
chmod +x "$client"

scrape() {
    scenario=$1 bash "$collector" "$client" "$work"
    [[ $(stat -c %a "$metrics") == 644 ]]
}

# Ignore HELP prose, but check all headers and samples, including order.
normalized() {
    sed -E 's/^(# HELP [^ ]+) .+$/\1/' "$metrics"
}

scrape good
diff -u - <(normalized) <<'EXPECTED'
# HELP fail2ban_up
# TYPE fail2ban_up gauge
fail2ban_up 1
# HELP fail2ban_jail_currently_banned
# TYPE fail2ban_jail_currently_banned gauge
fail2ban_jail_currently_banned{jail="sshd"} 2
fail2ban_jail_currently_banned{jail="web\\\"auth"} 1
# HELP fail2ban_jail_total_banned
# TYPE fail2ban_jail_total_banned counter
fail2ban_jail_total_banned{jail="sshd"} 12
fail2ban_jail_total_banned{jail="web\\\"auth"} 23
# HELP fail2ban_jail_total_failed
# TYPE fail2ban_jail_total_failed counter
fail2ban_jail_total_failed{jail="sshd"} 30
fail2ban_jail_total_failed{jail="web\\\"auth"} 40
EXPECTED

# An atomic replacement leaves a reader of the previous inode unaffected.
ln "$metrics" "$work/good"
for scenario in jail_failure status_failure malformed empty; do
    scrape "$scenario"
    up=0
    if [[ $scenario == empty ]]; then up=1; fi
    printf '# HELP fail2ban_up\n# TYPE fail2ban_up gauge\nfail2ban_up %s\n' "$up" | diff -u - <(normalized)
done
grep -qx 'fail2ban_up 1' "$work/good"
grep -q '^fail2ban_jail_' "$work/good"
