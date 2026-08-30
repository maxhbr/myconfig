#!/bin/bash
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# agent-gvisor-init — entrypoint wrapper that sets up "reverse port forwards"
# inside the sandbox and then execs the real command.
#
# WHY THIS RUNS INSIDE THE CONTAINER
#
# runsc (gVisor) gives the sandbox its OWN network stack, so the sandbox's
# 127.0.0.1 is gVisor's loopback and nothing on the host — not the host's
# loopback, and not a pasta `-T` listener, which lives in the container's Linux
# netns that gVisor sits on top of. The only listener a sandboxed process can
# reach on 127.0.0.1 is one opened by a process inside the sandbox itself.
# Hence this relay: it runs in the sandbox, binds the loopback port the
# configuration expects (e.g. 4000), and forwards to the address that IS
# reachable (pasta's --map-guest-addr target, where the host's port-scoped
# forwarder listens).
#
# This grants NO new reach: the sandbox can already open that outbound
# connection itself. It only gives the same connection the name the agent
# configuration uses, so `http://127.0.0.1:4000/v1` works verbatim.
#
# AGENT_GVISOR_LOOPBACK_FORWARD is a space-separated list of
# `LPORT:RHOST:RPORT` rules. Failures never abort the session: a broken or
# missing relay is reported and the payload still runs.
#
# The wrapper is also installed as the payload for `--nix` sessions
# (AGENT_GVISOR_NIX=1, see docs/nix-in-sandbox.md): those need a few
# directories prepared before the first `nix` invocation, because the
# session home bind mount masks the image's /home/agent content.
#
# Unlike the relays, the Nix setup FAILS CLOSED: `--nix` is an explicit
# request for a usable store, and the core mechanism (podman copy-up into
# the named volume, owned by the keep-id mapped user, under runsc) is
# exactly the part that can silently not work on a given host
# (docs/nix-in-sandbox.md §7 V1). A session whose /nix/store or Nix state
# directory is not writable would fail later, deep inside some `nix`
# invocation, with an unrelated-looking error — so refuse to start it.
set -u

log() { printf 'agent-gvisor-init: %s\n' "$*" >&2; }

die() {
    log "error: $*"
    exit 1
}

# True when a file can actually be created inside $1 (as this user). Not
# `test -w`: under keep-id the copy-up can land owned by an unmapped uid,
# where the permission bits look fine but every write is EACCES.
dir_is_writable() {
    local dir=$1 probe
    probe=$dir/.agent-gvisor-write-probe.$$
    (: >"$probe") 2>/dev/null || return 1
    rm -f "$probe" 2>/dev/null
    return 0
}

# Wait until the relay actually accepts connections, so an agent that connects
# immediately does not race the listener. Bash's /dev/tcp is used because the
# image is not guaranteed to carry a probing tool like nc.
wait_for_port() {
    local port=$1 tries=0
    while [ "$tries" -lt 50 ]; do
        if (exec 3<>"/dev/tcp/127.0.0.1/$port") 2>/dev/null; then
            return 0
        fi
        tries=$((tries + 1))
        sleep 0.1
    done
    return 1
}

start_forward() {
    local spec=$1 lport rest rhost rport
    lport=${spec%%:*}
    rest=${spec#*:}
    rhost=${rest%:*}
    rport=${rest##*:}
    if [ -z "$lport" ] || [ -z "$rhost" ] || [ -z "$rport" ] || [ "$rest" = "$spec" ]; then
        log "warning: ignoring malformed forward rule '$spec' (expected LPORT:RHOST:RPORT)"
        return
    fi
    if ! command -v socat >/dev/null 2>&1; then
        log "warning: socat is not in the image; 127.0.0.1:$lport will not be forwarded"
        return
    fi
    socat "TCP4-LISTEN:$lport,bind=127.0.0.1,fork,reuseaddr" "TCP4:$rhost:$rport" &
    if wait_for_port "$lport"; then
        log "127.0.0.1:$lport -> $rhost:$rport"
    else
        log "warning: relay for 127.0.0.1:$lport did not come up; use $rhost:$rport directly"
    fi
}

for rule in ${AGENT_GVISOR_LOOPBACK_FORWARD-}; do
    start_forward "$rule"
done

# Nix inside the sandbox (`--nix` / AGENT_GVISOR_NIX=1, docs/nix-in-sandbox.md).
# The CLI mounts the writable store volume at /nix/store and points Nix at
# state and temp directories on the session home bind mount (the container
# rootfs is --read-only, and /tmp is a small tmpfs); nothing here can create
# those the first time except this wrapper. Unlike the relays above, a failure
# here aborts the session: `--nix` was asked for explicitly, so an unusable
# store is an error, not a degraded mode.
if [ -n "${AGENT_GVISOR_NIX-}" ]; then
    for dir in "${NIX_STATE_DIR:-/home/agent/.local/state/nix}" \
        "${TMPDIR:-/home/agent/.cache/nix-tmp}" \
        "${NIX_LOG_DIR:-/home/agent/.local/state/nix/log}"; do
        mkdir -p "$dir" 2>/dev/null ||
            die "could not create the Nix state directory $dir;" \
                "start the session without --nix, or see docs/nix-in-sandbox.md"
        dir_is_writable "$dir" ||
            die "the Nix state directory $dir is not writable;" \
                "start the session without --nix, or see docs/nix-in-sandbox.md"
    done
    # The store volume itself: seeded by podman's copy-up and writable by
    # the mapped user, or --nix is not usable on this host at all
    # (docs/nix-in-sandbox.md §2 "Drop --read-only", §7 V1). NIX_STORE_DIR
    # is the store nix would actually use; the CLI never sets it, so this
    # is /nix/store in every real session.
    store=${NIX_STORE_DIR:-/nix/store}
    dir_is_writable "$store" ||
        die "$store is not writable in this sandbox;" \
            "the --nix store volume did not come up (copy-up/ownership);" \
            "see docs/nix-in-sandbox.md §7 V1"
fi

# The payload replaces this shell, so it keeps PID 1, the TTY and all signals.
# The relays stay as its children and die with the container.
[ "$#" -gt 0 ] || set -- /bin/bash
exec "$@"
