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
set -u

log() { printf 'agent-gvisor-init: %s\n' "$*" >&2; }

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
# those the first time except this wrapper. Failures never abort the session.
if [ -n "${AGENT_GVISOR_NIX-}" ]; then
    mkdir -p "${TMPDIR:-/home/agent/.cache/nix-tmp}" \
        /home/agent/.local/state/nix/log 2>/dev/null \
        || log "warning: could not prepare the Nix state directories"
fi

# The payload replaces this shell, so it keeps PID 1, the TTY and all signals.
# The relays stay as its children and die with the container.
[ "$#" -gt 0 ] || set -- /bin/bash
exec "$@"
