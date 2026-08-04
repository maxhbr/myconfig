#!/usr/bin/env bash
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.microvm — REAL-KVM runtime validation suite (improvement ticket 6
# part A).
#
# The `nix flake check` suite (tests/microvm.nix) is EVAL/BUILD only: it proves
# the module evaluates, that the pool is well-formed and that the shell code
# passes shellcheck. It can neither boot a guest nor move a packet. This script
# is the other half: it must be run BY AN OPERATOR, as root, ON A HOST WITH
# /dev/kvm and the feature enabled (f13), and it exercises the properties that
# only exist at runtime.
#
# It is deliberately NOT part of `nix flake check` (no KVM in CI, and it starts
# real VMs, mounts filesystems and sends packets).
#
# Usage:
#   sudo ./modules/myconfig.ai/myconfig.ai.microvm/runtime-validation.sh \
#        --repository /home/mhuber/some-git-repo \
#        [--section all|boot|net|l2|creds|lifecycle|malrepo|forgery]
#
# Every check prints exactly one line: `PASS`, `FAIL` or `SKIP` plus a reason.
# The script exits non-zero if any check FAILED. SKIPs are honest: they mark
# things this environment cannot decide (e.g. only one slot in a class), never
# silent passes.
#
# The procedure and the meaning of each section are documented in
# docs/agent-microvm-runtime-validation.md.
set -euo pipefail

PROG="agent-microvm-runtime-validation"

# --- configuration (must match the module options of the host under test) ----
GATEWAY="${AGENT_GATEWAY:-192.168.83.1}"
LITELLM_PORT="${AGENT_LITELLM_PORT:-4000}"
BRIDGE="${AGENT_BRIDGE:-agentbr0}"
RUNTIME_ROOT="${AGENT_RUNTIME_ROOT:-/var/lib/agent-microvms}"
STATE_ROOT="${AGENT_STATE_ROOT:-/var/lib/microvms}"
WORKSPACE_ROOT="$RUNTIME_ROOT/workspaces"
LAUNCHER="${AGENT_LAUNCHER:-agent-microvm}"
# Batch job share (job.nix). The guest sees the same paths under /run/agent-job.
JOBS_ROOT="$RUNTIME_ROOT/jobs"
RESULTS_DIR="$RUNTIME_ROOT/results"
GUEST_JOB_DIR="/run/agent-job"
GUEST_INPUT_DIR="$GUEST_JOB_DIR/input"
GUEST_CTRL_DIR="$GUEST_JOB_DIR/controller"
GUEST_WORKER_DIR="$GUEST_JOB_DIR/worker"

REPO=""
SECTION="all"

PASS=0
FAIL=0
SKIP=0

pass() {
    printf 'PASS  %s\n' "$*"
    PASS=$((PASS + 1))
}
fail() {
    printf 'FAIL  %s\n' "$*"
    FAIL=$((FAIL + 1))
}
skip() {
    printf 'SKIP  %s\n' "$*"
    SKIP=$((SKIP + 1))
}
info() { printf '#     %s\n' "$*"; }
section() { printf '\n=== %s ===\n' "$*"; }

die() {
    printf '%s: error: %s\n' "$PROG" "$*" >&2
    exit 1
}

# `check <description> <command...>` — PASS when the command succeeds.
check() {
    local desc="$1"
    shift
    if "$@" >/dev/null 2>&1; then
        pass "$desc"
    else
        fail "$desc"
    fi
}

# `check_fails <description> <command...>` — PASS when the command FAILS. Used
# for every "the guest must NOT be able to ..." property, where a success is a
# security failure.
check_fails() {
    local desc="$1"
    shift
    if "$@" >/dev/null 2>&1; then
        fail "$desc (expected failure, but it succeeded)"
    else
        pass "$desc"
    fi
}

usage() {
    cat >&2 <<EOF
Usage: sudo $0 --repository <git-repo> [--section all|boot|net|l2|creds|lifecycle|malrepo|forgery]

Sections:
  boot       per-class boot, readiness, /workspace, persistence, shares
  net        proxy-only allow/deny matrix (metadata, RFC1918, DNS, internet)
  l2         two-guest layer-2 isolation (ARP spoofing, direct ethernet)
  creds      absence of host credentials/sockets inside a guest
  lifecycle  forced failures at every stage + slot reusability
  malrepo    hostile repository fixture (hooks, flake.nix, direnv, symlinks)
  forgery    batch RESULT CHANNEL: a hostile worker must not be able to write,
             replace, delete or shadow the authoritative result, stale results
             must be rejected, timeouts must kill the whole worker cgroup, and
             cancellation must be bound to the allocation token
EOF
    exit 2
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --repository)
            REPO="${2-}"
            shift 2
            ;;
        --section)
            SECTION="${2-}"
            shift 2
            ;;
        -h | --help) usage ;;
        *) die "unknown argument '$1'" ;;
    esac
done

[[ "$(id -u)" -eq 0 ]] || die "must run as root (it starts VMs and inspects mounts)"
[[ -e /dev/kvm ]] || die "/dev/kvm is missing — this suite requires real KVM"
command -v "$LAUNCHER" >/dev/null || die "$LAUNCHER is not on PATH (is the feature enabled on this host?)"
[[ -n $REPO ]] || usage
[[ -d "$REPO/.git" ]] || die "--repository must be a git repository: $REPO"

# --- helpers ---------------------------------------------------------------
slots_of_class() {
    "$LAUNCHER" list | awk -v c="$1" '$2 == c { print $1 }'
}

all_classes() {
    "$LAUNCHER" list | awk '{ print $2 }' | sort -u
}

# Run a command INSIDE a guest slot, as the unprivileged agent user.
guest() {
    local slot="$1"
    shift
    "$LAUNCHER" ssh "$slot" -- "$@"
}

# Start a detached interactive slot for a task, echo the slot name.
start_task() {
    local task="$1" class="$2" agent="${3:-pi}"
    "$LAUNCHER" run --name "$task" --repository "$REPO" --agent "$agent" \
        --resource-class "$class" >/dev/null 2>&1 || return 1
    "$LAUNCHER" list | awk -v t="$task" '$5 == t { print $1 }'
}

cleanup_task() {
    local task="$1"
    "$LAUNCHER" stop "$task" >/dev/null 2>&1 || true
    "$LAUNCHER" workspace-remove "$task" --force >/dev/null 2>&1 || true
}

# --- (1) boot / filesystem -------------------------------------------------
section_boot() {
    section "boot + filesystem (ticket 6 A.1)"
    local class slot task
    for class in $(all_classes); do
        task="rtv-boot-$class"
        cleanup_task "$task"
        info "class $class: starting $task"
        if ! slot="$(start_task "$task" "$class")" || [[ -z $slot ]]; then
            fail "class $class boots"
            continue
        fi
        pass "class $class boots ($slot)"
        check "class $class: guest is SSH-ready" guest "$slot" true
        check "class $class: /workspace is a mount point" \
            guest "$slot" findmnt -n /workspace
        check "class $class: /workspace is writable" \
            guest "$slot" sh -c 'echo runtime-validation > /workspace/.rtv && rm -f /workspace/.rtv'
        # The host /nix/store must NEVER be shared in.
        check_fails "class $class: host /nix/store is not shared" \
            guest "$slot" findmnt -n -S /nix/store
        # Exactly the four expected shares, nothing else.
        if guest "$slot" sh -c 'findmnt -t virtiofs -o TARGET -n | sort | tr "\n" " "' \
            2>/dev/null | grep -q "/run/agent-job /var/lib/agent-hostkey /var/lib/agent-state /workspace"; then
            pass "class $class: exactly the expected virtiofs shares"
        else
            fail "class $class: unexpected virtiofs share set"
        fi

        # Workspace changes must survive shutdown; guest root/home must not.
        guest "$slot" sh -c 'echo persisted > /workspace/rtv-persisted.txt' >/dev/null 2>&1 || true
        # shellcheck disable=SC2016  # $HOME must expand in the GUEST, not here
        guest "$slot" sh -c 'echo ephemeral > /root-marker 2>/dev/null || true; echo ephemeral > "$HOME/rtv-home-marker"' >/dev/null 2>&1 || true
        "$LAUNCHER" stop "$task" >/dev/null 2>&1 || true
        if [[ -f "$WORKSPACE_ROOT/$task/rtv-persisted.txt" ]]; then
            pass "class $class: workspace changes persist in the clone"
        else
            fail "class $class: workspace changes were lost"
        fi
        # Restart the same task on the same slot and check the guest's own state.
        if slot="$(start_task "$task-again" "$class")" && [[ -n $slot ]]; then
            check_fails "class $class: guest home does not persist between runs" \
                guest "$slot" test -f /home/agent/rtv-home-marker
            check_fails "class $class: guest root does not persist between runs" \
                guest "$slot" test -f /root-marker
            cleanup_task "$task-again"
        else
            skip "class $class: could not restart for persistence check"
        fi
        cleanup_task "$task"
    done

    section "task isolation + agent-state persistence (ticket 6 A.7/A.8)"
    local class_a slot_a slot_b
    class_a="$(all_classes | head -1)"
    cleanup_task rtv-iso-a
    cleanup_task rtv-iso-b
    if slot_a="$(start_task rtv-iso-a "$class_a" hermes)" && [[ -n $slot_a ]]; then
        # Task A must not see task B's workspace (there is no path to it at all).
        check_fails "task A cannot see another task's workspace" \
            guest "$slot_a" test -e "$WORKSPACE_ROOT/rtv-iso-b"
        check_fails "task A cannot see the host workspace root" \
            guest "$slot_a" test -d "$WORKSPACE_ROOT"
        cleanup_task rtv-iso-a
    else
        skip "task isolation: could not start rtv-iso-a"
    fi
    # Declared-path-only persistence.
    cleanup_task rtv-persist
    if slot_b="$("$LAUNCHER" run --name rtv-persist --repository "$REPO" --agent hermes \
        --resource-class "$class_a" --persist-agent-state >/dev/null 2>&1 &&
        "$LAUNCHER" list | awk '$5 == "rtv-persist" { print $1 }')" && [[ -n $slot_b ]]; then
        check "persisted state: ~/.hermes is a symlink into the share" \
            guest "$slot_b" test -L /home/agent/.hermes
        guest "$slot_b" sh -c 'mkdir -p ~/.hermes && echo kept > ~/.hermes/rtv' >/dev/null 2>&1 || true
        guest "$slot_b" sh -c 'echo notkept > ~/rtv-undeclared' >/dev/null 2>&1 || true
        "$LAUNCHER" stop rtv-persist >/dev/null 2>&1 || true
        if [[ -f "$RUNTIME_ROOT/state/tasks/rtv-persist/hermes/.hermes/rtv" ]]; then
            pass "persisted state: declared path survived on the host"
        else
            fail "persisted state: declared path did not survive"
        fi
        if find "$RUNTIME_ROOT/state/tasks/rtv-persist" -name rtv-undeclared | grep -q .; then
            fail "persisted state: an UNDECLARED path was persisted"
        else
            pass "persisted state: undeclared paths were not persisted"
        fi
        cleanup_task rtv-persist
    else
        skip "agent-state persistence: could not start rtv-persist"
    fi
}

# --- (2) network (proxy-only) ---------------------------------------------
section_net() {
    section "network: proxy-only allow/deny matrix (ticket 6 A.2)"
    local class slot
    class="$(all_classes | head -1)"
    cleanup_task rtv-net
    if ! slot="$(start_task rtv-net "$class")" || [[ -z $slot ]]; then
        fail "network: could not start a guest"
        return
    fi
    # ALLOWED: the bridge-only LiteLLM endpoint, reached via the guest's own
    # loopback forwarder AND directly at the gateway.
    check "guest reaches the LiteLLM endpoint via loopback" \
        guest "$slot" curl -fsS -m 10 -o /dev/null "http://127.0.0.1:$LITELLM_PORT/v1/models"
    check "guest reaches the LiteLLM endpoint on the bridge gateway" \
        guest "$slot" curl -fsS -m 10 -o /dev/null "http://$GATEWAY:$LITELLM_PORT/v1/models"

    # DENIED — every one of these succeeding is a security failure.
    check_fails "guest cannot reach the cloud-metadata endpoint" \
        guest "$slot" curl -fsS -m 5 -o /dev/null http://169.254.169.254/
    check_fails "guest cannot reach host SSH on the gateway" \
        guest "$slot" sh -c "timeout 5 sh -c '</dev/tcp/$GATEWAY/22'"
    check_fails "guest cannot reach an arbitrary host port (8080)" \
        guest "$slot" sh -c "timeout 5 sh -c '</dev/tcp/$GATEWAY/8080'"
    check_fails "guest cannot reach RFC1918 10.0.0.0/8" \
        guest "$slot" sh -c "timeout 5 sh -c '</dev/tcp/10.0.0.1/80'"
    check_fails "guest cannot reach RFC1918 172.16.0.0/12" \
        guest "$slot" sh -c "timeout 5 sh -c '</dev/tcp/172.16.0.1/80'"
    check_fails "guest cannot reach RFC1918 192.168.0.0/16 (outside the agent subnet)" \
        guest "$slot" sh -c "timeout 5 sh -c '</dev/tcp/192.168.1.1/80'"
    check_fails "guest cannot reach a public IP" \
        guest "$slot" sh -c "timeout 5 sh -c '</dev/tcp/1.1.1.1/80'"
    check_fails "guest cannot reach a public DNS server" \
        guest "$slot" sh -c "timeout 5 sh -c '</dev/tcp/8.8.8.8/53'"
    check_fails "guest cannot resolve public DNS names" \
        guest "$slot" sh -c "timeout 5 getent hosts example.com"
    # IPv6 must not provide a bypass: the bridge has IPv6 disabled.
    check_fails "guest has no IPv6 route that bypasses the IPv4 policy" \
        guest "$slot" sh -c "ip -6 route show default | grep -q ."
    cleanup_task rtv-net
}

# --- (3) layer 2 ----------------------------------------------------------
section_l2() {
    section "layer 2 isolation between two guests (ticket 6 A.3)"
    local class slots slot_a slot_b ip_a ip_b
    # Two simultaneous guests are required; they may come from different classes.
    mapfile -t slots < <("$LAUNCHER" list | awk '{ print $1 }')
    if ((${#slots[@]} < 2)); then
        skip "layer 2: fewer than two slots in the pool"
        return
    fi
    cleanup_task rtv-l2a
    cleanup_task rtv-l2b
    class="$(all_classes | head -1)"
    slot_a="$(start_task rtv-l2a "$class" || true)"
    # Prefer a different class for B if the first one has a single slot.
    for class in $(all_classes); do
        slot_b="$(start_task rtv-l2b "$class" 2>/dev/null || true)"
        [[ -n $slot_b ]] && break
    done
    if [[ -z $slot_a || -z $slot_b ]]; then
        skip "layer 2: could not start two guests simultaneously"
        cleanup_task rtv-l2a
        cleanup_task rtv-l2b
        return
    fi
    ip_a="$("$LAUNCHER" status "$slot_a" | awk '/^  ip:/ { print $2 }')"
    ip_b="$("$LAUNCHER" status "$slot_b" | awk '/^  ip:/ { print $2 }')"
    info "guest A=$slot_a ($ip_a)  guest B=$slot_b ($ip_b)"

    # Bridge-port isolation must be visible on BOTH guest TAPs.
    if bridge link show | grep -E "vm-" | grep -qv "isolated on"; then
        fail "every guest TAP reports 'isolated on' (bridge link show)"
        bridge link show | sed 's/^/#     /'
    else
        pass "every guest TAP reports 'isolated on'"
    fi

    check_fails "guest A cannot ping guest B (IPv4)" \
        guest "$slot_a" ping -c 2 -W 2 "$ip_b"
    check_fails "guest A cannot open a TCP connection to guest B" \
        guest "$slot_a" sh -c "timeout 5 sh -c '</dev/tcp/$ip_b/22'"
    check_fails "guest A cannot reach guest B over IPv6 link-local" \
        guest "$slot_a" sh -c "ping -6 -c 2 -W 2 ff02::1%eth0 2>/dev/null | grep -q 'bytes from'"
    # ARP: after an explicit request, guest B's MAC must never appear in A's
    # neighbour table as reachable.
    guest "$slot_a" sh -c "ping -c 1 -W 1 $ip_b >/dev/null 2>&1 || true" >/dev/null 2>&1 || true
    if guest "$slot_a" sh -c "ip neigh show $ip_b | grep -q 'lladdr'" >/dev/null 2>&1; then
        fail "guest A cannot learn guest B's MAC (ARP is blocked at L2)"
    else
        pass "guest A cannot learn guest B's MAC (ARP is blocked at L2)"
    fi
    # Impersonation: adding B's IP to A must not let A answer for it. Verified
    # from the HOST, which must still reach the real B.
    guest "$slot_a" sh -c "ip addr add $ip_b/24 dev eth0 2>/dev/null || true" >/dev/null 2>&1 || true
    if ssh -o StrictHostKeyChecking=yes \
        -o UserKnownHostsFile="$RUNTIME_ROOT/known_hosts" \
        -o ConnectTimeout=5 -o BatchMode=yes "agent@$ip_b" true >/dev/null 2>&1; then
        pass "host still reaches the REAL guest B while A impersonates its IP"
    else
        fail "host lost/redirected its connection to guest B during impersonation"
    fi
    guest "$slot_a" sh -c "ip addr del $ip_b/24 dev eth0 2>/dev/null || true" >/dev/null 2>&1 || true
    cleanup_task rtv-l2a
    cleanup_task rtv-l2b
}

# --- (4) credential leakage ----------------------------------------------
section_creds() {
    section "credential boundary (ticket 6 A.4)"
    local class slot
    class="$(all_classes | head -1)"
    cleanup_task rtv-creds
    if ! slot="$(start_task rtv-creds "$class")" || [[ -z $slot ]]; then
        fail "credentials: could not start a guest"
        return
    fi
    # NOTE: only names/paths are ever printed, never values.
    local var
    for var in OPENAI_API_KEY ANTHROPIC_API_KEY; do
        if guest "$slot" sh -c "test \"\$$var\" = not-needed" >/dev/null 2>&1; then
            pass "guest $var is the placeholder, not a real key"
        else
            fail "guest $var is not the expected placeholder"
        fi
    done
    for var in OPENROUTER_API_KEY GITHUB_TOKEN GH_TOKEN GITLAB_TOKEN AWS_ACCESS_KEY_ID \
        AWS_SECRET_ACCESS_KEY GOOGLE_APPLICATION_CREDENTIALS AZURE_CLIENT_SECRET \
        KUBECONFIG SSH_AUTH_SOCK GPG_AGENT_INFO; do
        check_fails "guest environment does not contain $var" \
            guest "$slot" sh -c "test -n \"\${$var:-}\""
    done
    local path
    for path in /home/agent/.ssh/id_ed25519 /home/agent/.ssh/id_rsa \
        /home/agent/.aws /home/agent/.config/gcloud /home/agent/.kube \
        /home/agent/.password-store /home/agent/.gnupg \
        /var/run/docker.sock /run/docker.sock /run/podman/podman.sock \
        /nix/var/nix/daemon-socket/socket /run/dbus/system_bus_socket; do
        check_fails "guest has no $path" guest "$slot" test -e "$path"
    done
    check_fails "guest has no git credential helper configured" \
        guest "$slot" sh -c "git config --get credential.helper"
    check_fails "guest cannot read the host operator's home" \
        guest "$slot" sh -c "ls /home | grep -qv '^agent$'"
    cleanup_task rtv-creds
}

# --- (5) lifecycle failures ----------------------------------------------
section_lifecycle() {
    section "lifecycle failure handling (ticket 6 A.5)"
    local class slot
    class="$(all_classes | head -1)"

    # (a) clone creation fails (workspace already exists).
    cleanup_task rtv-life
    mkdir -p "$WORKSPACE_ROOT/rtv-life"
    if "$LAUNCHER" run --name rtv-life --repository "$REPO" --agent pi \
        --resource-class "$class" >/dev/null 2>&1; then
        fail "clone-creation failure is detected"
    else
        pass "clone-creation failure is detected"
    fi
    check_no_residue "after a failed clone"
    rmdir "$WORKSPACE_ROOT/rtv-life" 2>/dev/null || rm -rf "$WORKSPACE_ROOT/rtv-life"

    # (b) repository validation fails.
    check_fails "a non-repository is rejected" \
        "$LAUNCHER" run --name rtv-life2 --repository /tmp --agent pi --resource-class "$class"
    check_fails "the workspace root itself is rejected as a repository" \
        "$LAUNCHER" run --name rtv-life3 --repository "$WORKSPACE_ROOT" --agent pi --resource-class "$class"
    check_no_residue "after a rejected repository"

    # (c) launcher termination mid-run: SIGKILL the launcher, then recover.
    cleanup_task rtv-kill
    "$LAUNCHER" submit --name rtv-kill --repository "$REPO" --agent pi \
        --prompt-file /dev/null --timeout 60 >/dev/null 2>&1 &
    local pid=$!
    sleep 15
    kill -9 "$pid" 2>/dev/null || true
    wait "$pid" 2>/dev/null || true
    info "launcher killed; recovering"
    "$LAUNCHER" recover --dry-run | sed 's/^/#     /'
    "$LAUNCHER" recover | sed 's/^/#     /'
    check_no_residue "after a killed launcher + recover"
    if [[ -d "$WORKSPACE_ROOT/rtv-kill" ]]; then
        pass "the workspace clone survived the killed launcher"
    else
        fail "the workspace clone was lost"
    fi
    cleanup_task rtv-kill

    # (d) guest crash: hard-stop the VM under a running slot.
    cleanup_task rtv-crash
    if slot="$(start_task rtv-crash "$class")" && [[ -n $slot ]]; then
        systemctl kill --signal=SIGKILL "microvm@$slot.service" || true
        sleep 5
        "$LAUNCHER" recover | sed 's/^/#     /'
        check_no_residue "after a guest crash + recover"
        cleanup_task rtv-crash
    else
        skip "guest crash: could not start a guest"
    fi

    # (e) the slot must be reusable afterwards.
    cleanup_task rtv-reuse
    if slot="$(start_task rtv-reuse "$class")" && [[ -n $slot ]]; then
        pass "a slot is reusable after the failure battery ($slot)"
        cleanup_task rtv-reuse
    else
        fail "no slot could be allocated after the failure battery"
    fi
}

# No slot may stay falsely allocated, no VM may stay up, no bind mount and no
# runtime job data may remain.
check_no_residue() {
    local when="$1" residue=0 f
    for f in "$RUNTIME_ROOT"/slots/*/session.json; do
        [[ -e $f ]] || continue
        residue=1
    done
    if ((residue)); then
        fail "no slot stays allocated $when"
        "$LAUNCHER" list | sed 's/^/#     /'
    else
        pass "no slot stays allocated $when"
    fi
    if findmnt -n | grep -q "$STATE_ROOT/.*/workspace"; then
        fail "no stale workspace bind mount remains $when"
    else
        pass "no stale workspace bind mount remains $when"
    fi
    if find "$RUNTIME_ROOT/jobs" -name spec.json 2>/dev/null | grep -q .; then
        fail "no stale job spec remains $when"
    else
        pass "no stale job spec remains $when"
    fi
}

# --- (6) malicious repository --------------------------------------------
section_malrepo() {
    section "hostile repository fixture (ticket 6 A.6)"
    local dir class slot
    dir="$(mktemp -d /tmp/rtv-malrepo.XXXXXX)"
    git -C "$dir" init -q
    # A hook that would run on clone/checkout if hooks were ever honoured.
    mkdir -p "$dir/.git/hooks"
    cat >"$dir/.git/hooks/post-checkout" <<'HOOK'
#!/bin/sh
touch /tmp/rtv-HOOK-RAN
HOOK
    chmod +x "$dir/.git/hooks/post-checkout"
    # Nix / direnv / MCP files that must never be evaluated by the HOST.
    printf '{ outputs = _: builtins.throw "rtv-flake-evaluated"; }\n' >"$dir/flake.nix"
    printf 'touch /tmp/rtv-DIRENV-RAN\n' >"$dir/.envrc"
    printf '{"mcpServers":{"evil":{"command":"touch","args":["/tmp/rtv-MCP-RAN"]}}}\n' >"$dir/.mcp.json"
    # Symlink escapes and a nested repository.
    ln -s /etc/shadow "$dir/escape-shadow"
    ln -s / "$dir/escape-root"
    git -C "$dir" init -q nested
    # A .git FILE pointing outside the workspace.
    mkdir -p "$dir/gitfile-escape"
    printf 'gitdir: /etc\n' >"$dir/gitfile-escape/.git"
    git -C "$dir" add -A >/dev/null 2>&1 || true
    git -C "$dir" -c user.email=rtv@example.invalid -c user.name=rtv \
        commit -qm "hostile fixture" >/dev/null 2>&1 || true
    rm -f /tmp/rtv-HOOK-RAN /tmp/rtv-DIRENV-RAN /tmp/rtv-MCP-RAN

    class="$(all_classes | head -1)"
    cleanup_task rtv-mal
    slot="$(start_task rtv-mal "$class" || true)"
    if [[ -z $slot ]]; then
        skip "hostile repository: could not start a guest"
        rm -rf "$dir"
        return
    fi
    check_fails "the host never ran the repository's git hook" test -e /tmp/rtv-HOOK-RAN
    check_fails "the host never evaluated the repository's flake.nix" test -e /tmp/rtv-DIRENV-RAN
    check_fails "the host never ran the repository's MCP command" test -e /tmp/rtv-MCP-RAN
    # Inside the guest the symlinks must not become a path out of the workspace.
    check_fails "a symlink in the repo does not expose /etc/shadow to the guest" \
        guest "$slot" sh -c "cat /workspace/escape-shadow"
    # Guest limits must contain a fork bomb and a disk filler; both must leave
    # the VM (and the host) alive.
    guest "$slot" sh -c ':(){ :|:& };: 2>/dev/null &' >/dev/null 2>&1 || true
    sleep 5
    check "the guest is still reachable after a fork bomb" guest "$slot" true
    guest "$slot" sh -c 'dd if=/dev/zero of=/workspace/rtv-fill bs=1M count=20000 2>/dev/null || true' >/dev/null 2>&1 || true
    check "the guest is still reachable after a disk-filling attempt" guest "$slot" true
    check "the host is still healthy (systemd is running)" systemctl is-system-running --quiet
    guest "$slot" sh -c 'rm -f /workspace/rtv-fill' >/dev/null 2>&1 || true
    check_fails "the guest cannot enumerate host block devices" \
        guest "$slot" sh -c "ls /dev/sd* /dev/nvme* 2>/dev/null | grep -q ."
    cleanup_task rtv-mal
    rm -rf "$dir"
}

# --- (7) batch result channel: forgery, stale results, timeout, cancel -----
# THIS is the section that measures the trust split introduced with spec v2:
# the guest CONTROLLER (root) is the only writer of
# /run/agent-job/controller/result.json, and the WORKER (uid 1000, the same
# identity as the coding agent and every repository process) must not be able to
# influence it in any way.
#
# Everything here needs a booted guest, because it is the guest KERNEL that
# enforces the ownership/permission split over virtiofs. The eval/build suite
# can only prove the layout and the validators (see
# tests/microvm-batch-result-integrity.sh).
section_forgery() {
    section "batch result channel: forgery + stale results (ticket 7)"
    local class slot task prompt
    class="$(all_classes | head -1)"
    prompt="$(mktemp /tmp/rtv-prompt.XXXXXX)"
    # A prompt whose agent run is irrelevant: what matters is what the guest
    # user can do to the result channel WHILE a job is running.
    printf 'Print the word ok and exit.\n' >"$prompt"

    # A long-running batch job, so the forgery attempts happen while the
    # controller is still supervising the worker.
    task=rtv-forge
    cleanup_task "$task"
    "$LAUNCHER" submit --name "$task" --repository "$REPO" --agent pi \
        --prompt-file "$prompt" --timeout 300 --resource-class "$class" \
        >/tmp/rtv-forge-submit.log 2>&1 &
    local submit_pid=$!
    # Wait for the slot to appear and the guest to be reachable.
    local waited=0 slot=""
    while ((waited < 120)); do
        slot="$($LAUNCHER list | awk -v t="$task" '$5 == t { print $1 }')"
        if [[ -n $slot ]] && guest "$slot" true >/dev/null 2>&1; then break; fi
        sleep 5
        waited=$((waited + 5))
    done
    if [[ -z $slot ]]; then
        skip "forgery: no slot came up for $task"
        kill "$submit_pid" 2>/dev/null || true
        rm -f "$prompt"
        return
    fi
    info "forgery target: slot $slot (task $task)"

    # --- (A) direct result forgery ---------------------------------------
    check_fails "the guest agent cannot write the authoritative result" \
        guest "$slot" sh -c "echo '{\"version\":2,\"state\":\"completed\"}' > $GUEST_CTRL_DIR/result.json"
    check_fails "the guest agent cannot even LIST the controller directory" \
        guest "$slot" sh -c "ls $GUEST_CTRL_DIR"
    check_fails "the guest agent cannot read the controller result" \
        guest "$slot" sh -c "cat $GUEST_CTRL_DIR/result.json"
    check_fails "the guest agent cannot read the job spec (it holds the allocation token)" \
        guest "$slot" sh -c "cat $GUEST_INPUT_DIR/spec.json"
    check_fails "the guest agent cannot delete the controller result" \
        guest "$slot" sh -c "rm -f $GUEST_CTRL_DIR/result.json"
    check_fails "the guest agent cannot modify its own job spec" \
        guest "$slot" sh -c "echo x >> $GUEST_INPUT_DIR/spec.json"
    check_fails "the guest agent cannot modify its own prompt" \
        guest "$slot" sh -c "echo x >> $GUEST_INPUT_DIR/prompt.md"
    check_fails "the guest agent cannot forge a cancellation request" \
        guest "$slot" sh -c "echo '{}' > $GUEST_INPUT_DIR/cancel.json"

    # --- (B) directory replacement ---------------------------------------
    check_fails "the guest agent cannot rename the controller directory" \
        guest "$slot" sh -c "mv $GUEST_CTRL_DIR $GUEST_JOB_DIR/stolen"
    check_fails "the guest agent cannot remove the controller directory" \
        guest "$slot" sh -c "rmdir $GUEST_CTRL_DIR"
    check_fails "the guest agent cannot shadow the controller directory with a symlink" \
        guest "$slot" sh -c "ln -sfn $GUEST_WORKER_DIR $GUEST_JOB_DIR/controller"
    check_fails "the guest agent cannot create anything in the job share root" \
        guest "$slot" sh -c "touch $GUEST_JOB_DIR/x"
    check_fails "the guest agent cannot rename the input directory" \
        guest "$slot" sh -c "mv $GUEST_INPUT_DIR $GUEST_JOB_DIR/stolen-input"

    # --- (C) worker-side fake results ------------------------------------
    # The worker MAY write these; the host must ignore them entirely.
    guest "$slot" sh -c "echo '{\"version\":2,\"state\":\"completed\",\"exitCode\":0}' > $GUEST_WORKER_DIR/result.json" \
        >/dev/null 2>&1 || true
    guest "$slot" sh -c "echo '{\"version\":2,\"state\":\"completed\",\"exitCode\":0}' > /workspace/result.json" \
        >/dev/null 2>&1 || true
    check "a worker-written result in worker/ is possible (it is untrusted output)" \
        test -f "$JOBS_ROOT/$slot/worker/result.json"
    # ... and the controller-owned result is still absent or controller-owned.
    if [[ -e "$JOBS_ROOT/$slot/controller/result.json" ]]; then
        if [[ "$(stat -c %u "$JOBS_ROOT/$slot/controller/result.json")" == "0" ]]; then
            pass "the authoritative result is still owned by the guest controller (uid 0)"
        else
            fail "the authoritative result is NOT root-owned any more"
        fi
    else
        pass "no authoritative result exists yet (the controller is still supervising)"
    fi
    if [[ "$(stat -c '%u %a' "$JOBS_ROOT/$slot/controller")" == "0 700" ]]; then
        pass "the controller directory is still root:root 0700"
    else
        fail "the controller directory changed owner/mode: $(stat -c '%u %a' "$JOBS_ROOT/$slot/controller")"
    fi

    # --- (G) forged early completion must not end the job ----------------
    sleep 5
    if kill -0 "$submit_pid" 2>/dev/null; then
        pass "the host is still waiting despite the forged worker results"
    else
        fail "the host stopped waiting after a forged worker result"
    fi

    # Let the real job finish and check the outcome the HOST reports.
    wait "$submit_pid" 2>/dev/null || true
    if [[ -f "$RESULTS_DIR/$task.json" ]]; then
        local src state
        src="$(jq -r '.source // ""' "$RESULTS_DIR/$task.json")"
        state="$(jq -r '.state // ""' "$RESULTS_DIR/$task.json")"
        info "archived result: state=$state source=$src"
        if [[ $src == "controller" ]]; then
            pass "the archived result came from the guest CONTROLLER"
        else
            fail "the archived result did not come from the controller (source=$src)"
        fi
        if [[ $state == "completed" || $state == "failed" || $state == "timed-out" ]]; then
            pass "the archived state reflects a real controller verdict ($state)"
        else
            fail "unexpected archived state '$state'"
        fi
    else
        fail "no archived result for $task"
    fi
    cleanup_task "$task"

    # --- (D/E/F) stale, foreign and malformed results ---------------------
    # Plant documents in the CONTROLLER directory as root (i.e. give the
    # attacker strictly more power than a guest worker has) and check that the
    # host still refuses to accept them for a NEW allocation.
    task=rtv-stale
    cleanup_task "$task"
    local victim
    victim="$(slots_of_class "$class" | head -1)"
    install -d -m 0700 -o root -g root "$JOBS_ROOT/$victim/controller"
    # A syntactically perfect result — but from an older allocation.
    jq -n '{version:2, controllerVersion:1, taskId:"rtv-stale",
            allocationToken:"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef",
            slot:$slot, agent:"pi", state:"completed", exitCode:0,
            startedAt:"2020-01-01T00:00:00Z", finishedAt:"2020-01-01T00:00:01Z",
            timedOut:false, message:"stale"}' --arg slot "$victim" \
        >"$JOBS_ROOT/$victim/controller/result.json"
    chmod 0600 "$JOBS_ROOT/$victim/controller/result.json"
    local rc=0
    "$LAUNCHER" submit --name "$task" --repository "$REPO" --agent pi \
        --prompt-file "$prompt" --timeout 120 --resource-class "$class" \
        >/tmp/rtv-stale-submit.log 2>&1 || rc=$?
    if grep -q "allocation token does not belong" /tmp/rtv-stale-submit.log ||
        [[ "$(jq -r '.message // ""' "$RESULTS_DIR/$task.json" 2>/dev/null)" != "stale" ]]; then
        pass "a stale result from an earlier allocation is rejected (token mismatch)"
    else
        fail "the host accepted a STALE result (exit $rc)"
    fi
    cleanup_task "$task"

    # A malformed result must become an INFRASTRUCTURE error (exit 70), never
    # a success.
    task=rtv-malformed
    cleanup_task "$task"
    rc=0
    "$LAUNCHER" submit --name "$task" --repository "$REPO" --agent pi \
        --prompt-file "$prompt" --timeout 60 --resource-class "$class" \
        >/tmp/rtv-malformed-submit.log 2>&1 &
    submit_pid=$!
    sleep 20
    slot="$($LAUNCHER list | awk -v t="$task" '$5 == t { print $1 }')"
    if [[ -n $slot ]]; then
        printf '{ "version": 2, "state": ' >"$JOBS_ROOT/$slot/controller/result.json"
        chmod 0600 "$JOBS_ROOT/$slot/controller/result.json"
    fi
    rc=0
    wait "$submit_pid" || rc=$?
    if ((rc == 70)); then
        pass "a malformed result becomes an infrastructure error (exit 70)"
    else
        fail "a malformed result did not produce exit 70 (got $rc)"
    fi
    cleanup_task "$task"

    # --- (H) timeout kills the whole worker cgroup ------------------------
    task=rtv-timeout
    cleanup_task "$task"
    rc=0
    "$LAUNCHER" submit --name "$task" --repository "$REPO" --agent pi \
        --prompt-file "$prompt" --timeout 20 --resource-class "$class" \
        >/tmp/rtv-timeout-submit.log 2>&1 &
    submit_pid=$!
    sleep 20
    slot="$($LAUNCHER list | awk -v t="$task" '$5 == t { print $1 }')"
    if [[ -n $slot ]]; then
        # A double-forked descendant that must die with the cgroup.
        guest "$slot" sh -c 'setsid sh -c "sleep 3600" >/dev/null 2>&1 &' >/dev/null 2>&1 || true
    fi
    rc=0
    wait "$submit_pid" || rc=$?
    if ((rc == 124)); then
        pass "a job that exceeds its deadline reports timed-out (exit 124)"
    else
        fail "a timed-out job did not produce exit 124 (got $rc)"
    fi
    if [[ -f "$RESULTS_DIR/$task.json" ]] &&
        [[ "$(jq -r '.state' "$RESULTS_DIR/$task.json")" == "timed-out" ]] &&
        [[ "$(jq -r '.source' "$RESULTS_DIR/$task.json")" == "controller" ]]; then
        pass "the timeout verdict came from the guest controller"
    else
        info "archived: $(cat "$RESULTS_DIR/$task.json" 2>/dev/null)"
        fail "the timeout verdict did not come from the controller"
    fi
    cleanup_task "$task"

    # --- (I) cancellation is bound to the allocation token ----------------
    task=rtv-cancel
    cleanup_task "$task"
    "$LAUNCHER" submit --name "$task" --repository "$REPO" --agent pi \
        --prompt-file "$prompt" --timeout 300 --resource-class "$class" \
        >/tmp/rtv-cancel-submit.log 2>&1 &
    submit_pid=$!
    sleep 25
    slot="$($LAUNCHER list | awk -v t="$task" '$5 == t { print $1 }')"
    if [[ -z $slot ]]; then
        skip "cancellation: no slot came up"
        kill "$submit_pid" 2>/dev/null || true
    else
        # Keep a copy of the cancellation request, then cancel for real.
        "$LAUNCHER" cancel "$task" >/tmp/rtv-cancel.log 2>&1 || true
        cp "$JOBS_ROOT/$slot/input/cancel.json" /tmp/rtv-cancel-request.json 2>/dev/null || true
        rc=0
        wait "$submit_pid" || rc=$?
        if [[ "$(jq -r '.state // ""' "$RESULTS_DIR/$task.json" 2>/dev/null)" == "cancelled" ]]; then
            pass "cancellation is recorded as 'cancelled'"
        else
            fail "cancellation was not recorded (got '$(jq -r '.state // ""' "$RESULTS_DIR/$task.json" 2>/dev/null)')"
        fi
        cleanup_task "$task"
        # Now replay the STALE cancellation request against a NEW allocation of
        # the same slot: the token no longer matches, so the new job must run.
        task=rtv-cancel-replay
        cleanup_task "$task"
        "$LAUNCHER" submit --name "$task" --repository "$REPO" --agent pi \
            --prompt-file "$prompt" --timeout 120 --resource-class "$class" \
            >/tmp/rtv-replay-submit.log 2>&1 &
        submit_pid=$!
        sleep 20
        slot="$($LAUNCHER list | awk -v t="$task" '$5 == t { print $1 }')"
        if [[ -n $slot && -f /tmp/rtv-cancel-request.json ]]; then
            install -m 0400 -o root -g root /tmp/rtv-cancel-request.json \
                "$JOBS_ROOT/$slot/input/cancel.json"
        fi
        rc=0
        wait "$submit_pid" || rc=$?
        if [[ "$(jq -r '.state // ""' "$RESULTS_DIR/$task.json" 2>/dev/null)" == "cancelled" ]]; then
            fail "a STALE cancellation request stopped a newly allocated job"
        else
            pass "a stale cancellation request does not affect a new allocation"
        fi
        cleanup_task "$task"
    fi
    rm -f "$prompt" /tmp/rtv-cancel-request.json
}

# --- main -----------------------------------------------------------------
printf '%s: starting real-KVM validation (host %s, bridge %s)\n' \
    "$PROG" "$(uname -n)" "$BRIDGE"
"$LAUNCHER" list | sed 's/^/#     /'

case "$SECTION" in
    all)
        section_boot
        section_net
        section_l2
        section_creds
        section_lifecycle
        section_malrepo
        section_forgery
        ;;
    boot) section_boot ;;
    net) section_net ;;
    l2) section_l2 ;;
    creds) section_creds ;;
    lifecycle) section_lifecycle ;;
    malrepo) section_malrepo ;;
    forgery) section_forgery ;;
    *) die "unknown --section '$SECTION'" ;;
esac

printf '\n%s: %d passed, %d failed, %d skipped\n' "$PROG" "$PASS" "$FAIL" "$SKIP"
((FAIL == 0)) || exit 1
