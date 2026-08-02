#!/usr/bin/env nix-shell
#! nix-shell -i bash -p openssh git coreutils
#
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Generate the dedicated SSH keypair that authorises ONLY the guest `agent`
# user in the myconfig.ai.microvm sandboxes (see ./docs/agent-microvm.md, §18).
#
# Split placement — the public key is committed, the private key never is:
#   - PRIVATE key -> $PRIV_ROOT/hosts/host.<hostname>/secrets/dedicated-agent-vm-key
#       (the separate ../priv secrets repo; NEVER committed to THIS repo)
#   - PUBLIC  key -> hosts/host.<hostname>/dedicated-agent-vm-key.pub
#       (committed in-repo; a public key is not a secret and is exactly what
#        ai.<host>.nix references via `myconfig.ai.microvm.sshPublicKeyFile`)
#
# Point the launcher at the private key at runtime (it must survive sudo's
# env_reset on the `run --attach` path — see workmux.nix / finding M2):
#   AGENT_MICROVM_SSH_KEY=$PRIV_ROOT/hosts/host.<hostname>/secrets/dedicated-agent-vm-key \
#       agent-microvm ssh <slot>
#
# Usage:
#   ./mk-dedicated-agent-vm-key.sh [<hostname>]
#
#   <hostname>  Short host name (defaults to the current machine's hostname).
#   PRIV_ROOT   Override the priv repo location (default: ~/myconfig/priv).
#
# Idempotent: refuses to overwrite an existing private key. To rotate, remove
# the old private key (and the committed .pub) first.

set -euo pipefail

hostname="${1:-$(hostname 2>/dev/null || cat /proc/sys/kernel/hostname)}"
priv_root="${PRIV_ROOT:-$HOME/myconfig/priv}"

repo_root="$(git rev-parse --show-toplevel)"

key_name="dedicated-agent-vm-key"
comment="agent-microvm@${hostname}"

priv_dir="$priv_root/hosts/host.${hostname}/secrets"
priv_key="$priv_dir/$key_name"

repo_host_dir="$repo_root/hosts/host.${hostname}"
pub_key="$repo_host_dir/${key_name}.pub"

if [[ ! -d $priv_root ]]; then
    echo "error: priv repo not found at '$priv_root'" >&2
    echo "       set PRIV_ROOT=/path/to/priv or clone it first." >&2
    exit 1
fi
if [[ ! -d $repo_host_dir ]]; then
    echo "error: host dir '$repo_host_dir' does not exist in this repo" >&2
    exit 1
fi
if [[ -e $priv_key ]]; then
    echo "error: private key already exists: $priv_key" >&2
    echo "       refusing to overwrite; remove it (and $pub_key) to rotate." >&2
    exit 1
fi

mkdir -p "$priv_dir"

# Generate the keypair directly into the priv secrets dir. ssh-keygen writes
# both "$priv_key" and "$priv_key.pub"; we move the public half into this repo
# so only the private half ever lives under $PRIV_ROOT.
ssh-keygen -t ed25519 -N "" -C "$comment" -f "$priv_key"
chmod 600 "$priv_key"

mv -f "$priv_key.pub" "$pub_key"

# Stage the committed public key in THIS repo. The private key lives in the
# separate priv repo and is committed there out-of-band (never here).
git -C "$repo_root" add "$pub_key"

echo
echo "done:"
echo "  private key (priv repo, NOT committed here): $priv_key"
echo "  public  key (staged in this repo):           $pub_key"
echo
echo "Set myconfig.ai.microvm.sshPublicKeyFile = ./dedicated-agent-vm-key.pub"
echo "in hosts/host.${hostname}/ai.${hostname}.nix (already wired for f13)."
echo
echo "Use the private key at runtime via:"
echo "  AGENT_MICROVM_SSH_KEY=$priv_key agent-microvm ssh <slot>"
echo "Commit the private key inside the priv repo separately."
