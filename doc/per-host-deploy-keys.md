<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# Plan: per-host deploy keys, replacing the shared root key

Tracking: bd `myconfig-7zmd`.

## Problem

`modules/nixos.user.nix` copies the primary user's authorized keys onto
root:

```nix
root.openssh.authorizedKeys.keys =
  config.users.extraUsers."${myconfig.user}".openssh.authorizedKeys.keys;
```

`switch.sh` deploys as `root@<ip>` (`switch.sh:448`), so those keys are
not just an identity — they are the deploy credential. Consequences:

- Every host trusts the same key material, so compromising one user key
  gives root on every host, including vserver (the only host reachable
  from the internet).
- `PermitRootLogin = "no"` is therefore not available as a quick fix
  without breaking deployment.
- There is no per-host revocation: rotating means touching every host and
  every client at once.

The goal is one **deploy key per host**, held by the host that deploys it,
so that a leak is scoped to a single target and can be revoked
independently.

`hosts/metadata.json` already carries `managed_by` per host, which is the
existing source of truth for "who deploys what":

```
f13          (deploys p14, workstation, nas, vserver, nuc, ...)
p14
workstation
vserver
...
```

## Design

Three key sets, distinct roles:

| Key | Lives on | Authorized on | Purpose |
|---|---|---|---|
| `deploy_<host>` | deploying host, e.g. f13 | the deployed host, under **root** | `switch.sh` / `nixos-rebuild --target-host` |
| `id_ed25519` | the primary user everywhere | primary user only | interactive login, as today |
| `id_ed25519_no_pw` | hosts that push to others | target user (already used for r6c/thing/workstation) | pre-existing push paths — unchanged |

The new thing is the first row. It closes the root-equals-user-identity
overlap without changing who deploys what.

Two viable placements for the private key on the deploying host:

- **(A) `/etc/nix/deploy_<host>`, root-owned 0400, deployed via agenix.**
  Least change to `switch.sh`; the key is available to root at deploy
  time and nothing else. Revocation is "change the secret in priv, switch
  the target".
- **(B) `${HOME}/.ssh/deploy_<host>`, 0600, plus a `Host <host>` block in
  `programs.ssh.extraConfig` with `IdentitiesOnly yes`.** Mirrors the
  existing `setupAsBuildMachine` pattern (`flake.lib.nix:300-335`), which
  already generates exactly this shape for `nixBuild` with
  `sshKey = "/etc/nix/${name}"`. Preferable if deploys are ever run as a
  normal user rather than root.

Recommendation: **(A)**, because `switch.sh` already runs as root and the
existing `nixBuild` builder keys establish the `/etc/nix/` convention.
Follow the `setupAsBuildMachine` precedent for the ssh_config block.

## Phase 1 — create the keys (priv repo)

Add `scripts/priv.template/mk_deploy_keys.sh`, modelled on the existing
`mk_ssh_keypair.sh` (same `nix-shell` shebang style, same
`hosts/host.<name>/secrets/...` layout, same "already exists" guard).

Shape:

```bash
#!/usr/bin/env nix-shell
#! nix-shell -i bash -p openssh git

# Usage: mk_deploy_keys.sh <deploying-host> <target-host> [...]
#   mk_deploy_keys.sh f13 vserver
# or, for every host f13 manages according to hosts/metadata.json:
#   mk_deploy_keys.sh --all-managed f13

# For each target:
#   hosts/host.<deployer>/secrets/deploy/<target>/id_ed25519{,.pub}
#   ssh-keygen -t ed25519 -N "" -C "deploy@<deployer>-><target>"
```

Requirements the script must meet:

- **No passphrase** (`-N ""`), because `switch.sh` must deploy
  unattended. Document this, since it is the one security property being
  traded for automation.
- **One key per (deployer, target) pair**, not one per deployer — the
  point is per-target scoping.
- **`chmod 600`** on private keys, matching `umask 077` practice already
  used in the wireguard key comment
  (`hosts/host.vserver/service.wireguard-server/default.nix`).
- **Idempotent**, like `mk_ssh_keypair.sh`: refuse to overwrite.
- Enumerate targets from `hosts/metadata.json`'s `managed_by` rather than
  hard-coding a list, so `--all-managed` cannot drift.
- `git add` what it creates, so the priv repo records the public half.

Then commit in **priv**, which is where all key material lives (per
AGENTS.md: "All secrets must be stored in the separate `../priv/`
repository, never in this repo").

## Phase 2 — push the public halves, keep the shared key working

This phase must be **additive only**. Nothing is removed, so a mistake is
recoverable by simply not cutting over.

1. Publish the public keys into the public repo's `hosts/metadata.json`
   under a new namespace, e.g.
   `hosts.<target>.pubkeys."deploy.<deployer>.pub"`. Keeping them in the
   already-public metadata file is correct: public keys are public, and it
   keeps per-host authorization derivable during evaluation.

2. Add a reusable helper next to `setupAsBuildMachine` in `flake.lib.nix`:

   ```nix
   setupDeployKeys = deployers: {
     users.users.root.openssh.authorizedKeys.keys =
       map (d: metadata.hosts.${hostName}.pubkeys."deploy.${d}.pub") deployers;
   };
   ```

   `deployers` is the inverse of `managed_by`: the hosts whose
   `managed_by` is this host, plus this host itself if self-deploy is
   supported.

3. In each host config, add the deploy key to root's authorized keys
   **alongside** the existing inherited user key. Do not remove the
   inheritance yet — `modules/nixos.user.nix` still provides it and every
   existing deploy path still uses it.

4. In the deploying host's config, install the private key:
   - agenix secret per target (`myconfig.secrets."deploy-<deployer>-<target>"`),
     `dest = "/etc/nix/deploy_<target>"`, `owner = "root"`,
     `permissions = "0400"`;
   - `programs.ssh.extraConfig` block:
     ```
     Host <target>-deploy
         HostName <ip>
         User root
         IdentitiesOnly yes
         IdentityFile /etc/nix/deploy_<target>
         StrictHostKeyChecking accept-new
     ```

5. Deploy to every target with the **old** path (shared key). This is the
   step that lands the new public keys everywhere while nothing depends
   on them.

6. Verify the new keys work **before** anything depends on them, from the
   deploying host, explicitly forcing the new identity so there is no
   silent fallback:

   ```bash
   ssh -o IdentitiesOnly=yes -i /etc/nix/deploy_vserver \
       root@<vserver> 'echo ok; hostname'
   ```

   Do this for every target in `managed_by`. `-o IdentitiesOnly=yes` is
   essential: without it an agent or a default key can make the test pass
   while the new key is broken.

7. Only once every target answers on its own key, teach `switch.sh` to
   prefer it. Keep the old path as an explicit fallback for one release:

   ```bash
   # switch.sh, near line 448
   targetIP="root@$(get_ip_of_target "$target" "$use_wg")"
   deploy_key="/etc/nix/deploy_${target}"
   if [[ -r $deploy_key ]]; then
       ssh_opts="$ssh_opts -i $deploy_key -o IdentitiesOnly=yes"
   fi
   ```

   Note `switch.sh` already exports `NIX_SSHOPTS`; the `-i` must be in
   that variable (and in `copy_closure_to_target`'s ssh) for
   `nixos-rebuild --target-host` to inherit it.

## Phase 3 — stop deploying as root (optional, recommended)

Deploying as root is what forces root to hold a deploy key at all. Two
escapes, in increasing order of cleanliness:

1. **`nixos-rebuild --sudo`.** Keeps `root@` as the SSH user but runs the
   activation through sudo. Reduces what the deploy key can do
   interactively, does not remove root login.
2. **Deploy as the primary user with `--sudo`.** Change `targetIP` to
   `"${myconfig.user}@..."` and pass `--sudo`. Now the deploy key
   authorizes a normal account, `PermitRootLogin = "no"` becomes
   available, and root has no deploy identity at all.

Phase 3 is what actually delivers the "one key is not root" property. As
long as deploys are `root@`, a deploy key *is* a root credential — just a
narrower one. State this trade-off explicitly so it is a decision, not an
accident.

## Phase 4 — disable the shared root key

Gate on phase 2 step 6 having passed for every host, and on no remaining
consumer of the inherited key.

1. Audit every remote-root consumer before touching anything:

   ```bash
   grep -rn "root@" switch.sh scripts/ modules/ hosts/ | grep -v '\.md'
   grep -rn "authorizedKeys" modules/nixos.user.nix
   ```

   Include out-of-repo consumers: the priv `switch.sh`, any laptop
   `~/.ssh/config`, cron jobs, and Tailscale/wireguard bootstrap paths.

2. For hosts that are *not* deploy targets (desktop/laptop boxes you log
   into as yourself), drop the root-inheritance entirely:

   ```nix
   # modules/nixos.user.nix
   root.openssh.authorizedKeys.keys =
     lib.mkIf (!config.myconfig.isDeployTarget) []   # or just remove it
   ```

   A per-host option (e.g. `myconfig.deployTarget = true`) is cleaner
   than inferring from `managed_by`, because "is deployed remotely" and
   "is managed by a host" are not the same predicate.

3. On vserver specifically, once deploys use a deploy key or a non-root
   user, set:

   ```nix
   services.openssh.settings.PermitRootLogin = "no";
   ```

   and verify an interactive login as the primary user still works
   *before* closing the session that made the change.

4. Rotate: delete the old shared key from `metadata.json` /
   `nixos.user.nix`, `ssh-keygen -R` isn't needed (host keys unchanged),
   but remove the private half from every client that held it.

5. Re-verify from a fresh shell on the deploying host:

   ```bash
   ./switch.sh vserver          # must still work
   ssh -o IdentitiesOnly=yes -i /etc/nix/deploy_vserver root@<vserver>
   ssh root@<vserver>           # the shared key must now FAIL
   ```

## Rollback

Every phase is independently reversible:

- Phase 1–2 add material only; `git revert` the metadata/ssh_config
  commit and redeploy with the old path.
- Phase 2 step 7 (switch.sh) is behind an `-r $deploy_key` guard, so
  removing the key file restores the old behaviour without a code change.
- Phase 4 is the only destructive step. Do it in its own commit, and keep
  the old key in `metadata.json` for one release cycle after Phase 2
  passes, so recovery is one revert away.

## Open questions

1. **Self-deploy.** `switch.sh` handles `target == $(hostname)` by using
   `sudo` locally (the `direct_deploy_locally` path is commented out).
   Confirm whether a host ever deploys *itself* over SSH; if so it needs
   a `deploy_<self>` key too, or that path must stay on sudo.
2. **Jump host.** `ssh_opts` adds `-J vserver` when the target is not
   directly reachable. The jump hop authenticates with the *client's*
   keys, so the per-host deploy key only applies to the final hop —
   verify that `IdentitiesOnly` does not break the `-J` hop.
3. **Wireguard-only targets.** For hosts reachable only over wg0, the
   deploy key must be usable after the tunnel is up; confirm no ordering
   problem where the deploy needs the tunnel the deploy itself
   configures.
4. **`nixBuild` interaction.** `setupAsBuildMachine` already uses
   `/etc/nix/<name>` keys for distributed builds. Decide whether deploy
   keys share that directory (consistent) or a sibling
   (`/etc/nix/deploy/`, clearer separation).
