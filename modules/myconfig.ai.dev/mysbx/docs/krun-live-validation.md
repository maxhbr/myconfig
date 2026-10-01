# mysbx podman-krun live validation runbook

The scripted smoke + manual probes that validate the `podman-krun`
backend on a host with real `/dev/kvm` (f13). Everything here was
designed, source-verified and statically gated in bd myconfig-6di.5.1
– .5.8; what CANNOT be checked without a booting microVM is exactly
this list. Run it after `nixos-rebuild switch` with
`myconfig.ai.dev.mysbx.enable` and a sidecar/backend configuration in
place (see `docs/design/config.md`).

The scripted half lives next to this document:
`nix/krun-live-validation.sh` (built into the flake's checks? No —
it is a HOST script; run it from the repo checkout). It exits
non-zero on the first failed expectation and prints one `PASS`/
`FAIL` line per probe.

## 0. Preconditions

```bash
# rw access to /dev/kvm (the doctor gate refuses the run otherwise)
test -r /dev/kvm && test -w /dev/kvm

# the backend is configured and the image is loaded
mysbx --dry-run          # backend = "podman-krun" in the sidecar
mysbx podman-load-image  # the shared agent image, once per rebuild
```

The probes below assume the repo is a git checkout with a sidecar
(`mysbx init` already run) and `backend = "podman-krun"` set in
`.mysbx/config.toml` or the user layer.

## 1. The scripted smoke (automated)

```bash
./nix/krun-live-validation.sh /path/to/repo
```

Covers, in order: boot (a one-shot `true`), guest-kernel proof
(`uname -r`), exit-code propagation (`exit 42`), live-repo edit
(write through the workspace bind), ro rootfs (`EROFS` on `/`),
network=false enforcement (a positive egress control in the repo,
then real curl attempts — by hostname and by IP literal — from a
scratch repo whose sidecar sets `network = false`; a run mysbx
refuses counts as a FAIL, never as a denial), and the krun
annotations on the dry run. It needs `curl` in the image and egress
on the host. Each probe names the bd decision it
validates; a FAIL line quotes the decision document
(`docs/design/backends.md` D2).

## 2. The manual probes (the parts a script cannot see)

### 2.1 Mounts and state-dirs over virtio-fs (bd myconfig-6di.5.4)

```bash
# in one shell, inside the sandbox:
mysbx                            # interactive shell in the VM
git status                       # bd myconfig-zj2: NO dubious-ownership
                                 # failure — the krun run's trust file
                                 # names the workspace for git's
                                 # protected config
nix flake metadata .             # bd myconfig-jn0: NO "not owned by
                                 # current user" — libgit2 reads the
                                 # exact-path /etc/gitconfig
touch /synth-marker && echo hi > $HOME/.cache/marker
# from the HOST while the run is up:
ls -l <repo>/.mysbx/state        # the state-dirs sidecar backing
cat <repo>/synth-marker          # the live-repo bind is rw
```

Verify: files written in the sandbox appear host-uid-owned (the
virtiofs `set_creds` mapping, D2's uid row); the state-dirs bind
survives the run; a write to `/` fails `Read-only file system`.

### 2.2 Nested rootless podman (bd myconfig-6di.5.8)

Enable `myconfig.ai.dev.mysbx.krun.nestedPodman.enable`, rebuild,
reload the image, and inside the sandbox:

```bash
touch /tmp/m
podman run --rm docker.io/library/alpine true
# the storage wrapper mounted guest tmpfs (not virtiofs) for podman:
grep -E ' /(var/tmp|run)/containers tmpfs ' /proc/mounts
# nothing podman-owned was written to the virtio-fs share (-xdev skips
# the guest tmpfs mounts):
find /run /var/tmp /tmp -xdev -newer /tmp/m
podman run --rm docker.io/library/alpine sh -c 'echo hi > /data && cat /data'
# a non-root nested user writes inside its own (guest tmpfs) rootfs:
podman run --rm --user 1000 docker.io/library/alpine sh -c 'touch /tmp/x && id -u'
# egress + DNS from the nested container (netns = "host": the guest's TSI stack)
podman run --rm docker.io/library/alpine wget -qO- https://cache.nixos.org/nix-cache-info
podman run --rm docker.io/library/alpine nslookup cache.nixos.org
# for the network = false step below (storage does not survive the VM):
podman save -o alpine.tar docker.io/library/alpine
```

Verify: the pull works (network shared), both `/proc/mounts` lines
show `tmpfs`, `find` lists only `/run`, `/var/tmp` and the two
mountpoint directories in them, the runs succeed with `storage.conf`'s overlay driver
on those guest tmpfs mounts (no `EPERM` on layer creation), the
non-root run prints `1000`, and the nested container reaches the
network and resolves names without any bridge. Then set
`network = false` in the sidecar and start a new run: a pull fails
honestly (the documented failure mode), while
`podman load -i alpine.tar && podman run --rm docker.io/library/alpine true`
still works. If egress
fails, record whether `podman run --network=pasta` works instead —
that is the fallback the guest conf would switch to.

### 2.3 Nix inside the guest (bd myconfig-pz6, bd myconfig-0pi)

With `myconfig.ai.dev.mysbx.krun.nix.enable` (on by default under
`myconfig.ai.dev`), rebuild, reload the
image, and run `MYSBX_PODMAN_MEMORY=8g mysbx`. Then, inside the sandbox
(as guest root):

```bash
readlink -f /bin/nix             # the mysbx-krun-guest-nix wrapper
nix store info                   # Store URL: local
grep ' /nix/store ' /proc/mounts # overlay, lowerdir=/nix/store,upperdir=/run/mysbx-nix/upper
nix path-info --all | wc -l      # the registered image closure
ls -l /run/mysbx-nix/state/db    # db.sqlite and schema copied; big-lock/reserved owned by root (recreated)
nix build nixpkgs#hello --no-link --print-out-paths
nix run nixpkgs#hello
df -h /run/mysbx-nix
```

Verify: every command succeeds. Only paths missing from the image are
fetched (for example, glibc is reused from the image). With the
default 1024 MiB VM, the first nix call warns about the VM size.

With `network = false`, `nix build nixpkgs#hello` fails on the fetch,
and a build from the image closure still works:

```bash
nix build --impure --no-link --print-out-paths --expr \
  'derivation { name = "t"; system = builtins.currentSystem; builder = "/bin/sh"; args = [ "-c" "echo ok > $out" ]; }'
```

#### 2.3.1 The disk-backed nix scratch (bd myconfig-0pi)

The probes that CANNOT run without a booting VM — the agent sandbox
records them here, f13 runs them (probe (a) answers the GO/NO-GO of the
whole disk scratch; a NO-GO reverts the default pin to tmpfs):

```bash
# (a) the guest kernel has the loop module and ext4: from the HOST,
#     check libkrunfw's kernel config for CONFIG_BLK_DEV_LOOP=y and
#     CONFIG_EXT4_FS=y (the store path the pinned libkrunfw carries),
#     then inside the sandbox:
ls /dev/loop*                    # losetup --find needs loop-control support
# (b) the disk scratch in action: on the HOST, before the run,
ls -l <repo>.mysbx/scratch/     # empty; a real run creates <pid>.img here
#     then start `mysbx` and, inside the sandbox (first nix call):
df -T /run/mysbx-nix              # Type: ext4 (NOT tmpfs), the loop device
losetup -a                        # /dev/loopN: [9995]:<pid>.img (deleted) — the attach worked, the path is GONE
#     and on the HOST while the run is up:
ls -l <repo>.mysbx/scratch/     # EMPTY if the guest's best-effort rm worked; <pid>.img if the
                                # bind target was busy (record which — both are correct)
# (c) the crash gap: kill the VM (pkill the podman run from the
#     host), confirm the stale file stays named, then start the next
#     run — its startup sweep must remove the dead pid's file; with a
#     SECOND run still up, its <pid>.img must survive the sweep
# (d) probe (c) of the ORIGINAL plan, restated for the loop path: the
#     mount keeps working after the guest's rm — run nix build INSIDE
#     the same VM after the `losetup -a` above showed `(deleted)`, and
#     confirm host `du` shows the space freed after the VM exits
# (e) a rough speed comparison: `nix build nixpkgs#hello` timed with
#     the scratch pin dropped (tmpfs) vs set (ext4) — the disk buys
#     RAM headroom, the tmpfs buys speed; record both timings
# (f) the announced fallback: run with MYSBX_KRUN_SCRATCH_SIZE set to
#     an empty value — the first nix call must PRINT the tmpfs warning
#     line and /run/mysbx-nix must be tmpfs again
```

Verify: (b) is the core contract — ext4 on the loop device, the
state copy and the overlay on top of it. A failure of (a) (no loop module) or of (d)
(the unlink breaks the mount — virtiofsd would NOT keep the
unlinked-open file alive) is a NO-GO: set
`myconfig.ai.dev.mysbx.krun.nix.scratchSize` aside (pin the env var to
empty) and record the finding on bd myconfig-0pi — the tmpfs fallback
stays the mechanism, announced as ever.

### 2.4 Limits as VM annotations (bd myconfig-6di.5.6)

Set `MYSBX_PODMAN_CPUS`/`MYSBX_PODMAN_MEMORY` on the host (the
wrapper pins), and inside the sandbox:

```bash
nproc                            # == the annotation's whole vCPUs
grep MemTotal /proc/meminfo      # ~= the annotation's MiB
```

### 2.5 DNS over TSI (bd myconfig-6di.5.5)

```bash
getent hosts cache.nixos.org     # the VMM resolves from the netns
```

and with `network = false` in the sidecar, the same command must
FAIL (no route) — probe 6 of the script asserts the denial with
curl.

## 3. Recording

Append the results (host, date, kernel, PASS/FAIL per section) to
this file's sibling `krun-validation-log.md` (create it on the first
run). A FAIL invalidates the corresponding D2 row — file a bead
against the decision document, not against this runbook.

## 3. The direct backend's scratch disk (bd myconfig-dak.7, backends.md D7)

The probes a host with rw `/dev/kvm` runs for the DIRECT `krun`
backend's nix scratch — the virtio-blk half this agent sandbox could
not mount (host policy refuses ext4/overlay in the sandbox; the
guest kernel carries `EXT4_FS`/`OVERLAY_FS`/`VIRTIO_BLK` all `=y`).

Preconditions: the host pins `MYSBX_KRUN_SCRATCH_SIZE` (the Nix
wrapper sets it with `krun.nix.enable` — the SAME knob drives the
podman image's guest nix and the direct backend's scratch file; a
direct-only host sets the flag and the podman image is merely
passed, never built) and the direct launcher pin builds `withBlk`
(the default since bd myconfig-dak.7's first commit).

1. The announcement round-trip:

       ./nix/krun-direct-spike.sh  # or a plain run of the wrapped mysbx
       # inside the guest:
       ls /dev/vd*                 # exactly one device
       mount | grep mysbx-nix      # the ext4 scratch
       mount | grep 'overlay.*store-upper'   # the store overlay

2. Write-through: `touch /nix/store/probe` succeeds (the overlay's
   upper layer), and the file is GONE after the VM exits (the
   per-run scratch file is removed — nothing persists).

3. The honest absence: a run WITHOUT the size pin has no /dev/vd*,
   no overlay — the plain ro store share, writes to /nix/store
   fail EROFS.

4. RAM relief: `df /nix/store` shows the scratch ext4's size cap
   (not the VM's RAM-backed tmpfs); a `nix build` fills the disk,
   not the memory (watch the guest's MemAvailable).

