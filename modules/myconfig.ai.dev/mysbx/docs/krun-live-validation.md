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

### 2.3 Nix inside the guest (bd myconfig-pz6)

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
