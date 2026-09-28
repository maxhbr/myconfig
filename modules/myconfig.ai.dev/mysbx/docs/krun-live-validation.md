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
mysbx gvisor-load-image  # the shared agent image, once per rebuild
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
network=false enforcement, and the dry-run shape of every krun
feature this backend adds. Each probe names the bd decision it
validates; a FAIL line quotes the decision document
(`docs/design/backends.md` D2).

## 2. The manual probes (the parts a script cannot see)

Nix builds inside the guest are not part of the krun variant
(`docs/design/backends.md` D2, *Dropped*).

### 2.1 Mounts and state-dirs over virtio-fs (bd myconfig-6di.5.4)

```bash
# in one shell, inside the sandbox:
mysbx                            # interactive shell in the VM
git status                       # bd myconfig-zj2: NO dubious-ownership
                                 # failure — the krun run's trust file
                                 # names the workspace for git's
                                 # protected config
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
podman run --rm docker.io/library/alpine true
podman run --rm docker.io/library/alpine sh -c 'echo hi > /data && cat /data'
# egress + DNS from the nested container (netns = "host": the guest's TSI stack)
podman run --rm docker.io/library/alpine wget -qO- https://cache.nixos.org/nix-cache-info
podman run --rm docker.io/library/alpine nslookup cache.nixos.org
```

Verify: the pull works (network shared), the run succeeds with
`storage.conf`'s overlay driver on the `/var/tmp`+`/run` tmpfs
roots, the nested container reaches the network and resolves names
without any bridge, and with `network = false` in the sidecar the
PULL fails honestly (the documented failure mode) while a local
`podman run` of an already-pulled image still works. If egress
fails, record whether `podman run --network=pasta` works instead —
that is the fallback the guest conf would switch to.

### 2.3 Limits as VM annotations (bd myconfig-6di.5.6)

Set `MYSBX_GVISOR_CPUS`/`MYSBX_GVISOR_MEMORY` on the host (the
wrapper pins), and inside the sandbox:

```bash
nproc                            # == the annotation's whole vCPUs
grep MemTotal /proc/meminfo      # ~= the annotation's MiB
```

### 2.4 DNS over TSI (bd myconfig-6di.5.5)

```bash
getent hosts cache.nixos.org     # the VMM resolves from the netns
```

and with `network = false` in the sidecar, the same command must
FAIL (no route) — the probe 1.6 of the script asserts the shape.

## 3. Recording

Append the results (host, date, kernel, PASS/FAIL per section) to
this file's sibling `krun-validation-log.md` (create it on the first
run). A FAIL invalidates the corresponding D2 row — file a bead
against the decision document, not against this runbook.
