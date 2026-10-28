# mysbx podman-load-image

Load the shared agent image into the caller's Podman image storage.
Both `podman-gvisor` and `podman-krun` use it. Direct `krun` and
`microvm-qemu` do not need this command. See the
[current backend comparison](backend-comparison.md) for the different
store views, provisioning and validation limits.

## Synopsis

```
mysbx podman-load-image [--force|--test|--image <ref>|--help]
```

## Description

This command loads the agent container image that mysbx uses when configured with a podman backend (`podman-gvisor` or `podman-krun`).

The former name `mysbx gvisor-load-image` is kept as a hidden deprecated alias: it dispatches to this command and prints a deprecation notice pointing at `podman-load-image`.

Without options, the command:
1. Checks if the image is present in the local Podman store, comparing image IDs (config-blob digests) rather than tags
2. If missing or stale (different build), loads the tarball with `podman load`
3. Reports the state after loading
4. If the load replaced a stale build, removes that build by its image ID
   (`podman rmi <id>`, never forced). Podman refuses an image a container
   still uses or one that carries other tags; the command then keeps it,
   says so, and still exits 0

## Options

- `--force`: Reload the image unconditionally, even if the current build is already loaded
- `--test`: Report the image state without loading; exit 0 if current, 1 otherwise
- `--image <ref>`: Override the image reference (the pinned tarball is still loaded, then retagged; a path to an existing tarball file is also accepted)
- `--help`, `-h`: Show usage information

## Environment Variables

The Nix wrapper pins the first three when the host builds the shared agent image (`myconfig.ai.dev.mysbx.podman.image`, built by [`../podman.nix`](../podman.nix) unless set to `null`):

- `MYSBX_PODMAN_TARBALL`: the docker-archive tarball to `podman load`
- `MYSBX_PODMAN_IMAGE`: the image reference the runs use
- `MYSBX_PODMAN_IMAGE_ID`: the expected image ID (config-blob digest, extracted from the tarball at build time) — the staleness check
- `MYSBX_PODMAN`: the podman binary (not pinned; fallback: `podman` on PATH)

`--image` overrides the reference alone. With **no** pin and no `--image`, the command is a usage error (exit 2) instead of inventing a `localhost/...` reference: no registry serves the Nix-built image, so a `podman pull` fallback can never work.

### Run-only variables

These variables configure runs, not image loading. Some are wrapper
pins; others are per-invocation overrides.

#### Shared by both Podman backends

- `MYSBX_PODMAN_PASTA_SPEC` selects the Podman network specification.
  `network = false` still forces `none`. The wrapper uses
  `--set-default`, so an invocation can override it. When the host runs
  `myconfig.ai.dev.litellm-forwarder`, the default is
  `pasta:--map-guest-addr,<address>`. This maps the host's loopback
  forwarder into the container network. The option is
  `myconfig.ai.dev.mysbx.podman.pastaSpec`.
- `MYSBX_PODMAN_ENV` carries space-separated `KEY=VALUE` entries for
  both variants. They override configuration-layer `[env]` values,
  but infrastructure variables such as `HOME`, `PATH` and XDG paths
  override them. Entries without `=` are ignored. The wrapper pins
  LiteLLM endpoint variables here, including `OPENAI_BASE_URL` and
  per-agent endpoint variables. The option is
  `myconfig.ai.dev.mysbx.podman.env`.
- `MYSBX_PODMAN_PIDS_LIMIT`, `MYSBX_PODMAN_MEMORY` and
  `MYSBX_PODMAN_CPUS` override merged resource configuration. Their
  enforcement differs by runtime, as described below.
- `MYSBX_PODMAN_SHELL` and `MYSBX_PODMAN_TOOLS_PATH` name the payload
  shell and tools inside the image. Defaults are `/bin/bash` and
  `/bin:/usr/bin`. The wrapper pins the image's fish binary on fish
  hosts. Rendered fish configuration comes from `nix/shell-config.nix`
  (config.md D25). The tool path stays image-owned; `/bin` links the
  packages included in the image. There is no implicit host-store
  bind, so host paths outside the image do not become executable just
  because the host can resolve them.
- `MYSBX_PODMAN` selects the Podman binary, with `podman` on `PATH`
  as the fallback.

Both variants consume these endpoint/network pins. This source-level
wiring is not a live reachability result. Direct krun and QEMU have
different transports and do not consume the Podman endpoint pins.

#### `podman-gvisor` runtime variables

`MYSBX_GVISOR_CGROUP_MANAGER` selects `--cgroup-manager`. The rootless
default is `cgroupfs`; root omits the flag.

`MYSBX_GVISOR_RUNTIME_FLAGS` supplies space-separated runsc flags.
The rootless default is `ignore-cgroups`, because runsc cannot write
its pod's cgroup. With that flag, mysbx warns and omits all three
resource-limit flags. With cgroups enabled, it passes `--memory`,
`--cpus` and `--pids-limit`. Root defaults to no runtime flags.

#### `podman-krun` runtime variables

`MYSBX_KRUN_RUNTIME` selects crun built against libkrun. The wrapper
pins it from `myconfig.ai.dev.mysbx.krun.runtime`.
`MYSBX_KRUN_CGROUP_MANAGER` and `MYSBX_KRUN_RUNTIME_FLAGS` are separate
from the gVisor variables. The default runtime-flag list is empty;
`ignore-cgroups` is a runsc flag, not a crun flag.

CPU and RAM become the VM annotations `krun.cpus` and `krun.ram_mib`,
independent of cgroups. CPUs must be whole vCPUs, at least one. Memory
must resolve to whole MiB and be greater than 128 MiB. Invalid VM sizes
and an explicit `MYSBX_PODMAN_PIDS_LIMIT` override are refused.
A configured `pids-limit` instead warns and is dropped before the
builder. The claim that every unsupported request is refused is not
true for that configuration input.

### Stdio wiring

Both Podman backends run with mysbx's own
stdin/stdout/stderr, so the container is attached the same way
(`bd myconfig-jho`): `--interactive` is always passed — without it
podman closes the container's stdin, an interactive shell payload
reads instant EOF and exits 0 before any container shows up in
`podman ps` (the "exits immediately, no error" failure) — and
`--tty` is added when stdin is a terminal, so a piped one-shot
`run -- CMD` is not forced onto a pty. Under `--verbose` the exact
executed command is printed (`## exec:` / `## arg:` lines) before
the exec; podman's own stderr and exit code surface unchanged
(the exec inherits the streams), and a backend that cannot be
started at all is reported with exit 70.

The multiplexer integration runs the entry script baked into the
image: `myconfig.ai.dev.mysbx.podman.muxEntries` (default: every
available entry except orca) is folded into `podman.imagePackages` and
pinned as `MYSBX_PODMAN_MUX_ENTRY_<VALUE>`. A multiplexer without such
a pin is a refused run naming the missing pin (the same refusal a bwrap
host without that multiplexer gets) — never a silent plain shell. The
private socket dir is `/mysbx-home/.mysbx-tmux` under podman-gvisor and
the guest's `/dev/shm/mysbx-tmux` under podman-krun. The TLS trust anchors come from the image itself (its
OCI env pins `SSL_CERT_FILE` & co.), not from a host CA-bundle bind.

The default `podman-gvisor` layout has an image-owned store, no
writable-Nix overlay and no implicit host Nix-daemon bind. The dedicated
daemon-source guard rejects custom mounts exposing `/nix/var/nix`
when networking is denied; it is not a universal socket-mount ban.
Managed guest Nix is available through the provisioned `podman-krun`
image wrapper. Bubblewrap can use the host daemon with networking.
Direct VM writable Nix requires actual scratch provisioning, as the
[current guide](backend-comparison.md#direct-vm-store-and-writable-nix-choices)
explains.

## Image Sources

### Tarball (the pinned default)

`podman load` gives the image the reference recorded in the tarball's `RepoTags`; when the wanted reference differs (an explicit `--image` override), the image is retagged after the load, so the store serves it under both.

Example:
```bash
mysbx podman-load-image
```

### Explicit tarball path

A path to an existing file is accepted via `--image`:

```bash
mysbx podman-load-image --image /nix/store/...-agent-dev.tar.gz
```

## Exit Codes

- `0`: Success (image loaded or already current)
- `2`: Usage error (invalid options, repeated flags)
- `70`: Infrastructure error (podman unavailable, load failed)
- `1`: Test mode only (image is not current)

## Examples

### Check if image is loaded

```bash
$ mysbx podman-load-image --test
## image:    /nix/store/...-agent-dev.tar.gz
## ref:      localhost/agent-dev:latest
## expected: sha256:abc123...
## loaded:   sha256:abc123...
## state:    current
```

### Load the image

```bash
$ mysbx podman-load-image
## image:    /nix/store/...-agent-dev.tar.gz
## ref:      localhost/agent-dev:latest
## expected: sha256:abc123...
## loaded:   -
## state:    absent
loading /nix/store/...-agent-dev.tar.gz as localhost/agent-dev:latest (this may take a moment)...
## image:    /nix/store/...-agent-dev.tar.gz
## ref:      localhost/agent-dev:latest
## expected: sha256:abc123...
## loaded:   sha256:abc123...
## state:    current
```

### Force reload

```bash
$ mysbx podman-load-image --force
```

### Use custom image reference

```bash
$ mysbx podman-load-image --image localhost/my-agent:dev
```

## Integration and Workflow

### When to Run

The `podman-load-image` command should be run:

1. **Before starting mysbx with a podman backend**: The image must be loaded before mysbx can use it
2. **After rebuilding the agent image**: When the agent image is rebuilt, run this command to update the local copy
3. **As part of setup**: Include it in your development environment setup script

### Integration with the podman backends

When mysbx is configured with `backend = "podman-gvisor"` or `backend = "podman-krun"` in your sidecar config, it expects the agent image to be available in the local Podman store. This command ensures that image is present and up-to-date. Both backends run the SAME image — the container image is runtime-agnostic.

The workflow is:
1. Configure your sidecar: `backend = "podman-gvisor"`
2. Load the image: `mysbx podman-load-image`
3. Run mysbx normally: `mysbx` or `mysbx run -- CMD...`

### Automatic vs Manual

The command is **manual** — it is not run automatically by mysbx. This gives you explicit control over when images are loaded. However, failing to run it when the image is missing will cause mysbx to fail when trying to start the podman backend.

### What Happens If You Skip This Step

If you skip running `podman-load-image` and the image is not present:
- mysbx will fail to start the podman backend
- You'll see an error from podman indicating the image is missing
- Run `mysbx podman-load-image` to resolve the issue

## Implementation Notes

The command:

- Compares image IDs (digests) rather than tags to detect stale images
- Supports `--test` mode for CI/CD pipelines
- Provides detailed state reporting with `## ` prefix for consistency
- Handles errors gracefully with appropriate exit codes
- Validates sha256 digests (64 hex characters) to ensure manifest integrity
- Refuses instead of pulling when no tarball is configured — no registry
  serves the Nix-built image, so a `podman pull` fallback can never work

The command does not start a sandbox and therefore rejects global flags like `--verbose`, `--dry-run`, `--session`, `--ro`, and `--rw`.

## See Also

- `mysbx --help`: Main CLI help
- `podman-load(1)`: Podman image loading
- `mysbx` backend configuration: Set `backend = "podman-gvisor"` or `backend = "podman-krun"` in your sidecar config to use a podman backend
