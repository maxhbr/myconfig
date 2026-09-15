# mysbx gvisor-load-image

Load the gVisor agent container image into the caller's Podman store.

## Synopsis

```
mysbx gvisor-load-image [--force|--test|--image <ref>|--help]
```

## Description

This command loads the gVisor agent container image that mysbx uses when configured with the `podman-gvisor` backend. It is integrated into the mysbx CLI for convenience, providing the same functionality as the Nix-built `agent-gvisor-load-image` helper.

Without options, the command:
1. Checks if the image is present in the local Podman store, comparing image IDs (config-blob digests) rather than tags
2. If missing or stale (different build), loads the tarball with `podman load`
3. Reports the state after loading

## Options

- `--force`: Reload the image unconditionally, even if the current build is already loaded
- `--test`: Report the image state without loading; exit 0 if current, 1 otherwise
- `--image <ref>`: Override the image reference (the pinned tarball is still loaded, then retagged; a path to an existing tarball file is also accepted)
- `--help`, `-h`: Show usage information

## Environment Variables

The Nix wrapper pins all three when the host builds a gVisor agent image (`myconfig.ai.dev.mysbx.gvisor.image` — by default the gvisor tier's image when that module is enabled):

- `MYSBX_GVISOR_TARBALL`: the docker-archive tarball to `podman load`
- `MYSBX_GVISOR_IMAGE`: the image reference the runs use
- `MYSBX_GVISOR_IMAGE_ID`: the expected image ID (config-blob digest, extracted from the tarball at build time) — the staleness check

`--image` overrides the reference alone. With **no** pin and no `--image`, the command is a usage error (exit 2) instead of inventing a `localhost/...` reference: no registry serves the Nix-built image, so a `podman pull` fallback can never work.

### Run-only variables (not set by the wrapper)

`backend = "podman-gvisor"` **runs** additionally read (empty/unset falls
back to the built-in defaults):

- `MYSBX_GVISOR_CGROUP_MANAGER`: the podman `--cgroup-manager` value.
  Rootless default: `cgroupfs`; root default: flag omitted.
- `MYSBX_GVISOR_RUNTIME_FLAGS`: space-separated runsc runtime flags
  (`--runtime-flag` each). Rootless default: `ignore-cgroups` (a
  rootless runsc cannot write its pod's cgroup — without it, `run`
  fails with `cannot set up cgroup for root`); root default: none.
  The flag `ignore-cgroups` also disables the `--pids-limit` /
  `--memory` / `--cpus` argv entries: runsc would not enforce them.
- `MYSBX_GVISOR_PIDS_LIMIT` / `MYSBX_GVISOR_MEMORY` / `MYSBX_GVISOR_CPUS`:
  resource limits, only applied while cgroups are not ignored.
- `MYSBX_GVISOR_PASTA_SPEC`: a pasta network spec overriding the
  default shared network (`network = false` still forces `none`).
- `MYSBX_GVISOR_SHELL` / `MYSBX_GVISOR_TOOLS_PATH`: the payload shell
  and the tool `PATH` — **paths inside the container image**, not
  host store paths (the backend mounts nothing from the host
  `/nix/store`, so the bwrap pins `MYSBX_SHELL`/`MYSBX_TOOLS_PATH`
  must not and do not reach this backend's argv; bd myconfig-wao).
  Defaults: `/bin/bash` and `/bin:/usr/bin`, the gVisor agent image's
  own OCI config (`Cmd` / `Env`) — the same userland the agent-gvisor
  sessions run against. The Nix wrapper pins `MYSBX_GVISOR_SHELL` on
  fish hosts to the fish binary as it exists inside the image (bd
  myconfig-cew: the image is provisioned with the host user's fish
  world, so an interactive session lands in the same shell, aliases
  and configuration, via the ro `~/.config/fish` mount); the tool
  `PATH` stays the image's own — `buildEnv` links every baked
  package's `bin` into the image `/bin`, which the OCI `PATH` already
  covers.
- `MYSBX_PODMAN`: the podman binary (fallback: `podman`).

### Stdio wiring

Every podman-gvisor run execs podman with mysbx's own
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

The multiplexer integration is **not available** under this backend
yet: no image ships an in-image entry script, so a config selecting
`multiplexer = "…"` is a refused run naming the missing pin (the same
refusal a bwrap host without that multiplexer gets) — never a silent
plain shell. The TLS trust anchors come from the image itself (its
OCI env pins `SSL_CERT_FILE` & co.), not from a host CA-bundle bind.

**No nix inside the sandbox**: the backend supports no nix — neither
the host daemon socket (the daemon-dir guard refuses it under a
denied network, like bwrap) nor a writable in-container store. The
gvisor tier's `--nix` volume mechanism is deliberately out of scope
here; the image ships no `nix` binary, and payloads that need one must
run under the gvisor tier or bwrap instead.

## Image Sources

### Tarball (the pinned default)

`podman load` gives the image the reference recorded in the tarball's `RepoTags`; when the wanted reference differs (an explicit `--image` override), the image is retagged after the load, so the store serves it under both.

Example:
```bash
mysbx gvisor-load-image
```

### Explicit tarball path

A path to an existing file is accepted via `--image`:

```bash
mysbx gvisor-load-image --image /nix/store/...-agent-dev.tar.gz
```

## Exit Codes

- `0`: Success (image loaded or already current)
- `2`: Usage error (invalid options, repeated flags)
- `70`: Infrastructure error (podman unavailable, load failed)
- `1`: Test mode only (image is not current)

## Examples

### Check if image is loaded

```bash
$ mysbx gvisor-load-image --test
## image:    /nix/store/...-agent-dev.tar.gz
## ref:      localhost/agent-dev:latest
## expected: sha256:abc123...
## loaded:   sha256:abc123...
## state:    current
```

### Load the image

```bash
$ mysbx gvisor-load-image
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
$ mysbx gvisor-load-image --force
```

### Use custom image reference

```bash
$ MYSBX_GVISOR_IMAGE=localhost/my-agent:dev mysbx gvisor-load-image
```

## Integration and Workflow

### When to Run

The `gvisor-load-image` command should be run:

1. **Before starting mysbx with the podman-gvisor backend**: The image must be loaded before mysbx can use it
2. **After rebuilding the agent image**: When the gVisor agent image is rebuilt, run this command to update the local copy
3. **As part of setup**: Include it in your development environment setup script

### Integration with podman-gvisor Backend

When mysbx is configured with `backend = "podman-gvisor"` in your sidecar config, it expects the gVisor agent image to be available in the local Podman store. This command ensures that image is present and up-to-date.

The workflow is:
1. Configure your sidecar: `backend = "podman-gvisor"`
2. Load the image: `mysbx gvisor-load-image`
3. Run mysbx normally: `mysbx` or `mysbx run -- CMD...`

### Automatic vs Manual

The command is **manual** — it is not run automatically by mysbx. This gives you explicit control over when images are loaded. However, failing to run it when the image is missing will cause mysbx to fail when trying to start the gVisor backend.

### What Happens If You Skip This Step

If you skip running `gvisor-load-image` and the image is not present:
- mysbx will fail to start the gVisor backend
- You'll see an error from podman indicating the image is missing
- Run `mysbx gvisor-load-image` to resolve the issue

## Implementation Notes

This command is a Rust reimplementation of the Nix-built `agent-gvisor-load-image` script, integrated into the mysbx CLI. It follows the same semantics:

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
- `agent-gvisor-load-image(1)`: The Nix-built helper (when available)
- `podman-load(1)`: Podman image loading
- `mysbx` backend configuration: Set `backend = "podman-gvisor"` in your sidecar config to use the gVisor backend
