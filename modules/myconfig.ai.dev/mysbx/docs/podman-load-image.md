# mysbx podman-load-image

Load the agent container image into the caller's Podman store.

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

## Options

- `--force`: Reload the image unconditionally, even if the current build is already loaded
- `--test`: Report the image state without loading; exit 0 if current, 1 otherwise
- `--image <ref>`: Override the image reference (the pinned tarball is still loaded, then retagged; a path to an existing tarball file is also accepted)
- `--help`, `-h`: Show usage information

## Environment Variables

The Nix wrapper pins all three when the host builds a gVisor agent image (`myconfig.ai.dev.mysbx.podman.image`, built by [`../podman.nix`](../podman.nix) unless set to `null`):

- `MYSBX_PODMAN_TARBALL`: the docker-archive tarball to `podman load`
- `MYSBX_PODMAN_IMAGE`: the image reference the runs use
- `MYSBX_PODMAN_IMAGE_ID`: the expected image ID (config-blob digest, extracted from the tarball at build time) — the staleness check

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
- `MYSBX_PODMAN_PIDS_LIMIT` / `MYSBX_PODMAN_MEMORY` / `MYSBX_PODMAN_CPUS`:
  resource limits, only applied while cgroups are not ignored.
- `MYSBX_PODMAN_PASTA_SPEC`: the pasta network spec used instead of the
  default shared network (`network = false` still forces `none`).
  Pinned by the Nix wrapper with `--set-default` (so an invocation can
  still override it) as `pasta:--map-guest-addr,<address>` whenever the
  host runs the shared LiteLLM forwarder
  (`myconfig.ai.dev.litellm-forwarder`): that translation is what makes
  the host's loopback-only proxy reachable from inside the container,
  which the container's own `127.0.0.1` is not. The option behind the
  pin is `myconfig.ai.dev.mysbx.podman.pastaSpec`.

`backend = "podman-krun"` (backends.md D2 — the same builder under a
libkrun runtime) **runs** additionally read its OWN flag pins:

- `MYSBX_KRUN_RUNTIME`: the OCI runtime binary (crun built against
  libkrun, pinned by the Nix wrapper; the option is
  `myconfig.ai.dev.mysbx.krun.runtime`).
- `MYSBX_KRUN_CGROUP_MANAGER` / `MYSBX_KRUN_RUNTIME_FLAGS`: the krun
  variants of the two flags above, defaulting to NO runtime flags —
  crun has no `ignore-cgroups` option (that is a runsc flag), so the
  gvisor default would make crun die on an unknown flag. They are
  separate pins so an operator can configure one variant without
  breaking the other.
- `MYSBX_PODMAN_PIDS_LIMIT` / `MYSBX_PODMAN_MEMORY` / `MYSBX_PODMAN_CPUS`:
  SHARED with the gvisor variant (one "resource limits of the
  sandbox" setting per host) but mapped differently: the krun run
  turns them into the VM annotations `krun.cpus=<n>` / `krun.ram_mib=<m>`
  (crun's krun handler sizes the microVM from them, no cgroup
  dependency). What the annotation cannot express is REFUSED, not
  silently degraded: a pids limit (no pids controller is wired for a
  whole-VM "container"), a fractional CPU count (`krun.cpus` is a
  whole number of vCPUs, never rounded), a memory value below 128 MiB
  (crun silently defaults `krun.ram_mib <= 128`) or not a whole MiB.
- `MYSBX_PODMAN_ENV`: space-separated `KEY=VALUE` environment pins for
  this backend alone, emitted as `--env` after the config layers' `[env]`
  (a pin wins over a configured value) and before the sandbox's own
  `HOME`/`PATH`/XDG variables (which no pin can repoint). Entries
  without a `=` are ignored. The wrapper pins the model endpoint of the
  LiteLLM forwarder here (`OPENAI_BASE_URL` plus the per-agent variables
  the generated pi/opencode configurations read), because that URL is
  correct only inside a container. The option behind the pin is
  `myconfig.ai.dev.mysbx.podman.env`.
- `MYSBX_PODMAN_SHELL` / `MYSBX_PODMAN_TOOLS_PATH`: the payload shell
  and the tool `PATH` — **paths inside the container image**, not
  host store paths (the backend mounts nothing from the host
  `/nix/store`, so the bwrap pins `MYSBX_SHELL`/`MYSBX_TOOLS_PATH`
  must not and do not reach this backend's argv; bd myconfig-wao).
  Defaults: `/bin/bash` and `/bin:/usr/bin`, the gVisor agent image's
  own OCI config (`Cmd` / `Env`) — the same userland the agent-gvisor
  sessions run against. The Nix wrapper pins `MYSBX_PODMAN_SHELL` on
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

The multiplexer integration runs the entry script baked into the
image: `myconfig.ai.dev.mysbx.podman.muxEntries` (default: every
available entry except orca) is folded into `podman.imagePackages` and
pinned as `MYSBX_PODMAN_MUX_ENTRY_<VALUE>`. A multiplexer without such
a pin is a refused run naming the missing pin (the same refusal a bwrap
host without that multiplexer gets) — never a silent plain shell. The
private socket dir is `/mysbx-home/.mysbx-tmux` under podman-gvisor and
the guest's `/dev/shm/mysbx-tmux` under podman-krun. The TLS trust anchors come from the image itself (its
OCI env pins `SSL_CERT_FILE` & co.), not from a host CA-bundle bind.

**No writable nix store under `podman-gvisor`**: neither the host
daemon socket (the daemon-dir guard refuses it under a denied network,
like bwrap) nor a writable in-container store. Payloads that need nix
run under `podman-krun` (`krun.nix`, `design/backends.md` D2) or bwrap.

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
$ MYSBX_PODMAN_IMAGE=localhost/my-agent:dev mysbx podman-load-image
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
