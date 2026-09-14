# mysbx gvisor-load-image

Load the gVisor agent container image into the caller's Podman store.

## Synopsis

```
mysbx gvisor-load-image [--force|--test|--image <ref>|--help]
```

## Description

This command loads the gVisor agent container image that mysbx uses when configured with the `podman-gvisor` backend. It is integrated into the mysbx CLI for convenience, providing the same functionality as the Nix-built `agent-gvisor-load-image` helper.

Without options, the command:
1. Checks if the image is present in the local Podman store
2. If missing or stale (different build), loads it from a tarball or pulls the image reference
3. Reports the state after loading

## Options

- `--force`: Reload the image unconditionally, even if the current build is already loaded
- `--test`: Report the image state without loading; exit 0 if current, 1 otherwise
- `--image <ref>`: Override the image reference or tarball path
- `--help`, `-h`: Show usage information

## Environment Variables

- `MYSBX_GVISOR_IMAGE`: The image reference to load (default: `localhost/agent-gvisor:latest`)

When `--image` is not provided, the command uses `MYSBX_GVISOR_IMAGE` if set, otherwise falls back to the default `localhost/agent-gvisor:latest`.

## Image Sources

The command supports two types of image sources:

### Tarball Path

When the image reference is a path to an existing file (tarball), the command:
1. Extracts the image ID from the tarball's `manifest.json`
2. Compares it with the loaded image's ID
3. Loads the tarball if the image is absent or stale

Example:
```bash
mysbx gvisor-load-image --image /nix/store/...-agent-gvisor-image.tar.gz
```

### Image Reference

When the image reference is not a tarball path (e.g., `localhost/agent-gvisor:latest`), the command automatically falls back to `podman pull` to fetch the image. This is the default behavior when no tarball is provided.

Example:
```bash
mysbx gvisor-load-image --image localhost/agent-gvisor:latest
# or simply (uses default):
mysbx gvisor-load-image
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
image:    localhost/agent-gvisor:latest
ref:      localhost/agent-gvisor:latest
expected: -
loaded:   sha256:abc123...
state:    current
```

### Load the image

```bash
$ mysbx gvisor-load-image
image:    /nix/store/...-agent-gvisor-image.tar.gz
ref:      localhost/agent-gvisor:latest
expected: sha256:abc123...
loaded:   -
state:    absent
loading /nix/store/...-agent-gvisor-image.tar.gz as localhost/agent-gvisor:latest
...
image:    /nix/store/...-agent-gvisor-image.tar.gz
ref:      localhost/agent-gvisor:latest
expected: sha256:abc123...
loaded:   sha256:abc123...
state:    current
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
- Automatically falls back to `podman pull` for image references
- Validates sha256 digests (64 hex characters) to ensure manifest integrity

The command does not start a sandbox and therefore rejects global flags like `--verbose`, `--dry-run`, `--session`, `--ro`, and `--rw`.

## See Also

- `mysbx --help`: Main CLI help
- `agent-gvisor-load-image(1)`: The Nix-built helper (when available)
- `podman-load(1)`: Podman image loading
- `mysbx` backend configuration: Set `backend = "podman-gvisor"` in your sidecar config to use the gVisor backend
