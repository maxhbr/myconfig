# Podman + gVisor coding-agent worktrees on NixOS

A flake-based starting point for container-based coding-agent isolation. It

- registers Nixpkgs' `gvisor` package as Podman's `runsc` OCI runtime,
- builds the sandbox image with Nix (no Containerfile / no `apt`),
- ships `agent-session`, a rootless session manager for parallel Git worktree
  sessions.

## Flake outputs

| Output | Description |
| --- | --- |
| `packages.<system>.agent-session` (`default`) | Session manager CLI |
| `packages.<system>.agent-sandbox-image` | OCI image tarball built by `dockerTools` |
| `packages.<system>.agent-sandbox-load-image` | Loads that image into rootless Podman |
| `apps.<system>.agent-session` / `.load-image` | `nix run` entry points |
| `nixosModules.default` | `programs.agentSandboxes` NixOS module |
| `overlays.default` | Adds the three packages to Nixpkgs |
| `devShells.<system>.default` | agent-session + podman + gvisor + shellcheck |
| `checks.<system>` | package builds and `shellcheck` |

Try it without installing anything. The image must be in the local Podman
store first, and the packaged CLI points `--runtime` at Nixpkgs' `runsc`
binary by default, so no `containers.conf` entry is required:

```bash
nix run github:you/gvisor-agent-sandbox#load-image
nix run github:you/gvisor-agent-sandbox -- start --name demo --repo "$HOME/src/example"
```

Set `AGENT_PODMAN_RUNTIME=runsc` to use a runtime *name* registered in
`containers.conf` (what the NixOS module configures) instead of the baked
absolute path.

Verify the whole chain (runtime, image, sandbox startup) with:

```bash
nix run .# -- doctor
```

## 1. Install on NixOS

```nix
{
  inputs.agent-sandbox.url = "github:you/gvisor-agent-sandbox";

  outputs = { nixpkgs, agent-sandbox, ... }: {
    nixosConfigurations.host = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        agent-sandbox.nixosModules.default
        {
          programs.agentSandboxes = {
            enable = true;
            users = [ "alice" ]; # subordinate UID/GID ranges for rootless Podman
          };
        }
      ];
    };
  };
}
```

The module enables Podman, registers `runsc`, and installs `agent-session`
plus `agent-sandbox-load-image`.

Rebuild, log out/in if subordinate ID mappings changed, then verify:

```bash
podman --runtime=runsc run --rm docker.io/library/alpine:latest uname -a
```

## 2. The agent image

The image is defined in [`nix/agent-image.nix`](nix/agent-image.nix) and is
deliberately generic: bash, coreutils, git, a C toolchain, Node.js, Python,
`jq`, `ripgrep`, `fd`, `curl`, CA certificates, and an SSH client.

Load it into the rootless Podman store of the user that runs sessions:

```bash
agent-sandbox-load-image          # skips work if the tag is already present
agent-sandbox-load-image --force  # reload after changing the image definition
```

`packages`, `extraPackages`, `imageName` and `imageTag` are overridable. Do
not put API tokens into the image.

### Adding your agent CLI

The image intentionally ships no agent CLI, so `claude`, `codex` or `pi` are
"command not found" until you add one. Do *not* try to bind-mount a host
binary: its whole Nix store closure would have to come along, and mounting
host `/nix` breaks the isolation boundary. Rebuild the image instead.

Via the NixOS module (`agent-session`'s default image reference follows the
override):

```nix
{ inputs, pkgs, ... }: {
  programs.agentSandboxes.image = pkgs.agent-sandbox-image.override {
    extraPackages = [
      pkgs.claude-code
      inputs.pi.packages.${pkgs.stdenv.hostPlatform.system}.default
    ];
  };
}
```

Ad hoc, without NixOS:

```bash
nix build --impure --expr '
  let
    self = builtins.getFlake (toString ./.);
    pkgs = self.packages.${builtins.currentSystem};
    pi = builtins.getFlake "github:badlogic/pi-mono";
  in pkgs.agent-sandbox-image.override {
    extraPackages = [ pi.packages.${builtins.currentSystem}.default ];
  }'
podman load --input ./result
```

The agent's own configuration stays outside the image; mount it per session
with `--config "$HOME/.pi:/home/agent/.pi:ro"`.

## 3. Start parallel sessions

```bash
agent-session start \
  --name parser-refactor \
  --repo "$HOME/src/example" \
  --base main \
  --config "$HOME/.claude:/home/agent/.claude:ro" \
  -- claude \
     "Refactor the parser and commit the result"

agent-session start \
  --name tests \
  --repo "$HOME/src/example" \
  --base main \
  --config "$HOME/.codex:/home/agent/.codex:ro" \
  --detach \
  -- codex exec "Improve parser coverage and commit the result"
```

Each session gets branch `agent/<name>`. The host checkout itself is not
mounted. A disposable bare Git pool is seeded from committed refs in the host
repository, and each session gets a worktree from that pool.

Image selection order: `--image`, then `$AGENT_SANDBOX_IMAGE`, then the
Nix-built default baked into the package (`$AGENT_SANDBOX_DEFAULT_IMAGE`).
Runtime selection order: `$AGENT_PODMAN_RUNTIME`, then the baked
`$AGENT_SANDBOX_DEFAULT_RUNTIME` (absolute `runsc` path).

`start` verifies the OCI runtime and the image *before* creating any worktree
or session state, so a misconfigured host leaves nothing behind.

## Troubleshooting

`agent-session doctor` reports the effective settings and starts a throwaway
sandbox container. Relevant knobs:

| Variable | Default | Purpose |
| --- | --- | --- |
| `AGENT_PODMAN_RUNTIME` | baked `runsc` path | Runtime name or absolute path |
| `AGENT_PODMAN_CGROUP_MANAGER` | `cgroupfs` rootless, Podman default as root | Podman cgroup manager |
| `AGENT_PODMAN_RUNTIME_FLAGS` | `ignore-cgroups` rootless, empty as root | Extra `runsc` flags, space separated |
| `AGENT_SANDBOX_IMAGE` | baked image ref | Image override |
| `AGENT_SANDBOX_STATE` | `$XDG_STATE_HOME/agent-sandbox` | State directory |

### Rootless cgroups

Two rootless defaults exist because of how `runsc` handles cgroups:

- `systemd error: Access denied ... interactive authentication` — Podman's
  default systemd cgroup manager makes `runsc` request a scope on the *system*
  bus, which polkit denies for a normal user. Hence `cgroupfs`.
- `cannot set up cgroup for root: open /sys/fs/cgroup/cgroup.subtree_control:
  permission denied` — with `cgroupfs`, `runsc` still tries to configure the
  root cgroup, which a rootless user cannot write. Hence `ignore-cgroups`.

**Consequence:** rootless sessions run without cgroup limits. `--memory`,
`--cpus` and `--pids-limit` are then *not* passed to Podman, and
`agent-session` warns on every start. To get enforcement back, unset
`AGENT_PODMAN_RUNTIME_FLAGS` and provide a cgroup hierarchy that `runsc` may
write (for example run the session manager as root, or delegate the needed
controllers).
- `container image ... is not in the local Podman store`: run
  `agent-sandbox-load-image` (or `nix run .#load-image`).
- Podman flags are re-applied to every container command, so `logs`, `shell`,
  `stop` and `destroy` also work with a path-based runtime.

## 4. Operate sessions

```bash
agent-session list
agent-session status tests
agent-session logs tests --follow
agent-session shell tests
agent-session stop tests
agent-session run tests --detach -- codex exec "Continue the task"
agent-session destroy tests
```

`destroy` refuses to remove a dirty worktree unless `--force` is supplied. It
preserves the branch by default. Add `--delete-branch` to remove it too.

To bring a result back to the host checkout, fetch from the pool printed by
`agent-session status NAME`, for example:

```bash
git fetch "$HOME/.local/state/agent-sandbox/pools/<repo-id>.git" \
  agent/parser-refactor:agent/parser-refactor
```

## Isolation boundaries

- Only the session worktree, disposable Git pool, session home, and explicit
  config mounts are visible to the sandbox.
- The root filesystem is read-only; Linux capabilities are dropped and
  `no-new-privileges` is set.
- Rootless sessions have **no cgroup limits** by default (see Troubleshooting),
  so a runaway agent can exhaust host CPU, memory and PIDs. This is a
  denial-of-service risk, not a confidentiality one.
- The image contains no Nix daemon and no host `/nix` mount; it only carries
  the closure of the selected packages.
- Agent configuration is read-only by default, but readable secrets can still
  be exfiltrated over the network. Use narrowly scoped tokens and an egress
  proxy/allowlist for stronger control.
- Never mount the Podman socket, SSH agent socket, host `/nix`, or the original
  repository's `.git` into an untrusted agent.
- Worktrees share the disposable Git pool. Concurrent normal Git operations are
  lock-safe, but a malicious agent can damage that pool. It cannot thereby
  mutate the original host checkout, which is why the pool is disposable.
- gVisor is defense in depth, not a VM boundary. Use a VM as the outer boundary
  for hostile multi-tenant workloads or secrets with a high impact radius.
