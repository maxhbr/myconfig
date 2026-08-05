# Podman + gVisor coding-agent worktrees on NixOS

This starter registers Nixpkgs' `gvisor` package as Podman's `runsc` OCI
runtime and provides `agent-session`, a rootless session manager.

## 1. Install

Copy the directory into the NixOS configuration repository and import:

```nix
{
  imports = [ ./gvisor-agent-sandbox/nixos/agent-sandboxes.nix ];

  # Replace with the user that launches sessions.
  users.users.alice.autoSubUidGidRange = true;
}
```

Rebuild, log out/in if subordinate ID mappings changed, then verify:

```bash
podman --runtime=runsc run --rm docker.io/library/alpine:latest uname -a
```

## 2. Build an agent image

The included image is deliberately generic. Add the desired coding-agent CLI
at build time, or use an existing internal image:

```bash
podman build -t localhost/agent-dev:latest -f Containerfile .
```

Do not put API tokens in the image.

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
