# How-to: develop a feature with a gVisor agent sandbox

This walks through one complete session — start a sandboxed worktree, let an
agent do the work, bring the result back to the host checkout, then clean up.
It assumes the module is enabled on the host and the image is loaded (see
`../README.md` for first-run setup; verify with `agent-gvisor doctor`).

## 1. Start a session (creates the branch)

`agent-gvisor start` is the single entry point. It never mounts the host
checkout. Instead it seeds a **disposable bare Git pool** from the host repo's
committed refs and checks out a worktree from that pool on a fresh branch,
defaulting to `agent/<name>`.

```bash
agent-gvisor start \
  --name fix-parser \
  --repo ~/src/myconfig \
  --base main \
  --env-file ~/.config/agent-gvisor/litellm.env \
  --env OPENAI_API_KEY="$OPENAI_API_KEY" \
  -- pi "Refactor the parser and commit the result"
```

What happens:

1. The OCI runtime and image are checked **before** any state is created, so a
   misconfigured host leaves nothing behind.
2. `/home/agent` is seeded from the **activated home-manager generation** of
   the calling user (the agent's skills, prompts and settings come along). The
   loopback LiteLLM URLs in those files are rewritten to the
   sandbox-reachable endpoint `http://192.168.84.1:14000` — the
   `--map-guest-addr` address pasta translates to the host, where a
   port-scoped forwarder proxies to the loopback-only LiteLLM proxy (see
   *Model access (host LiteLLM)* in `../README.md`).
3. A bare pool is created (or reused) at
   `~/.local/state/agent-gvisor/pools/<repo-id>.git`, a worktree is added at
   `<repo>_agent-gvisor/fix-parser` — i.e. next to the host repository — on
   branch `agent/fix-parser`, and the container starts.
4. `pi` runs as the default command (it was baked into the image because
   `myconfig.ai.pi-coding-agent.enable = true` on the host). Omit the trailing
   `-- COMMAND` to drop into `herdr` (the configured `defaultCommand`), or pass
   `-- bash` for a plain shell.

If a session named `fix-parser` already exists, an interactive terminal asks
whether to destroy it (and its branch); pass `--force` to do so unattended,
or run `agent-gvisor destroy fix-parser --force --delete-branch` first.

## 2. Interact with the agent

The agent works **inside** the sandbox and commits to branch `agent/fix-parser`
in the disposable pool. The host checkout is untouched.

Inspect progress from the host:

```bash
agent-gvisor list                       # all sessions, status + branch
agent-gvisor status fix-parser          # pool/worktree/container + git status
agent-gvisor logs fix-parser --follow   # container stdout/stderr
```

Drop into the running sandbox for an interactive look (the worktree is mounted
at the same absolute path inside as outside, so the agent's paths line up):

```bash
agent-gvisor shell fix-parser           # a shell in the container
agent-gvisor shell fix-parser -- pi "now add tests"
```

Run another command against the same session (it refuses if the container is
already running unless you `stop` it first):

```bash
agent-gvisor run fix-parser --detach -- pi "fix the failing test"
```

The agent's commits live only in the pool — they are **not** in the host repo
yet.

## 3. Merge the result back

Use the built-in `merge` subcommand to fetch the session branch from its
disposable pool into the **original** host repository and merge it into the
branch you currently have checked out. The host repo is never mounted into
the sandbox, so this is the path the work takes back.

```bash
cd ~/src/myconfig
```

Switch to the target branch (e.g. `main`), make sure the tree is clean,
then:

```bash
agent-gvisor merge fix-parser
```

It defaults to `--no-ff` (a merge commit) so the feature work stays
traceable; pass `--ff` or `--squash` for the matching `git merge` behaviour,
or `--repo PATH` to target a different clone. The session worktree is left
untouched — tear it down afterwards (step 4).

## 4. Clean up

Once the result is in the host checkout, tear the session down. `destroy`
refuses a **dirty** worktree unless `--force` is given, and preserves the
branch by default — add `--delete-branch` to remove it from the pool too:

```bash
agent-gvisor stop fix-parser                            # stop the container
agent-gvisor destroy fix-parser --delete-branch         # worktree + branch
```

`--force --delete-branch` skips the dirty-check and removes everything. The
disposable pool itself is shared across sessions for the same repo, so it
stays until its last session is destroyed.

## Quick reference

| Step | Command |
| --- | --- |
| Start / create branch | `agent-gvisor start --name N --repo R --base main -- pi …` |
| List / status | `agent-gvisor list` · `agent-gvisor status N` |
| Watch output | `agent-gvisor logs N --follow` |
| Enter sandbox | `agent-gvisor shell N` |
| Run more work | `agent-gvisor run N --detach -- pi …` |
| Fetch result | `agent-gvisor merge N` (into the current branch) |
| Stop container | `agent-gvisor stop N` |
| Destroy | `agent-gvisor destroy N --delete-branch` |
| Verify host setup | `agent-gvisor doctor` |
