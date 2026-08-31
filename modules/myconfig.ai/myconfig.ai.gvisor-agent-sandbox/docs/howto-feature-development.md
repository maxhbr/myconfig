# How-to: develop a feature with a gVisor agent sandbox

This walks through one complete session — start a sandboxed worktree, let an
agent do the work, bring the result back to the host checkout, then clean up.
It assumes the module is enabled on the host and the image is loaded (see
`../README.md` for first-run setup; verify with `agent-gvisor doctor`).

## 1. Start a session (creates the branch)

`agent-gvisor start` is the single entry point. The name may be positional
and the repository defaults to the current directory, so the shorthand
`agent-gvisor NAME` means `agent-gvisor start --name NAME --repo .`; run it
from the repository you want to work on. It never mounts the host
checkout. Instead every session gets its own **fully isolated
`git clone --no-hardlinks`** of the repository — no hardlinks, so the
sandbox can never write through to the host's object files — checked out
on the session branch, defaulting to `agent/gvisor/<name>`. If the
repository already has a branch with that name, the session continues it
at its tip; a new session branch otherwise starts at `--base` (default
`HEAD`). The branch belongs to the session clone and does not track
`origin`. Inside the container
the clone is mounted **at the original repository's path**, so the
in-container paths match the host ones.

```bash
agent-gvisor start \
  fix-parser \
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
3. The session state (meta, home, mounts, env) is created at
   `<repo>__agent-gvisor/__sessions/fix-parser/` — i.e. next to the host
   repository — the session's own clone is created at
   `<repo>__agent-gvisor/fix-parser` on branch `agent/gvisor/fix-parser`, and
   the container starts. A symlink in the session registry
   (`~/.local/state/agent-gvisor/sessions/fix-parser`) points to the session
   directory, so commands that only take a session name find it.
4. `pi` runs as the default command (it was baked into the image because
   `myconfig.ai.pi-coding-agent.enable = true` on the host). Omit the trailing
   `-- COMMAND` to drop into `herdr` (the configured `defaultCommand`), or pass
   `-- bash` for a plain shell.

If a session named `fix-parser` already exists, an interactive terminal asks
whether to destroy it (and its branch); pass `--force` to do so unattended,
or run `agent-gvisor destroy fix-parser --force --delete-branch` first.

## 2. Interact with the agent

The agent works **inside** the sandbox and commits to branch `agent/gvisor/fix-parser`
in its own clone. The host checkout is untouched.

Inspect progress from the host:

```bash
agent-gvisor list                       # all sessions, status + branch
agent-gvisor status fix-parser          # worktree/container + git status
agent-gvisor logs fix-parser --follow   # container stdout/stderr
```

Drop into the running sandbox for an interactive look (the worktree is mounted
at the original repo's path inside, so the agent's paths line up):

```bash
agent-gvisor shell fix-parser           # a shell in the container
agent-gvisor shell fix-parser -- pi "now add tests"
```

Run another command against the same session (it refuses if the container is
already running unless you `stop` it first):

```bash
agent-gvisor run fix-parser --detach -- pi "fix the failing test"
```

The agent's commits live only in the session clone — they are **not** in the
host repo yet.

## 3. Merge the result back

Use the built-in `merge` subcommand to fetch the session branch from its
worktree into the **original** host repository and merge it into the
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

Without merging, two related commands bring the branch out of the session
clone:

```bash
agent-gvisor fetch fix-parser   # into the repository you are in (--repo to
                                 # target another one); no merge, just the
                                 # local branch agent/gvisor/fix-parser
agent-gvisor push fix-parser     # fetch first, then git push origin
agent-gvisor push fix-parser myremote   # … or any other configured remote
```

`merge` runs the same fetch internally before merging.

## 4. Clean up

Once the result is in the host checkout, tear the session down. `destroy`
refuses a **dirty** worktree unless `--force` is given. The session branch
lives in the session clone, so `destroy` removes it with the clone. Add
`--delete-branch` to also remove a **host-local copy** of the branch when
one exists (e.g. one left behind by `fetch` or `push`); when the host has
no such branch — the normal case after `merge`, which already cleans up
its temporary ref — `--delete-branch` simply succeeds:

```bash
agent-gvisor stop fix-parser                            # stop the container
agent-gvisor destroy fix-parser --delete-branch         # session clone + host-local branch, if any
```

`--force --delete-branch` skips the dirty-check and removes everything
(container, session home, session clone, and a host-local branch copy if
one exists). A `--delete-branch` destroy that fails on a genuine Git
problem (for example a host-local branch that is currently checked out in
the host) keeps the session recoverable — resolve the cause and run it
again.

## Quick reference

| Step | Command |
| --- | --- |
| Start / create branch | `agent-gvisor start N --base main -- pi …` (from the repo; `agent-gvisor N` for short) |
| List / status | `agent-gvisor list` · `agent-gvisor status N` |
| Watch output | `agent-gvisor logs N --follow` |
| Enter sandbox | `agent-gvisor shell N` |
| Run more work | `agent-gvisor run N --detach -- pi …` |
| Fetch result | `agent-gvisor merge N` (into the current branch) |
| Fetch branch only | `agent-gvisor fetch N` (into the current repo) |
| Push branch | `agent-gvisor push N [REMOTE]` (fetches, then pushes) |
| Stop container | `agent-gvisor stop N` |
| Destroy | `agent-gvisor destroy N --delete-branch` |
| Verify host setup | `agent-gvisor doctor` |
