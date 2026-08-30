# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# `agent-gvisor` CLI specification — authoritative contract of the Rust rewrite
#
# This is the written contract the Rust implementation (../rust/) follows and
# the test suites (../rust/tests/, ../tests/agent-gvisor-cli-harness.sh)
# enforce. It was derived line-by-line from the original bash implementation
# (the historical bin/agent-gvisor, since deleted — see the git history);
# where the two disagree, this file wins and
# the difference is listed in "Dropped in the rewrite" below.
#
# The observable API is preserved byte-for-byte where users can script
# against it: subcommands, flags, env vars, the session-state layout, the
# podman argument vector and the error-message texts. Everything that existed
# only to keep sessions from PRE-rewrite layouts loadable is dropped (see the
# dedicated section).

## 1. Command surface

```
agent-gvisor [start] NAME [options] -- [COMMAND...]
agent-gvisor start --name NAME --repo PATH [options] -- [COMMAND...]
agent-gvisor list
agent-gvisor status NAME
agent-gvisor run NAME [--detach] -- [COMMAND...]
agent-gvisor shell NAME [COMMAND...]
agent-gvisor logs NAME [PODMAN-LOGS-ARGS...]
agent-gvisor stop NAME
agent-gvisor merge NAME [--no-ff] [--ff] [--squash] [--repo PATH] [GIT-MERGE-ARGS...]
agent-gvisor destroy NAME [--force] [--delete-branch]
agent-gvisor doctor
```

Dispatch (first argument):

| first argument | action |
| --- | --- |
| `start` | `cmd_start` on the remaining arguments |
| `list` `status` `run` `logs` `shell` `stop` `merge` `destroy` `doctor` | the matching subcommand |
| `-h`, `--help`, `help`, or no arguments at all | print `usage()` to stdout, exit 0 |
| anything else not starting with `-` | **positional shorthand**: `cmd_start NAME <rest...>` |
| anything starting with `-` | `error: unknown subcommand: <arg>`, exit 1 |

`usage()` prints the exact historical heredoc text (the one the bash CLI
printed for `--help`); the Rust binary embeds it verbatim so
`agent-gvisor --help` output is byte-identical to the bash CLI's.

The Nix package (`nix/agent-gvisor.nix`) also ships a hand-written fish
tab completion (`rust/completions/agent-gvisor.fish`, installed to
`$out/share/fish/vendor_completions.d/agent-gvisor.fish`; the crate stays
zero-dependency, so it is a plain fish script, not clap-generated). It
mirrors this grammar: the dispatch words with descriptions, the `start`
options (§2), the per-subcommand flags (§3/§9), and existing session
names read from the registry (§4) — offered for the NAME positions of the
name-taking subcommands and for `start --name`. Keep it in sync when the
grammar changes; the `agent-gvisor-completions` check (`nix/checks.nix`)
enforces the sync: it fails when the file does not parse (`fish -n`) or a
subcommand or an option documented in `rust/src/usage.txt` is not
completed.

`start` flag parsing continues across positionals: in
`start NAME --detach -- x`, `NAME` is the positional session name and
`--detach` is still parsed as a flag. The first positional is the session
name; every further positional is prepended to the COMMAND (exactly like the
arguments after `--`). `--name` plus a positional name is
`error: session name given twice (--name and positional)`, exit 1.

## 2. `start` options

| flag | default | notes |
| --- | --- | --- |
| `NAME` (positional) | — | alternative to `--name` |
| `--repo PATH` | current directory | host repository; realpath'd, then anchored at the repository root (`rev-parse --show-toplevel`), so a subdirectory start behaves like one from the root |
| `--base REF` | `HEAD` | must resolve to a commit in the host repo |
| `--branch BRANCH` | `agent/gvisor/NAME` | worktree branch |
| `--image IMAGE` | `$AGENT_GVISOR_IMAGE` / `$AGENT_GVISOR_DEFAULT_IMAGE` / `localhost/agent-dev:latest` | podman image |
| `--config HOST:DEST[:ro\|rw]` | — | repeatable; default mode `ro` |
| `--mount HOST:DEST[:ro\|rw]` | — | repeatable; default mode `rw` |
| `--env KEY=VALUE` | — | repeatable; stored verbatim in `env.list` |
| `--env-file PATH` | — | resolved with realpath (`-e`: fails if missing) |
| `--network MODE` | `$AGENT_GVISOR_NETWORK` (empty ⇒ omit) | |
| `--detach` | interactive | `--detach` vs `--interactive --tty` |
| `--memory LIMIT` | `8g` | only passed when limits are enforced (§5) |
| `--cpus NUMBER` | `4` | dito |
| `--pids-limit NUMBER` | `2048` | dito |
| `--seccomp-unconfined` | off | adds `--security-opt=seccomp=unconfined` |
| `--nix` | `$AGENT_GVISOR_NIX` | writable Nix store volume at `/nix/store` (see `docs/nix-in-sandbox.md`); recorded in `meta` as `nix=true` |
| `--no-nix` | — | start without the Nix store, even when `$AGENT_GVISOR_NIX` enables it by default |
| `--force` | off | destroy an existing session of the same name |
| `--home-seed PATH` | see §6 | realpath'd; must be a directory |
| `--no-home-seed` | seeding on | start with an empty `/home/agent` |
| `--` | — | everything after it is the COMMAND |
| `-h`, `--help` | | print `usage()`, exit 0 |
| any other `-…` | | `error: unknown start option: <arg>`, exit 1 |

A value-taking flag at the end of the argument list is an error (exit 1);
the bash CLI died on bash's own `${2:?}` diagnostic there, the Rust CLI
normalises this to `error: option requires a value: <flag>` (this is the only
error-text change in the rewrite; nothing scripts against the bash wording).

### `parse_mount` rules (`--config` / `--mount`)

Spec form is `HOST:DEST[:MODE]`:

1. `HOST` and `DEST` must be non-empty and there must be no fourth field —
   otherwise `error: invalid mount '<spec>'; expected HOST:DEST[:ro|rw]`.
2. `DEST` must be absolute — otherwise
   `error: container mount destination must be absolute: <dest>`.
3. `MODE` defaults to the flag's default (`ro` for `--config`, `rw` for
   `--mount`), must be `ro` or `rw` — otherwise
   `error: mount mode must be ro or rw: <spec>`.
4. `HOST` must exist — otherwise `error: mount source does not exist: <host>`
   with the path AS THE CALLER GAVE IT (bash's failed
   `host=$(realpath -e …)` assignment clobbered the variable and printed an
   empty path; the rewrite deliberately keeps the original). Host paths are
   canonicalised like `realpath -e`.

Resolved mounts are stored tab-separated in `__sessions/<name>/mounts.tsv`.

## 3. Other subcommands

- `status NAME` — prints the session fields, container state (via
  `podman inspect`) if the container exists, else `status:    stopped/absent`,
  then `git -C <worktree> status --short --branch` (inherited exit code).
- `run NAME [--detach] -- [COMMAND...]` — refuses when the container is
  already running: `error: container is already running: <name>`. Any argument
  before `--` that is not `--detach` starts the COMMAND (like `start`).
- `logs NAME [PODMAN-LOGS-ARGS...]` — container must exist, else
  `error: container is absent: <name>`; then `exec`s
  `podman <globals> logs <args...> <container>` (exit code = podman's).
- `shell NAME [COMMAND...]` — container must exist, else
  `error: container is not running: <name>`; `exec`s
  `podman <globals> exec --interactive --tty <container> [/bin/bash | COMMAND...]`.
- `stop NAME` — `podman <globals> stop --time 10 <container>` if the container
  exists, else logs `container already absent: <container>`.
- `merge NAME [...]` — see §9.
- `destroy NAME [--force] [--delete-branch]` — see §9; any other argument:
  `error: unknown destroy option: <arg>`.
- `doctor` — see §10.
- `list` — see §8.

## 4. Environment variables

| variable | default | semantics |
| --- | --- | --- |
| `AGENT_GVISOR_IMAGE` | — | overrides the default image (highest priority) |
| `AGENT_GVISOR_DEFAULT_IMAGE` | `localhost/agent-dev:latest` | baked into the Nix wrapper (`--set-default`) |
| `AGENT_GVISOR_STATE` | `${XDG_STATE_HOME:-$HOME/.local/state}/agent-gvisor` | session REGISTRY root (`sessions/` name→symlink) |
| `AGENT_GVISOR_WORKTREES` | empty | empty ⇒ worktrees repo-adjacent as `<repo>__agent-gvisor/<name>`; a directory ⇒ `$ROOT/<repo-id>/<name>` |
| `AGENT_GVISOR_PODMAN_RUNTIME` | — | runtime name or absolute path; overrides `AGENT_GVISOR_DEFAULT_RUNTIME` |
| `AGENT_GVISOR_DEFAULT_RUNTIME` | `runsc` | baked into the Nix wrapper (absolute runsc path) |
| `AGENT_GVISOR_PODMAN_CGROUP_MANAGER` | see §5 | podman `--cgroup-manager` value; empty string ⇒ flag omitted |
| `AGENT_GVISOR_PODMAN_RUNTIME_FLAGS` | see §5 | **space-separated list**; each non-empty entry becomes `--runtime-flag=<entry>` |
| `AGENT_GVISOR_HOME_SEED` | — | seed `/home/agent` from this directory instead of the activated home-manager generation |
| `AGENT_GVISOR_HOME_SEED_PATHS` | empty ⇒ no seeding | **space-separated** relative paths copied from the seed tree (allowlist) |
| `AGENT_GVISOR_HOME_SEED_REWRITE` | empty ⇒ no rewriting | **space-separated** `OLD=NEW` rules applied literally to the seeded files |
| `AGENT_GVISOR_MODEL_ENDPOINT` | unset | probed by `doctor` from inside a sandbox |
| `AGENT_GVISOR_NETWORK` | unset ⇒ empty | default `--network` for sessions and `doctor` probes |
| `AGENT_GVISOR_LOOPBACK_FORWARD` | unset | **space-separated** `LPORT:RHOST:RPORT` rules; passed into the container and set up by `/bin/agent-gvisor-init` |
| `AGENT_GVISOR_DEFAULT_COMMAND` | `/bin/bash` | word-split command run by `start`/`run` when no COMMAND is given; `shell` is unaffected |
| `AGENT_GVISOR_NIX` | unset ⇒ off | default for `--nix`; enabled unless unset, empty or exactly `false` |
| `AGENT_GVISOR_NIX_CONFIG` | unset | passed into `--nix` sessions as the container env `NIX_CONFIG` (the in-sandbox `nix.conf`; see `docs/nix-in-sandbox.md`) |

The Nix module (../default.nix) bakes the list-encoded variables joined with
single spaces via `makeWrapper --set-default`, so they remain overridable per
invocation. Word-splitting for all list variables is plain IFS whitespace
splitting with empty items skipped.

## 5. EUID-dependent rootless behaviour

When run as root (`geteuid() == 0`):

- cgroup manager default: empty (podman default, flag omitted)
- runtime flags default: empty

When run as a normal user:

- cgroup manager default: `cgroupfs`
- runtime flags default: `ignore-cgroups`

Both are independently overridable via the two `AGENT_GVISOR_PODMAN_*`
variables (including to the empty string, which disables the flag /
the defaults).

Consequence (identical to the bash CLI): when the runtime flags contain
`ignore-cgroups`, resource limits CANNOT be enforced. `run`/`start` then

- omit `--pids-limit`, `--memory`, `--cpus` entirely, and
- log to stderr:
  `agent-gvisor: warning: memory/cpu/pids limits not enforced, the runtime ignores cgroups`.

## 6. Global podman arguments

Every podman invocation starts with the global arguments, in this order:

1. `--runtime=$PODMAN_RUNTIME` (always)
2. `--cgroup-manager=$CGROUP_MANAGER` (only when non-empty)
3. `--runtime-flag=<flag>` for every non-empty entry of the runtime flags, in order

## 7. Exit codes

- `0` — success (also for `--help`/`usage`, and for `list`, which ignores
  per-session probe failures).
- `1` — every error the CLI itself detects (same as the bash `die`).
- The exit code of the exec'd process for `start`, `run` (podman run), `logs`,
  `shell` (podman logs/exec) and the trailing
  `git status --short --branch` of `status`.

## 8. Session-state layout

Per host repository at `<repo>` (the repository ROOT — `start` anchors
there via `rev-parse --show-toplevel`, even from a subdirectory), everything
lives repo-adjacent in
`$(dirname <repo>)/$(basename <repo>)__agent-gvisor/` ("agent root"):

```
<repo>__agent-gvisor/
  __pools/<repo-id>.git      disposable bare pool (one per repo)
  __pools/<repo-id>.lock     flock target serialising pool access
  __sessions/<name>/         session state (meta last, see §9)
    meta                     shell-quoted key=value (see below)
    mounts.tsv               tab-separated host,dest,mode (+ trailing empty line)
    env.list                 one KEY=VALUE per line (+ trailing empty line)
    home/                    bind-mounted over /home/agent (mode 700)
    last-command             the exec'd podman argv, %q-quoted, space-joined
  <name>/                    session worktree (or $AGENT_GVISOR_WORKTREES/<repo-id>/<name>)
```

- `<repo-id>` is the first 16 hex characters of `sha256(<realpath of repo>)`,
  computed by exec'ing `sha256sum` (so IDs stay identical to sessions created
  by the bash CLI). The hashed string has NO trailing newline.
- Session names match `^[A-Za-z0-9][A-Za-z0-9_.-]*$`; anything else:
  `error: invalid session name '<name>' (allowed: letters, digits, dot, underscore, hyphen)`.
  Because names cannot start with an underscore, `__pools`/`__sessions`
  never collide with a worktree.
- Container name: `agent-<repo-id>-<name>`, lowercased, every
  `[^a-z0-9_.-]` run collapsed to `-`, leading/trailing `-` stripped.
- Default branch: `agent/gvisor/<name>`.

The registry `$STATE_ROOT/sessions/<name>` is ALWAYS a symlink to
`<repo>__agent-gvisor/__sessions/<name>` (created with `ln -sfn` semantics).

### `meta` format

`meta` keeps the historical shell-quoted `key=value` line format so sessions
created by the bash CLI stay loadable. Writer output is byte-identical to
`printf 'key=%q\n'` for all realistic values (see §12 for the quoting
rules). Fields, in this exact order:

```
name= repo= repo_id= pool= worktree= home= container= branch= image=
memory= cpus= pids_limit= network= seccomp_unconfined= env_file= nix=
```

`seccomp_unconfined` is the literal string `true`/`false`; the run-time
`--security-opt=seccomp=unconfined` flag is added whenever the field is
anything but exactly `false` (bash-source semantics). `nix` is the literal
`true`/`false` too, but STRICTER: the Nix mounts apply only when it is
exactly `true` — a field absent from bash-era metas parses as empty and
must not gain mounts, because the CLI never created a backing volume for
those sessions. Empty values are written as `''`. The reader accepts general shell quoting: bare words,
backslash escapes, `'…'`, `"…"`, `$'…'` (ANSI-C); unknown keys are ignored,
missing keys default to empty.

## 9. Behavioural contracts

### `start` ordering

1. flag/positional parsing (mount specs validated + canonicalised inline),
   name validation, `need` checks (`git`, `podman`, `sha256sum`).
2. `check_runtime` and `check_image` — fail BEFORE anything is created.
3. `realpath` the repo; `git -C <repo> rev-parse --is-inside-work-tree`;
   anchor at the repository root via `git -C <repo> rev-parse
   --show-toplevel` (a subdirectory start behaves like one from the root);
   resolve `--base` via `git rev-parse --verify <base>^{commit}`.
4. create `$STATE_ROOT/sessions`, agent-root `__pools`, `__sessions`.
5. existing-session probe on the registry entry (below).
6. `mkdir -p` worktree parent, `__sessions/<name>`, `home`; **create the
   registry symlink**; create `home/.cache`, `home/.config`, `home/.local/state`;
   `chmod 700` the session dir and home.
7. home seeding (§11) — never aborts the start on a partially copyable tree.
8. `flock` `__pools/<repo-id>.lock`.
9. pool: `git init --bare` + `remote add host <repo>` if absent, else
   `remote set-url host <repo>`; `fetch --prune --no-recurse-submodules host
   +refs/heads/*:refs/remotes/host/* +refs/tags/*:refs/tags/*`; if the base
   commit is missing: `fetch --no-tags host <base-commit>`.
10. refuse an existing worktree path (`error: worktree path already exists: …`);
    `git worktree add` (existing pool branch checked out as-is, otherwise
    `-b <branch>` at the base commit).
11. write `mounts.tsv` and `env.list` (a single newline when the list is
    empty — historical byte layout), **`meta` LAST**.
12. release the flock; log
    `created worktree <path> on branch <branch>` and the retry/doctor/destroy
    hint line; then run the container (§10 argv) via `exec`.

### Existing session of the same name

The probe looks at the registry entry, not the session dir:

- registry entry exists but its `meta` is absent ⇒ debris of an interrupted
  start ⇒ `reset_partial_session` (below).
- otherwise ⇒ existing session: read the old branch (falling back to the
  new default branch if the old session cannot be loaded);
  - `--force`: log `--force: destroying existing session <name> and deleting branch <old-branch>`, then destroy;
  - interactive TTY: prompt (§13);
  - neither: `error: session already exists: <name> (pass --force, or remove it with 'agent-gvisor destroy <name> --force --delete-branch')`, exit 1.
  
  The destroy runs as `destroy --force --delete-branch`; if it fails:
  `error: could not destroy the existing session: <name>`.

### `reset_partial_session` (incomplete-session recovery)

`meta` written last means a session dir without `meta` is debris: the next
`start` of the same name logs
`session <name> is incomplete (interrupted start); removing <meta-dir>` and
removes it — plus the leftover worktree: if it is a git worktree with a
present pool, it must be clean, else

```
error: leftover worktree has uncommitted changes: <worktree>
Inspect it, then remove it with:
  git --git-dir=<pool-q> worktree remove --force <worktree-q>
```

(`%q`-quoted arguments); `worktree remove --force` failure:
`error: could not remove leftover worktree: <worktree>`. A non-worktree path
is plain `rm -rf`'d. The BRANCH is never touched. `list` shows such names as
`incomplete` and every name-only command explains the debris (§ "unknown
session" inventory).

### `merge`

1. `--repo PATH` overrides the session's origin repo (realpath'd;
   `error: --repo: not a path: <arg>`); target must contain `.git`
   (`error: --repo: not a Git work tree: <path>`).
2. refuse detached HEAD: `error: the repository at <path> is in detached HEAD state; switch to the branch you want to merge into first`
3. refuse a dirty tree: `error: working tree of <path> is dirty; commit or stash before merging`
4. `--no-ff` is the default; `--ff`/`--squash`/explicit git-merge args are
   passed through (args after `--` verbatim).
5. fetch the session branch from the pool into `refs/heads/<branch>`
   (`git fetch --no-tags <pool> +<branch>:refs/heads/<branch>`); failure:
   `error: fetch from pool failed; is the session pool still present?`
6. `git merge <merge-args...> <branch>`; on success the temporary ref is
   deleted; on failure:
   `error: merge failed; resolve conflicts in <repo>, then delete the leftover ref with 'git -C "<repo>" branch -D <branch>'`
   (note the literal double quotes around the repo path).

### `destroy`

1. remove the container if it exists (`podman rm --force --time 10`? — no:
   `podman rm --force --time 5 <container>`).
2. remove the session's Nix store volume if the session is a `nix=true`
   session and the volume exists (`podman volume rm <container>-nix`;
   see `docs/nix-in-sandbox.md`).
3. if the worktree dir exists: refuse a dirty tree without `--force`
   (`error: worktree has uncommitted changes; commit them or use --force`);
   `git --git-dir=<pool> worktree remove [--force] <worktree>`.
4. `--delete-branch`: `git --git-dir=<pool> branch -D <branch>`.
5. `rm -rf` the session dir **and** the registry entry; log
   `destroyed session <name>`.

### `list`

Header `SESSION STATUS BRANCH WORKTREE` (columns 24/12/28), then one line
per registry entry (alphabetical). Status via `podman container exists` +
`inspect --format '{{.State.Status}}'` (`unknown` if inspect fails), `stopped`
if absent. Incomplete entries (symlink without `meta`): status `incomplete`,
branch `-`, worktree = the registry path. Pre-rewrite entries (real
directory): status `incompatible (pre-rewrite layout)` (§ "Dropped"), with
`-` for BRANCH and the registry path for WORKTREE (like `incomplete` rows).

## 10. The podman `run` argument vector

For `start` and `run` (identical builder, `--replace` recreates):

```
podman <global args> run --replace (--detach | --interactive --tty)
  --name <container>
  --hostname <session name>
  --userns=keep-id
  --read-only
  --read-only-tmpfs=true
  --cap-drop=ALL
  --security-opt=no-new-privileges
  --workdir <repo>
  --mount type=bind,src=<worktree>,dst=<repo>,rw
  --mount type=bind,src=<pool>,dst=<pool>,rw
  --mount type=bind,src=<home>,dst=/home/agent,rw
  [--mount type=volume,src=<container>-nix,dst=/nix/store]  # only when nix=true
  --env HOME=/home/agent
  --env XDG_CONFIG_HOME=/home/agent/.config
  --env XDG_CACHE_HOME=/home/agent/.cache
  --env XDG_STATE_HOME=/home/agent/.local/state
  --env AGENT_SESSION=<session name>
  --env AGENT_WORKTREE=<repo>
  [--env AGENT_GVISOR_LOOPBACK_FORWARD=<value>]      # only when set
  [--env NIX_REMOTE=local                            # only when nix=true:
   --env NIX_STATE_DIR=/home/agent/.local/state/nix    # the daemon-less store
   --env NIX_LOG_DIR=/home/agent/.local/state/nix/log  # and its on-home state,
   --env TMPDIR=/home/agent/.cache/nix-tmp             # see docs/nix-in-sandbox.md
   --env AGENT_GVISOR_NIX=1]
  [--env NIX_CONFIG=<nix.conf>]                      # only when AGENT_GVISOR_NIX_CONFIG is set
  (--pids-limit <n> --memory <m> --cpus <c>)         # only when limits enforced (§5)
  [--network <mode>]                                 # only when non-empty
  [--security-opt=seccomp=unconfined]                # seccomp_unconfined != "false"
  [--env-file <path>]                                # only when non-empty
  --mount type=bind,src=<host>,dst=<dest>,<mode>      # per mounts.tsv line, in order
  --env <KEY=VALUE>                                  # per env.list line, in order
  <image>
  [/bin/agent-gvisor-init]                            # when LOOPBACK_FORWARD is set OR nix=true
  <COMMAND...>                                       # or the word-split AGENT_GVISOR_DEFAULT_COMMAND
```

`<repo>` is the repository ROOT recorded in `meta` (§8: `start` anchors
there via `rev-parse --show-toplevel`); the session
worktree (a HOST path) is bind-mounted at that path inside the container, so
the worktree appears where the repository normally lives and `--workdir` /
`AGENT_WORKTREE` point at it. The host checkout itself is never mounted.

Before `exec`ing, the full vector (including the leading literal `podman`)
is written to `<session>/last-command` as `%q`-quoted, space-joined words
plus a trailing newline — byte-identical to the bash CLI's file.

`check_runtime`: an absolute runtime path must be executable
(`error: OCI runtime is not executable: <runtime>`); a named runtime is
probed with `podman info`, and on failure:

```
error: Podman OCI runtime <runtime-q> is not registered.
Register it in containers.conf (on NixOS:
virtualisation.containers.containersConf.settings.engine.runtimes), or
set AGENT_GVISOR_PODMAN_RUNTIME to the absolute path of a runsc binary.
```

`check_image`: `podman image exists <image>` must succeed, else:

```
error: container image <image-q> is not in the local Podman store.
Build and load it with: agent-gvisor-load-image
(or: nix run .#load-image), or pass --image with another reference.
```

## 11. Home seeding

Source resolution (`--home-seed` > `AGENT_GVISOR_HOME_SEED` > activated
home-manager generation), candidate generation paths in order:

1. `${XDG_STATE_HOME:-$HOME/.local/state}/home-manager/gcroots/current-home`
2. `/nix/var/nix/gcroots/per-user/$USER/current-home`
3. `${XDG_STATE_HOME:-$HOME/.local/state}/nix/profiles/home-manager`
4. `/nix/var/nix/profiles/per-user/$USER/home-manager`

each checked for `home-files` (canonicalised). No source ⇒ seeding silently
skipped. A found source that is not a directory:
`error: home seed is not a directory: <seed>`.

Seeding never aborts `start`:

- an absent allowlist path is skipped; a DANGLING top-level symlink logs
  `warning: skipping dangling seed path <rel> -> <link-target>`.
- entries are copied DEREFERENCED (`cp -RL --no-preserve=mode` semantics:
  recursion, symlink resolution, mode not preserved); a tree that copies
  only partially still counts, logs
  `warning: seed path <rel> copied incompletely (broken links in the home-manager generation):`
  followed by the per-file `cp`-style error lines indented by two spaces.
- afterwards `chmod -R u+rwX` the home; summary log
  `seeded /home/agent with <n> path(s) from <seed>[ (<k> of them incomplete)]`.

Then `AGENT_GVISOR_HOME_SEED_REWRITE` rules are applied: each rule must be
`OLD=NEW` (`error: invalid home-seed rewrite rule (expected OLD=NEW): <rule>`);
replacement is literal, binary files (NUL byte) are skipped, every rewritten
file is written back with exactly one trailing newline (bash `$(<file)` +
`printf '%s\n'` semantics), and files are located with `grep -rIlZF`
semantics (recursive, fixed-string, binary-excluded). If anything changed:
`applied <n> host-endpoint rewrite(s) to the seeded configuration`.

## 12. Shell-quoting rules (meta, last-command, %q in messages)

The writer reproduces bash's `printf %q` for all inputs the CLI can
produce:

- empty string → `''`
- any control character (0x00–0x1F, 0x7F), DEL, or an invalid-UTF-8 byte
  ⇒ the WHOLE value in `$'…'` with ANSI-C escapes
  (`\a \b \e \f \n \r \t \v \\ \'` named, `\NNN` octal for the rest; printable
  multibyte sequences stay raw). A non-printable *valid* UTF-8 codepoint
  (Unicode Cc/Cf/…) is treated like a control character via a small builtin
  table of common format characters — an approximation of glibc's
  `iswprint`, documented here as such.
- otherwise backslash-escape exactly bash's set:
  space `!"$&'()*`,;<=>?[\]^`{|}` (TAB/NL never reach this path), plus `#`
  at the very start and `~` at the start or after `:`/`=`; everything else
  (alphanumerics, `@%+=:./-_`, printable UTF-8) stays bare.

The reader accepts general shell quoting (bare, backslash, `'…'`, `"…"`,
`$'…'` with octal/hex/named escapes, `\<newline>` line continuation) so
meta files written by the bash CLI parse identically.

## 13. Interactive (TTY) behaviour — not unit-tested

`start` on an existing session without `--force`, on an interactive
terminal (stdin AND stderr are TTYs), prompts

```
agent-gvisor: session <name> already exists; destroy it and delete branch <branch>? [y/N] 
```

(no trailing newline, on stderr) and proceeds on `y`/`yes` (any case);
EOF or anything else fails like the non-interactive case. TTY detection,
the prompt and the reply handling are validated by a MANUAL smoke test on a
real host only; the automated suites cover the `--force` and non-TTY paths.

## 14. Dropped in the rewrite

All old-layout compatibility from the bash CLI is gone. Concretely:

1. **Registry entries that are real directories** (the pre-rewrite layout,
   where `$STATE/sessions/<name>` WAS the session dir) are no longer read by
   `load_meta`/`destroy`/`reset_partial_session`. `list` shows them as
   `incompatible (pre-rewrite layout)`; every other subcommand — including
   `destroy` — fails with
   ```
   error: session <name> is from the pre-rewrite layout; remove it by hand with:
   rm -rf <registry-entry-q>
   ```
2. **The old central pool layout** (`$STATE_ROOT/pools/<repo-id>.git`) is
   not consulted; orphaned central pools are ignored entirely.
3. **Old-layout session-state dual paths** in `load_meta` (following real
   directories instead of symlinks) — see 1.
4. **Bash dynamic-scoping workarounds** (the subshell indirection around
   `load_meta`/`cmd_destroy` in `cmd_start`) — internal to bash, obsolete.
5. **`realpath`/`flock`/`grep`/`sed`/`tr`/`cut` as external runtime
   dependencies** — `realpath` is `fs::canonicalize`, `flock` a libc
   binding, binary-file detection a NUL-byte scan; only `git`, `podman` and
   `sha256sum` are still exec'd from PATH (see AGENTS/repo docs for the
   wrapper PATH).
6. The bash `${2:?}` "parameter null or not set" diagnostics for a missing
   flag value are replaced by `error: option requires a value: <flag>`
   (§2).
7. The bash `${1:?session name required}` diagnostics for the subcommands
   that take a positional NAME (`status`, `run`, `logs`, `shell`, `stop`,
   `merge`, `destroy`) embed the bash line number and are not reproducible;
   they become `error: session name required`.

Everything else — subcommands, flags, env vars, state layout, podman argv,
exit codes, message texts — is preserved. Sessions created by the CURRENT
bash layout (registry symlink + repo-adjacent `__pools`/`__sessions`) remain
fully loadable because repo-ids are path-based (stable across sessions) and
`meta` keeps the shell-quoted format.

## 15. Validation

Two behavioural layers, both wired into `nix flake check` via
`nix/checks.nix` (which adds a third, packaging-level check):

- `rust/tests/` — cargo integration tests (`tests/common/mod.rs` builds
  isolated scenarios and installs the recording `git`/`podman` stubs from
  `rust/tests/stubs/` into a PATH-prepended directory; stub behaviour is
  switched by marker files, invocations are recorded NUL-separated):
  `podman_argv.rs` (the exact `run` vector, pure + recorded),
  `state_layout.rs` (registry/pools/sessions tree, `meta` bytes, `list`,
  `status`, `destroy`), `error_paths.rs` (every fatal message verbatim),
  `shellwords.rs` (bash-`%q` fixtures, reader, `split_ws`),
  `home_seed.rs` (seeding, partial copies, rewrite rules, through `start`).
  The `--nix` surface (volume mount, in-container Nix env block, init
  wrapper, `meta` `nix=`, `destroy`'s `podman volume rm`) lives in
  `podman_argv.rs` (`build_run_args_nix`,
  `start_nix_records_volume_and_destroy_removes_it`).
- `tests/agent-gvisor-cli-harness.sh` — end-to-end CLI flows (`doctor`
  happy/sad, a full session cycle, `list` rows, podman argv sanity, a
  `--nix` session incl. its volume cleanup) driving the UNWRAPPED binary
  (the production wrapper's PATH would shadow the stubs).
- `agent-gvisor-completions` (`nix/checks.nix`) — the fish tab completion
  shipped by the production package (§1): installed at the vendor path,
  byte-identical to `rust/completions/agent-gvisor.fish`, parsed by
  `fish -n`, and covering every subcommand and every option documented in
  `rust/src/usage.txt` (built with neutral defaults so it needs neither
  the sandbox image nor gvisor).

### Deletion gate (bash parity)

Before deleting the bash CLI, both implementations were run through the
same ~40-step scenario (same directory layout, so repo-ids — and therefore
container names, pool paths and every printed path — were identical;
same stubs): every step's stdout, stderr and exit code matched
byte-for-byte, as did the `meta`/`mounts.tsv`/`env.list`/`last-command`
bytes and every recorded `git`/`podman` argv (including the full `podman
run` vector with loopback forwarding, mounts, env-file and `-- COMMAND`).
The only diffs were the deliberate normalizations: the `session name
required` wording (§14.7), `mount source does not exist: <original-host>`
keeping the path the caller gave (§2, mount rule 4), and `list`'s
`incompatible (pre-rewrite layout)` row (§14.1). The bash CLI remains
recoverable from the git history for re-running the comparison.

TTY behaviour (§13) is deliberately not automated: there is no terminal in
a build sandbox. The nix checks run on `x86_64-linux` only.
