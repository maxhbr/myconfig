# myconfig.ai.gvisor-agent-sandbox

Rootless Podman + gVisor (`runsc`) sandboxes for coding agents, providing the
`agent-gvisor` session manager and a Nix-built sandbox image.

## Rust rewrite

The session manager CLI is implemented in Rust ([`rust/`](./rust), packaged by
[`nix/agent-gvisor.nix`](./nix/agent-gvisor.nix) via `rustPlatform.buildRustPackage`),
replacing the original bash script (`bin/agent-gvisor`, now deleted). The
authoritative contract — subcommands, flags, env vars, session-state layout,
the exact podman argument vector, error-message texts, and the behaviours
dropped by the rewrite — is written down in
[`docs/spec.md`](./docs/spec.md); the cargo tests in `rust/tests/` and the
executed stub harness `tests/agent-gvisor-cli-harness.sh` enforce it (wired
into `nix flake check` via `nix/checks.nix`, x86_64-linux only).

Compatibility notes:

- Sessions created by the bash CLI (current layout: registry symlink +
  repo-adjacent `__pools`/`__sessions`) keep working unchanged: repo-ids are
  path-based and the shell-quoted `meta` format is preserved (see
  `docs/spec.md` §8/§12).
- Pre-rewrite-layout sessions (registry entry = real session directory, old
  central `$XDG_STATE_HOME/agent-gvisor/pools/`) are NOT loadable any more:
  `list` shows them as `incompatible (pre-rewrite layout)` and every other
  subcommand asks you to remove them by hand. The full list of dropped
  behaviours is in `docs/spec.md` §14.
- Only `git`, `podman` and `sha256sum` are exec'd from `PATH`; `realpath`,
  `flock`, binary-file detection etc. are implemented in Rust (zero cargo
dependencies).

Because everything under this directory is a vendored `git subtree`, future
`git subtree pull`s will conflict with the Rust additions — see
[`doc/TODOs/agent-gvisor-subtree-rust-conflict.md`](../../../doc/TODOs/agent-gvisor-subtree-rust-conflict.md).

It is the container-based tier of the repo's sandboxing ladder — see
[`../docs/README.md`](../docs/README.md) ("Agent sandboxing tiers") for the
other tiers (`agent-tmux`, `jailed-*`, `sandboxed-*`, `agent-microvm`) and how
they compare.

The package also ships a hand-written fish tab completion
([`rust/completions/agent-gvisor.fish`](./rust/completions/agent-gvisor.fish),
installed into `share/fish/vendor_completions.d/`): subcommands with
descriptions, the `start`/`merge`/`fetch`/`push`/`destroy`/`run`/`logs` flags, and
existing session names read from the session registry — also offered for
`start --name`. It mirrors `rust/src/usage.txt` and is kept in sync by the
`agent-gvisor-completions` check (`nix/checks.nix`).

## Origin

- Upstream: <https://github.com/maxhbr/gvisor-agent-sandbox>
- Vendored via `git subtree` (history preserved, **not** squashed).
- Imported commit: `769745bec150e86d2d9461686ca52b982074a47f`
  ("fix: make rootless gVisor sandboxes actually start").

The upstream project is *not* a flake input of this repo; its sources live
in-tree and are built with plain `pkgs.callPackage`.

### Pulling future upstream changes

```bash
git remote add gvisor-agent-sandbox https://github.com/maxhbr/gvisor-agent-sandbox   # once
git fetch gvisor-agent-sandbox
git subtree pull --prefix=modules/myconfig.ai/myconfig.ai.gvisor-agent-sandbox \
    gvisor-agent-sandbox main
```

Expect conflicts in the files touched during integration (see below), notably
`README.md` (upstream's version now lives at `docs/upstream-README.md`) and
`bin/agent-session`, which is locally patched *and* renamed to
`bin/agent-gvisor`. Everything upstream calls `agent-session` /
`agent-sandbox*` / `AGENT_SANDBOX_*` is called `agent-gvisor` /
`AGENT_GVISOR_*` here, so a pull re-adds the upstream paths as new files and
their content has to be merged into the renamed ones by hand:

| upstream | here |
| --- | --- |
| `bin/agent-session` | `bin/agent-gvisor` |
| `nix/agent-session.nix` | `nix/agent-gvisor.nix` |
| `nix/agent-sandbox-init.sh` | `nix/agent-gvisor-init.sh` |
| `agent-session` (binary, overlay attr) | `agent-gvisor` |
| `agent-sandbox-load-image` | `agent-gvisor-load-image` |
| `agent-sandbox-image` (overlay attr) | `agent-gvisor-image` |
| `/bin/agent-sandbox-init` | `/bin/agent-gvisor-init` |
| `AGENT_SANDBOX_*`, `AGENT_PODMAN_*` | `AGENT_GVISOR_*`, `AGENT_GVISOR_PODMAN_*` |
| `~/.local/state/agent-sandbox` | `~/.local/state/agent-gvisor` |

The upstream names are generic enough to collide with the other agent-sandbox
tiers in `modules/myconfig.ai/`, all of which manage "sessions" of some kind;
the names used here say which tier they belong to. Only the *module* keeps the
upstream project name (`myconfig.ai.gvisor-agent-sandbox`), because that is
what the vendored subtree is.

## What changed during integration

Removed (existed only to make upstream a standalone flake):

- `flake.nix`, `flake.lock` — the packages are instead pulled in through
  `nix/overlay.nix`, which this repo's module adds to `nixpkgs.overlays`.
- `nixos/agent-sandboxes.nix` — the upstream `programs.agentSandboxes` NixOS
  module; replaced by `./default.nix` (option
  `myconfig.ai.gvisor-agent-sandbox`), following this repo's conventions
  (option-path-mirroring file name, `mkEnableOption` gate, everything under
  `lib.mkIf cfg.enable`, packages installed via
  `home-manager.sharedModules`/`home.packages` like the other
  `myconfig.ai.*` modules).

Moved:

- `bin/agent-gvisor` → reimplemented in Rust under `rust/` (the bash CLI was
  deleted after stub-parity against it was proven; see "Rust rewrite" above).
- `README.md` → `docs/upstream-README.md` (upstream documentation: usage of
  `agent-gvisor`, image contents, isolation boundaries, troubleshooting),
  with the names adjusted to the ones used here. Its "install as a flake
  input" section does not apply, and still names the upstream flake.

Patched:

(The patches below describe the bash CLI as it was last vendored — the
file itself is deleted; the Rust rewrite preserves all of them — see
`docs/spec.md`, the authoritative contract — including the interactive
`[y/N]` confirm on `start` of an existing session (spec §13).)

- `bin/agent-gvisor` — the session name and repository can be given
  positionally: a first argument that is no action word is a session name, so
  `agent-gvisor NAME` is shorthand for
  `agent-gvisor start --name NAME --repo "$PWD"`, and
  `agent-gvisor start NAME` works as well. Any further positional argument
  before `--` is the command, like the arguments after `--`. `--name` and
  `--repo` keep working; `--repo` now defaults to the current directory. A
  first argument starting with `-` is still an unknown subcommand, so a
  mistyped flag does not silently start a session.
- `bin/agent-gvisor` — `start` no longer dies outright when a session of
  the same name exists. With the new `start --force` it destroys the old
  session (including its branch) unattended; otherwise, on a terminal, it
  asks `session NAME already exists; destroy it and delete branch BRANCH?
  [y/N]` and, on `y`, runs `destroy NAME --force --delete-branch` before
  creating the new session. Without a terminal and without `--force` the
  previous fail-fast behaviour is kept, now with the exact recovery command
  in the message. `start` also seeds `/home/agent` from the activated
  home-manager generation (`--home-seed`, `--no-home-seed`,
  `AGENT_GVISOR_HOME_SEED*`; see above). The destroy runs
  in a subshell because it sources the old session's `meta`, which would
  otherwise clobber `cmd_start`'s locals through bash's dynamic scoping.
  Additionally `start` honours `AGENT_GVISOR_NETWORK` as the default
  `--network` (overridable per session), and `doctor` probes the model
  endpoint through that same network so the check exercises the real path —
  both needed to make a host-loopback model endpoint reachable from a runsc
  sandbox (see *Model access (host LiteLLM)*). Home seeding is also
  *resilient*: `home-files` regularly contains symlinks whose `/nix/store`
  target is gone (garbage-collected, or an input update moved the file while
  the old generation is still activated). `cp -RL` fails on such an entry, and
  under `set -e` that used to abort the whole `start` with a bare `cp: cannot
  stat …`. Now a dangling top-level seed path is skipped with a warning, a
  partially copyable one is copied as far as possible and reported, and the
  summary line counts how many paths came over incomplete.
  Half-created sessions are recoverable: `meta` is written *last*, so a start
  that dies before that (aborted seed, `^C`, failed `worktree add`) used to
  leave a directory that `start` counted as "already exists" while `destroy`
  rejected it as "unknown session" — unremovable without `rm -rf`. Such debris
  is now cleared by the next `start` (with its leftover worktree, unless that
  has uncommitted changes; the branch is never touched), `list` shows it as
  `incomplete`, and every other command explains it instead of claiming the
  name is unknown.
  The pools and session state moved NEXT TO the repository, into the same
  `<repo>__agent-gvisor/` directory that already hosts the worktrees: the bare
  pool at `__pools/<repo-id>.git`, the per-session meta/home/mounts/env at
  `__sessions/<name>/`. `$XDG_STATE_HOME/agent-gvisor/sessions` remains the
  name→session REGISTRY — `start` puts a symlink per session there, so
  name-only commands (`run`, `stop`, `destroy`, `merge`, …) still resolve
  sessions without a `--repo` argument, and sessions started with the old
  central layout keep working (their registry entry is the real session
  directory). Merge or destroy those before starting a same-repo session:
  the fresh pool under `__pools/` does not know their branches, and the old
  `$XDG_STATE_HOME/agent-gvisor/pools/` pools are left behind as orphans
  (safe to delete by hand once no old session references them).
  The default session branch is `agent/gvisor/<name>` instead of upstream's
  `agent/<name>`, so branches from the different sandboxing tiers are
  distinguishable (`agent-microvm` uses `agent/microvm/<task>`). Existing
  sessions keep the branch recorded in their `meta`.

- `nix/load-image.nix` — `agent-gvisor-load-image` no longer decides by tag
  alone. See *Image freshness check* below.

Kept unchanged in substance (only nixfmt-rfc-style reformatting):

- `nix/overlay.nix`, `nix/agent-gvisor.nix`, `nix/agent-image.nix` — the
  package definitions (renamed as listed above, otherwise untouched).

## Usage in this repo

Disabled by default. Enable per host:

```nix
myconfig.ai.gvisor-agent-sandbox.enable = true;
```

The sandbox image is *not* generic-empty: `extraImagePackages` defaults to the
coding-agent CLIs the host itself has enabled, derived from the
`myconfig.ai.<agent>.enable` flags (`pi-coding-agent`, `opencode`,
`claude-code`, `codex`, `github-copilot-cli`, `qwen-code`), each mapped to the
same package attribute the corresponding host wrapper uses. So enabling an
agent on the host automatically makes it available inside the sandbox; the
host-side chat front-ends `aichat` / `llm` are deliberately excluded.

Whenever at least one of those agents is enabled, `pkgs.herdr` (the terminal
agent multiplexer) is added too — the same condition `../programs.herdr.nix`
uses on the host — and becomes the session's default command via
`defaultCommand`. A bare `agent-gvisor x` (shorthand for
`start --name x --repo .`) therefore
drops you into `herdr` rather than a plain shell; `-- COMMAND` still wins, and
`agent-gvisor shell` always gives a shell. Its host configuration is seeded
via `.config/herdr` in `home.seedPaths`. Set `defaultCommand = null` for the
upstream `/bin/bash` behaviour, or to any command line (word-split, e.g.
`"herdr --flag"`).

Override the package list to slim down or extend:

```nix
myconfig.ai.gvisor-agent-sandbox.extraImagePackages = [ pkgs.claude-code ];
```

When enabled the module

- bakes the enabled agent CLIs into the image (see above),
- adds `nix/overlay.nix` to `nixpkgs.overlays`, providing `agent-gvisor`,
  `agent-gvisor-image` and `agent-gvisor-load-image`,
- enables rootless Podman and registers `${pkgs.gvisor}/bin/runsc` as the
  `runsc` OCI runtime in `containers.conf`,
- installs `agent-gvisor`, `agent-gvisor-load-image` and `gvisor` into the
  user's `home.packages` (the `agent-gvisor` re-wrap that bakes the host
  defaults keeps the package's `share/` tree, so the fish tab completion
  ships too),
- grants subordinate UID/GID ranges (`autoSubUidGidRange`) to
  `myconfig.ai.gvisor-agent-sandbox.users` (default: `myconfig.user`).

### Sandbox home seeding (home-manager dotfiles)

`start` seeds the new session's `/home/agent` from the home-manager
generation **currently activated for the calling user**, resolved at runtime
from, in order:

1. `$XDG_STATE_HOME/home-manager/gcroots/current-home/home-files`
2. `/nix/var/nix/gcroots/per-user/$USER/current-home/home-files`
3. `$XDG_STATE_HOME/nix/profiles/home-manager/home-files`
4. `/nix/var/nix/profiles/per-user/$USER/home-manager/home-files`

So the agent finds its own skills, prompts and settings inside the sandbox,
and a dotfile change needs no image rebuild.

Baking the dotfiles into the *image* would not work: `agent-gvisor` bind
mounts the per-session home over `/home/agent`, which masks whatever the
image carried there (upstream creates the XDG dirs host-side for exactly that
reason).

Two consequences of the isolation boundary:

- `home-files` is a forest of symlinks into `/nix/store` and the sandbox has
  no `/nix`, so files are copied **dereferenced**. Configuration whose
  *content* points at `/nix/store` paths still dangles inside.
- The tree also contains `.ssh`, mail and browser configuration, so the copy
  is an **allowlist**, `myconfig.ai.gvisor-agent-sandbox.home.seedPaths`,
  baked into the wrapper as `AGENT_GVISOR_HOME_SEED_PATHS`. Default:
  `.agents .agignore .claude .codex .pi .config/git .config/herdr
  .config/opencode`.
  Anything added here is readable by a possibly hostile agent — never add
  credentials.

The seeded configuration points at the host's **loopback** LiteLLM proxy
(`http://127.0.0.1:4000/v1`), which does not exist inside a sandbox — there,
`127.0.0.1` is the container's own loopback, so agents fail with a connection
error. After copying, `start` therefore applies the literal `OLD=NEW` rules
from `home.rewriteEndpoints` (default: the loopback, `localhost` and
forwarder-port forms of the LiteLLM URL → the sandbox-reachable endpoint
`http://192.168.84.1:14000`) to the seeded files; binary files are skipped.
Set the option to `[ ]` to copy the configuration verbatim.

`192.168.84.1` is not a host interface address; it is the `--map-guest-addr`
target pasta translates to the host's global address (see *Model access (host
LiteLLM)* below), where a port-scoped forwarder listens on `14000` and
proxies to the loopback-only LiteLLM proxy on `127.0.0.1:4000`. Making
`localhost:4000` work inside the sandbox cannot be arranged from the **host**
side at all, despite what the podman/pasta documentation suggests (it takes a
relay *inside* the sandbox instead — see *Reverse forward* below):

- `--network 'pasta:-T,4000'` (namespace→host forwarding) does not help,
  because `runsc` runs its **own network stack**. pasta's `-T` listener lives
  in the container's Linux netns, while the container's processes see gVisor's
  internal loopback — verified on f13, `curl 127.0.0.1:4000` inside the
  sandbox gets `Connection refused`.
- `host.containers.internal` does not help either: podman configures pasta
  with `--map-guest-addr 169.254.1.2`, and that maps to the host's *global*
  address, not to its loopback (`pasta(1)`), so a loopback-only service is not
  behind it.

### Reverse forward: `127.0.0.1:4000` inside the sandbox

The rewrite rules above only fix the *files* the allowlist copies. Anything
else that names the host loopback — an `OPENAI_BASE_URL` the user exports, an
MCP server definition outside the seed, a hand-typed `curl
localhost:4000` — still fails, because the sandbox's `127.0.0.1` is gVisor's
loopback.

So the endpoint is *also* served on the sandbox's own loopback, by a relay that
runs **inside** the sandbox: `/bin/agent-gvisor-init` (baked into the image,
see `nix/agent-gvisor-init.sh`) starts one `socat` per rule in
`AGENT_GVISOR_LOOPBACK_FORWARD` (`LPORT:RHOST:RPORT`, baked by the module as
`4000:192.168.84.1:14000`), waits for the listener, then `exec`s the payload —
which therefore keeps PID 1, the TTY and its signals. `agent-gvisor shell`
and `agent-gvisor logs` attach to the same container and share its network
stack, so they see the relay too.

Inside the sandbox must be where it runs: a listener that gVisor's netstack
serves can only be opened by a process inside that netstack. This is the same
reason pasta's `-T` does not work (see above) — and the reason this is not a
security regression: the relay only re-labels a connection the sandbox can
already make (`192.168.84.1:14000`, port-scoped), it does not add reach.
Switch it off with `myconfig.ai.gvisor-agent-sandbox.litellm.loopbackForward =
false`; a slimmed image without `socat` degrades to a warning, not a failure.

Verify the whole chain from inside a sandbox with `agent-gvisor doctor`,
which probes `AGENT_GVISOR_MODEL_ENDPOINT` (baked from `litellm.endpoint`)
through the same pasta network a session uses (see below) in a throwaway
container and accepts any HTTP status as "reachable", and then probes each
loopback relay through `/bin/agent-gvisor-init`, exactly as a session starts
it.

Per invocation: `--no-home-seed` for an empty home, `--home-seed PATH` for a
different source tree, `AGENT_GVISOR_HOME_SEED` /
`AGENT_GVISOR_HOME_SEED_PATHS` / `AGENT_GVISOR_HOME_SEED_REWRITE` to
override source, allowlist and rewrite rules. Turn it off for the host with
`myconfig.ai.gvisor-agent-sandbox.home.enable = false`.

### Model access (host LiteLLM)

`services.litellm` is loopback-only on purpose, which a rootless sandbox
cannot reach directly: with pasta (podman's rootless default) the sandbox's
own `127.0.0.1` is the container's loopback, not the host's, and runsc runs
its own netstack on top of the pasta netns.

The endpoint is reached with a **port-scoped forwarder** + pasta's
`--map-guest-addr` (`pasta(1)`):

1.  A socket-activated `systemd-socket-proxyd` listens on
    `0.0.0.0:${forwardPort}` (default `14000`) and forwards to
    `127.0.0.1:${port}` (the loopback-only LiteLLM proxy). The forward port
    differs from the LiteLLM port so the `0.0.0.0` wildcard bind does not
    collide with LiteLLM's own `127.0.0.1` listener. The NixOS firewall trusts
    `lo` and drops `${forwardPort}` on every other interface (it is not in
    `allowedTCPPorts`), so the forwarder is reachable only from the host and
    from sandboxes, never from external hosts.

    The socket unit must keep `Accept=no` (the default):
    `systemd-socket-proxyd` inherits the *listening* socket and accepts
    connections itself. With `Accept=yes` systemd passes an already-accepted
    *connection* socket, the proxy fails on it and exits, and every client sees
    the handshake succeed followed by an immediate reset (`curl: (56) Recv
    failure: Connection reset by peer`) — a failure mode that looks exactly
    like a networking problem but is not one.

2.  The `agent-gvisor` wrapper bakes `AGENT_GVISOR_NETWORK` as a
    `pasta:--map-guest-addr,<address>` podman network spec (see
    `./default.nix`). `--map-guest-addr` translates `<address>` to the *guest's
    assigned address on the host* — by default the host's global address (the
    address on the default-route interface). That address IS on the
    `SO_BINDTODEVICE` interface, so the connection IS locally delivered
    (unlike the old bridge, which was on a different interface). The port is
    kept unchanged.

3.  The sandbox therefore connects to `<address>:${forwardPort}`, which pasta
    translates to `<host-global-addr>:${forwardPort}`, where the forwarder
    accepts and proxies to `127.0.0.1:${port}` (LiteLLM).

**Why this is port-scoped (the key isolation property).** The sandbox can
reach `<address>:${forwardPort}` (the forwarder → LiteLLM) and other ports
on the host's global address — but only services that already bind to
`0.0.0.0` (i.e., are already network-accessible). Loopback-ONLY services
(bound to `127.0.0.1`) are **not** reachable, because `--map-guest-addr`
maps to the host's global address, not to `127.0.0.1`. This closes the
loophole of the previous `--map-host-loopback` mechanism, which translated
*every* port on the mapped address to `127.0.0.1` and so exposed the entire
host loopback to a possibly hostile agent.

This works where the alternatives do not (see above): `-T` is invisible to
runsc's netstack, and `host.containers.internal` maps to the host global
address, not the loopback. An earlier version used `--map-host-loopback`,
which worked for reachability but was address-scoped (all loopback ports).

The pasta options are baked into the `agent-gvisor` wrapper as
`AGENT_GVISOR_NETWORK` (see `./default.nix`): a
`pasta:--map-guest-addr,<address>` podman network spec. Podman's default
`--no-map-gw` applies (no gateway→loopback mapping). The old `--map-gw` flag
(a podman-only flag, NOT a real pasta option) was dropped: the pasta source
shows `--map-host-loopback`/`--map-guest-addr` set their config fields
directly, and `--no-map-gw` only fills in the *default* (gateway) mapping
when those fields are still unspecified — an explicit mapping is never
overridden. `agent-gvisor start` uses `AGENT_GVISOR_NETWORK` as the default
`--network`, and `agent-gvisor doctor` probes the endpoint through the same
network, so the check exercises the real path. Override per session with
`--network`, or `--network none` for an offline session.

An earlier version exposed the proxy through a member-less bridge
(`agentsbr0`, `192.168.84.1`) plus a socket-activated `systemd-socket-proxyd`
bound to it. That did **not** work: pasta binds its outbound sockets to the
host's default-route interface, so a sandbox connection to the host-local
bridge address egresses that interface toward its gateway instead of being
locally delivered, and the socket's `BindToDevice` rejected anything not
arriving on the bridge. The old bridge approach and the intermediate
`--map-host-loopback` fix (address-scoped, exposing every loopback port) are
documented in the header comment of `./litellm-endpoint.nix`.

The host LiteLLM proxy itself is not touched and stays loopback-only. It is
enabled automatically wherever `services.litellm.enable` is on; knobs live
under `myconfig.ai.gvisor-agent-sandbox.litellm.{enable,port,forwardPort,address}`, and
the resulting base URL is the read-only `…litellm.endpoint`.

Use it from a session (the generated env file carries only the base URL — no
secret ends up in the Nix store or in the session state):

```bash
agent-gvisor demo --repo ~/src/foo \
  --env-file ~/.config/agent-gvisor/litellm.env \
  --env OPENAI_API_KEY="$OPENAI_API_KEY" \
  -- pi

agent-gvisor shell demo -c 'curl -s "$OPENAI_BASE_URL/models"'
```

Networking is otherwise podman's rootless default (pasta, full outbound NAT);
pass `--network none` for an offline session.

First run on a host:

```bash
agent-gvisor-load-image   # load the Nix-built image into the rootless store
agent-gvisor doctor       # verify runtime, image and sandbox startup
```

### Image freshness check

The image tag (`localhost/agent-dev:latest`) is reused by every rebuild, so
"is this reference present?" does not answer "is the *current* artifact
loaded?". After adding an agent to `extraImagePackages`, or after any input
change, the store still holds the previous build under the same tag.

`agent-gvisor-load-image` therefore compares **image IDs**: the digest of the
OCI config blob, which Podman reports as `.Id` and which a docker-archive
records in `manifest.json` as its config file name. The expected digest is
extracted from the tarball once **at build time** (derivation
`agent-gvisor-image-id`) and baked into the wrapper, so no invocation has to
decompress the multi-hundred-MB image.

```bash
agent-gvisor-load-image           # load if absent or stale, else no-op
agent-gvisor-load-image --force   # reload unconditionally
agent-gvisor-load-image --test    # report only; exit 0 iff already current
```

`--test` never touches the store and prints the full state, which makes it
usable in scripts and health checks:

```
image:    /nix/store/…-agent-dev.tar.gz
ref:      localhost/agent-dev:latest
expected: sha256:0803…
loaded:   sha256:0803…
state:    current
```

`state` is `current` (exit 0), `stale` (a different build is loaded under the
tag, exit 1) or `absent` (nothing loaded under the tag, `loaded: -`, exit 1).
The same report is written to stderr after a load, so a plain run also shows
what the store now holds.

See `docs/upstream-README.md` for the full CLI reference.

### Building the packages

The packages are exposed through the host's package set rather than as
top-level flake `packages` outputs (the repo's `packages` output only carries
the ISO and the impure microvm runners, and `nix flake check` would otherwise
build the multi-hundred-MB OCI image on every check):

```bash
./build-pkg-for-host.sh agent-gvisor-configured f13
nix build --impure --expr '(builtins.getFlake (toString ./.)).nixosConfigurations.f13.pkgs.agent-gvisor-image'
```
