# myconfig.ai.gvisor-agent-sandbox

Rootless Podman + gVisor (`runsc`) sandboxes for coding agents, providing the
`agent-session` session manager and a Nix-built sandbox image.

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
`bin/agent-session` (locally patched `start`).

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

- `README.md` → `docs/upstream-README.md` (verbatim upstream documentation:
  usage of `agent-session`, image contents, isolation boundaries,
  troubleshooting). Its "install as a flake input" section does not apply
  here.

Patched:

- `bin/agent-session` — `start` no longer dies outright when a session of
  the same name exists. With the new `start --force` it destroys the old
  session (including its branch) unattended; otherwise, on a terminal, it
  asks `session NAME already exists; destroy it and delete branch BRANCH?
  [y/N]` and, on `y`, runs `destroy NAME --force --delete-branch` before
  creating the new session. Without a terminal and without `--force` the
  previous fail-fast behaviour is kept, now with the exact recovery command
  in the message. `start` also seeds `/home/agent` from the activated
  home-manager generation (`--home-seed`, `--no-home-seed`,
  `AGENT_SANDBOX_HOME_SEED*`; see above). The destroy runs
  in a subshell because it sources the old session's `meta`, which would
  otherwise clobber `cmd_start`'s locals through bash's dynamic scoping.
  Additionally `start` honours `AGENT_SANDBOX_NETWORK` as the default
  `--network` (overridable per session), and `doctor` probes the model
  endpoint through that same network so the check exercises the real path —
  both needed to make a host-loopback model endpoint reachable from a runsc
  sandbox (see *Model access (host LiteLLM)*).

Kept unchanged in substance (only nixfmt-rfc-style reformatting):

- `nix/overlay.nix`, `nix/agent-session.nix`, `nix/agent-image.nix`,
  `nix/load-image.nix` — the package definitions.

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
`defaultCommand`. A bare `agent-session start --name x --repo …` therefore
drops you into `herdr` rather than a plain shell; `-- COMMAND` still wins, and
`agent-session shell` always gives a shell. Its host configuration is seeded
via `.config/herdr` in `home.seedPaths`. Set `defaultCommand = null` for the
upstream `/bin/bash` behaviour, or to any command line (word-split, e.g.
`"herdr --flag"`).

Override the package list to slim down or extend:

```nix
myconfig.ai.gvisor-agent-sandbox.extraImagePackages = [ pkgs.claude-code ];
```

When enabled the module

- bakes the enabled agent CLIs into the image (see above),
- adds `nix/overlay.nix` to `nixpkgs.overlays`, providing `agent-session`,
  `agent-sandbox-image` and `agent-sandbox-load-image`,
- enables rootless Podman and registers `${pkgs.gvisor}/bin/runsc` as the
  `runsc` OCI runtime in `containers.conf`,
- installs `agent-session`, `agent-sandbox-load-image` and `gvisor` into the
  user's `home.packages`,
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

Baking the dotfiles into the *image* would not work: `agent-session` bind
mounts the per-session home over `/home/agent`, which masks whatever the
image carried there (upstream creates the XDG dirs host-side for exactly that
reason).

Two consequences of the isolation boundary:

- `home-files` is a forest of symlinks into `/nix/store` and the sandbox has
  no `/nix`, so files are copied **dereferenced**. Configuration whose
  *content* points at `/nix/store` paths still dangles inside.
- The tree also contains `.ssh`, mail and browser configuration, so the copy
  is an **allowlist**, `myconfig.ai.gvisor-agent-sandbox.home.seedPaths`,
  baked into the wrapper as `AGENT_SANDBOX_HOME_SEED_PATHS`. Default:
  `.agents .agignore .claude .codex .pi .config/git .config/opencode`.
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
`localhost:4000` work literally inside the sandbox is **not** an available
alternative, despite what the podman/pasta documentation suggests:

- `--network 'pasta:-T,4000'` (namespace→host forwarding) does not help,
  because `runsc` runs its **own network stack**. pasta's `-T` listener lives
  in the container's Linux netns, while the container's processes see gVisor's
  internal loopback — verified on f13, `curl 127.0.0.1:4000` inside the
  sandbox gets `Connection refused`.
- `host.containers.internal` does not help either: podman configures pasta
  with `--map-guest-addr 169.254.1.2`, and that maps to the host's *global*
  address, not to its loopback (`pasta(1)`), so a loopback-only service is not
  behind it.

Verify the whole chain from inside a sandbox with `agent-session doctor`,
which probes `AGENT_SANDBOX_MODEL_ENDPOINT` (baked from `litellm.endpoint`)
through the same pasta network a session uses (see below) in a throwaway
container and accepts any HTTP status as "reachable".

Per invocation: `--no-home-seed` for an empty home, `--home-seed PATH` for a
different source tree, `AGENT_SANDBOX_HOME_SEED` /
`AGENT_SANDBOX_HOME_SEED_PATHS` / `AGENT_SANDBOX_HOME_SEED_REWRITE` to
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

2.  The `agent-session` wrapper bakes `AGENT_SANDBOX_NETWORK` as a
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

The pasta options are baked into the `agent-session` wrapper as
`AGENT_SANDBOX_NETWORK` (see `./default.nix`): a
`pasta:--map-guest-addr,<address>` podman network spec. Podman's default
`--no-map-gw` applies (no gateway→loopback mapping). The old `--map-gw` flag
(a podman-only flag, NOT a real pasta option) was dropped: the pasta source
shows `--map-host-loopback`/`--map-guest-addr` set their config fields
directly, and `--no-map-gw` only fills in the *default* (gateway) mapping
when those fields are still unspecified — an explicit mapping is never
overridden. `agent-session start` uses `AGENT_SANDBOX_NETWORK` as the default
`--network`, and `agent-session doctor` probes the endpoint through the same
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
agent-session start --name demo --repo ~/src/foo \
  --env-file ~/.config/agent-sandbox/litellm.env \
  --env OPENAI_API_KEY="$OPENAI_API_KEY" \
  -- pi

agent-session shell demo -c 'curl -s "$OPENAI_BASE_URL/models"'
```

Networking is otherwise podman's rootless default (pasta, full outbound NAT);
pass `--network none` for an offline session.

First run on a host:

```bash
agent-sandbox-load-image   # load the Nix-built image into the rootless store
agent-session doctor       # verify runtime, image and sandbox startup
```

See `docs/upstream-README.md` for the full CLI reference.

### Building the packages

The packages are exposed through the host's package set rather than as
top-level flake `packages` outputs (the repo's `packages` output only carries
the ISO and the impure microvm runners, and `nix flake check` would otherwise
build the multi-hundred-MB OCI image on every check):

```bash
./build-pkg-for-host.sh agent-session f13
nix build --impure --expr '(builtins.getFlake (toString ./.)).nixosConfigurations.f13.pkgs.agent-sandbox-image'
```
