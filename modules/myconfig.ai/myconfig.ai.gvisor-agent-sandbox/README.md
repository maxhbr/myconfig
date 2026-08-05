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
host-side chat front-ends `aichat` / `llm` are deliberately excluded. Override
to slim down or extend:

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
from `home.rewriteEndpoints` (default: the loopback and `localhost` forms of
the LiteLLM URL → the bridge endpoint `http://192.168.84.1:4000`) to the
seeded files; binary files are skipped. Set the option to `[ ]` to copy the
configuration verbatim.

If you would rather keep `localhost:4000` working literally inside the
sandbox, pass `--network 'pasta:-T,4000'` instead: podman starts pasta with
`-T none`, and `-T 4000` re-enables namespace→host forwarding for that one
port. That is an alternative to the rewrite, not a requirement for it.

Per invocation: `--no-home-seed` for an empty home, `--home-seed PATH` for a
different source tree, `AGENT_SANDBOX_HOME_SEED` /
`AGENT_SANDBOX_HOME_SEED_PATHS` / `AGENT_SANDBOX_HOME_SEED_REWRITE` to
override source, allowlist and rewrite rules. Turn it off for the host with
`myconfig.ai.gvisor-agent-sandbox.home.enable = false`.

### Model access (host LiteLLM)

`services.litellm` is loopback-only on purpose, which a rootless sandbox
cannot reach (with pasta the sandbox's `127.0.0.1` is its own loopback, not
the host's). `./litellm-bridge.nix` therefore adds the same construction the
microVM tier uses (`../myconfig.ai.microvm/network.nix` §16):

- a member-less bridge `agentsbr0` carrying the single host address
  `192.168.84.1/24` — a stable, non-loopback, non-LAN address that pasta can
  reach, because pasta re-opens the sandbox's outbound connections in the
  host network namespace,
- `systemd.sockets.gvisor-agent-sandbox-litellm-proxy`, bound *only* to
  `192.168.84.1:4000` (`BindToDevice` + `FreeBind`, ordered after
  `agentsbr0-netdev.service`), handing connections to
  `systemd-socket-proxyd 127.0.0.1:4000`,
- an `GVISOR_AGENT_SANDBOX_INPUT` firewall chain that accepts that address
  and port only on `lo` and on the bridge, and drops everything else
  addressed to it, so the endpoint is never reachable from the LAN.

The host LiteLLM proxy itself is not touched and stays loopback-only. It is
enabled automatically wherever `services.litellm.enable` is on; knobs live
under `myconfig.ai.gvisor-agent-sandbox.litellm.{enable,port,bridgeName,address,prefixLength}`,
and the resulting base URL is the read-only `…litellm.endpoint`.

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
