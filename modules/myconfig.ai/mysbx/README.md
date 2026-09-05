# mysbx

This project defines the cli tool mysbx, which is my sandboxing tool.

## Documentation

The `docs/` directory holds all documentation. This README stays a short
overview; design decisions, TODOs and any other long text belong in `docs/`:

- [`CONTEXT.md`](./CONTEXT.md) - the glossary: repo, sidecar, user config,
  layer, backend, payload, base, mount
- [`docs/plan.md`](./docs/plan.md) - the phase plan, starting with the MVP;
  work items in [`docs/TODOs/`](./docs/TODOs)
- [`docs/feature-comparison.md`](./docs/feature-comparison.md) - how mysbx
  compares to the sandboxing tiers already implemented in myconfig (CLI,
  sandboxing features, implementation state)
- `docs/design/` - design decisions
  - [`docs/design/cli.md`](./docs/design/cli.md) - the command line surface
  - [`docs/design/config.md`](./docs/design/config.md) - configuration and the
    sidecar directory
- `docs/TODOs/` - planned work, one file per TODO

## How to use:

```
$ pwd
/path/to/the/repo
$ mysbx init
# init...
## created: /path/to/the/repo.mysbx/
## created: /path/to/the/repo.mysbx/config.toml
$ mysbx
```

`mysbx run -- CMD...` runs one command in the sandbox;
`mysbx run --dry-run -- CMD...` prints the exact `bwrap` argv it would
execute (one argument per line) and exits without running it — the
acceptance surface described in [`docs/design/cli.md`](./docs/design/cli.md).

`--verbose` prints what the run is configured to do before it happens
(cli.md D10) — on stdout, every line prefixed `## `, so it can be combined
with `--dry-run` and stripped again with `grep -v '^## '`:

```
$ mysbx --verbose --dry-run
## mysbx 0.1.0 — run configuration
## repo root:      /path/to/the/repo
## sidecar:        /path/to/the/repo.mysbx (missing)
## user config:    /home/user/.config/mysbx/config.toml (loaded)
## sidecar config: /path/to/the/repo.mysbx/config.toml (absent — empty layer)
## backend:        bubblewrap
## network:        shared (--share-net)
## mounts:         2 (in declaration order)
##   rw /path/to/the/repo -> /path/to/the/repo  [repo, implicit]
##   ro /home/user/data -> /data  [user config]
## env:            1 forwarded from the host, 1 from [env] (values shown verbatim — they may be secrets)
##   TERM=xterm-256color  [host]
##   EDITOR=nvim  [config]
##   PATH=/nix/store/…-mysbx-tools/bin  [tools]
## bwrap:          /nix/store/…-bubblewrap/bin/bwrap
## shell:          /nix/store/…-bash/bin/bash
## tools PATH:     /nix/store/…-mysbx-tools/bin
## payload:        shell /nix/store/…-bash/bin/bash
## mode:           dry run — the argv follows, nothing is executed
--clearenv
--unshare-all
…
```

Note that `[env]` values are printed verbatim and may be secrets.

## The sidecar directory
The `config.toml` file in the sidecar defines

- additional mounts into the sandbox and the forwarded environment
  - the repo itself is implicit: always available rw at its real path,
    not expressible in the config
- ...

It is deliberately placed outside of the repo and the sandbox.

The directory has room for:
- state files
- mounts for ~/.share
- ...

## Configuration:
A system wide config in `$XDG_CONFIG_HOME/mysbx/config.toml` defines system wide defaults, especially which (agent) config files from the host should be available in the sandbox

# Supported Technologies
## Already Implemented:
- bubblewrap — the MVP (phase 1 in [`docs/plan.md`](./docs/plan.md)):
  `mysbx run [--dry-run] -- CMD` and the bare interactive form, driven by the
  user config + sidecar layers, with the bwrap binary, payload shell and
  dev-tool PATH all pinned from the Nix package (see [`nix/mysbx.nix`](./nix/mysbx.nix))
## On the Roadmap:
### next:
- the packaged bubblewrap tier is expected to evolve per phase 2 in
  [`docs/plan.md`](./docs/plan.md) (generated user config, credentials,
  network policy, `sandboxTools` integration, further backends)
## after that:
- container (via podman), with gvisor for additional layer of security
- nono
### long term:
- qemu
- microvm

## Relation to other sandboxing methods
The myconfig repo contains several implementations for sandboxing which are all in alpha/beta state and WIP. This repo here has the goal to learn from them and implement the future implementation. The goal is to replace all sandboxing implemented in myconfig.

A feature-by-feature comparison of those implementations against mysbx is in
[`docs/feature-comparison.md`](./docs/feature-comparison.md).
