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

## The sidecar directory
The `config.toml` file in the sidecar defines defines

- which repos are accessible
  - by default is /path/to/the/repo available as rw
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
none
## On the Roadmap:
### next:
- bubblewrap (https://git.sr.ht/~alexdavid/jail.nix)
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
