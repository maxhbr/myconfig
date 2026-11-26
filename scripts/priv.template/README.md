# priv repo: sources, secrets and their management

This repository is the private half of a two-repo NixOS configuration:

- **public repo** `myconfig` (default sibling checkout `../myconfig`,
  upstream `https://github.com/maxhbr/myconfig`): NixOS modules
  (`modules/`), the common host definitions, `flake.lib.nix` with
  `nixosConfigurationsGen` and the `myconfig.secrets` machinery.
- **this repo** `priv` (this checkout, default sibling `../priv`):
  per-host overrides in `hosts/host.<name>/`, metadata overrides in
  `hosts/metadata.json` (merged *on top of* the public metadata), and
  all secrets.
- **flake input sources**: `../myconfig/get_input.sh <input-name>`
  prints the pinned store path of any flake input (nixpkgs, agenix,
  home-manager, ...). Local checkouts of some upstream repositories
  live in `../nixos/` (nixpkgs, home-manager, nixos-hardware).
- **template**: this repo was seeded from the `priv.template` branch
  of the public repo; the layout of scripts, `flake.nix` skeleton and
  this README are maintained there (`scripts/priv.template/` in the
  public repo) and instantiated via `scripts/priv.template.setup.sh`.
  Improvements to the layout should be made in the template and
  re-applied here.

The two flakes compose: this flake builds hosts as
`myconfig.nixosConfigurationsGen."host-<name>" (hosts/host.<name>) metadataOverrides`,
so every host is "public module set + private overrides + private secrets".

## Standards: where secrets may live

| kind | location | in git? |
| --- | --- | --- |
| age ciphertext | `<x>/secrets/**.age` | **yes — the default** |
| hashes & public material (hashedPassword, ssh/wireguard *pub*keys, metadata) | inline in nix / `hosts/metadata.json` | **yes** — hashes and public keys are not secrets |
| plaintext secrets | `<x>/secrets-plain/` (gitignored) | **no** — only on the machine that edits/encrypts secrets |
| plaintext that cannot be encrypted (rare, see below) | inline in nix, with a justification comment | **yes, only with an inline explanation** |
| `*.nix` wiring under `secrets/` | where they are | **yes** — they contain no secrets and are ignored by the encryption/verification scripts |

Rules:

1. **Committed `.age` is the only sanctioned form for real secrets.**
   Every runtime secret lives as `<x>/secrets/**.age`, encrypted to the
   target host's ssh host key plus the master age key.
2. **Plaintext never enters git.** The gitignored `secrets-plain/`
   trees are work material for the secret-*editing* machine only.
   Install the guard once per clone:
   `scripts/secrets-verify.sh --install-hooks` — it rejects staged or
   tracked plaintext and warns about plaintext without committed
   ciphertext.
3. **Nix code never references `secrets-plain/`.** Nix cannot keep
   secrets: `builtins.readFile`, `environment.etc.*.source` and
   interpolated strings copy the file into the world-readable store.
   Consumers must go through the `.age` runtime route, or be an
   explained exception (below). A `builtins.readFile` on a secret is a
   bug even when it reads from `secrets-plain/` — the secret still
   ends up in the store.
4. **Explained-plaintext exception.** The only secrets that may be
   committed unencrypted are *bootstrap identities* that cannot be
   delivered through the encryption machinery itself (chicken and
   egg), e.g. the pre-baked ssh host key of a never-booted host:
   every `.age` file of that host is encrypted to that very key, so it
   must ship with the repo/image and be materialized before the first
   activation (`system.activationScripts.preAgenixSshHostKey`).
   Such a file is committed as plaintext **and** the nix file using it
   must carry a comment explaining why that is acceptable (exposure
   bounded: matching public key is public anyway, authorizes nothing
   yet, rotate on first provisioning if the repo/image was shared).
5. **Build-time consumers must not consume secrets.** If a service
   genuinely needs a secret at build time, restructure so the secret
   is injected at runtime (activation script assembling an included
   config, systemd `EnvironmentFile`, agenix `dest`), like the f13
   github token: the token is an agenix secret and an activation
   script assembles `/etc/nix/gh-token.conf` which the store's
   `nix.conf` pulls in via nix's `!include`.

## How secrets are managed (age + agenix)

Layout:

```
hosts/host.<h>/secrets/         committed *.age ciphertext of host <h>
hosts/host.<h>/secrets-plain/   gitignored plaintext (editing machine only)
secrets/                        committed host-agnostic *.age + secrets.nix
secrets/secrets.nix             generated agenix manifest (recipients per file)
```

Recipients: every `*.age` file is encrypted to

- the **master age key** (`AGE_MASTER_KEY`, default
  `~/myconfig/.age-master.key`, never enters any repository, back it
  up offline — it replaces the old git-crypt default key and can
  re-provision any checkout), plus
- the **target host's ssh host public key**, resolved from the merged
  public+private metadata via
  `hosts.<host>.pubkeys."/etc/ssh/ssh_host_rsa_key.pub"` — the same key
  agenix's default `age.identityPaths` decrypts with on the host.

### Consumption paths on a host

- **`myconfig.secrets.<name>.source = ./secrets/**.age`** (preferred):
  `myconfig.secrets` detects the `.age` suffix and passes
  the file through to agenix unchanged. agenix decrypts during
  activation (`agenixNewGeneration`) into `dest`
  (default `/run/agenix/<name>`, root-owned, 0400). No plaintext is
  needed at eval or build time anywhere.
- **plaintext source form** (`source = <secrets-plain/...>`):
  `myconfig.secrets` encrypts it to the host key at build time. It
  works, but requires plaintext on every evaluating machine — do not
  use it in this repo; commit the `.age` instead.
- **bootstrap identities**: materialized before agenix by an early
  activation script (see optiplex), because agenix itself depends on
  them.

### Scripts (priv repo root and `scripts/`)

Per-script usage (migration walkthrough, daily-use commands, hook
installation): [scripts/README.md](scripts/README.md).

| script | purpose |
| --- | --- |
| `scripts/mk_age_master_key.sh` | one-time: create the master age key |
| `scripts/secrets-encrypt.sh` | re-encrypt every `secrets-plain/` change into committed `secrets/**.age` and regenerate `secrets/secrets.nix` |
| `scripts/secrets-decrypt.sh` | fresh checkout / editing: decrypt all `*.age` into `secrets-plain/` |
| `scripts/secrets-verify.sh [--install-hooks]` | guard: no tracked/staged plaintext, no orphaned plaintext |
| `scripts/secrets-migrate-from-git-crypt.sh` | one-time git-crypt → age migration |
| `mk_*_keypair.sh` (repo root) | generate new key material (ssh, wireguard, tls, syncthing, gpg, step-ca, ...) into `secrets-plain/` and encrypt it |

### Daily workflow

- **evaluating/building**: no plaintext needed — hosts decrypt their
  committed `*.age` at activation with their ssh host key.
- **editing a secret**: change the plaintext under `secrets-plain/`
  (decrypt first on a fresh checkout), then
  `scripts/secrets-encrypt.sh`, then commit only the `*.age` and the
  regenerated manifest.
- **new host**: collect its ssh host pubkey into
  `hosts/metadata.json` (`pubkeys."/etc/ssh/ssh_host_rsa_key.pub"`),
  generate its key material with the `mk_*_keypair.sh` scripts, run
  `secrets-encrypt.sh`. Without a host pubkey in the metadata the
  files are encrypted to the master key only and agenix cannot
  decrypt them on the host.
