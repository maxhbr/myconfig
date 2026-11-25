# priv repo secret scripts

Scripts managing the age/agenix secret layout of the priv repo.

## Layout

```
hosts/host.<h>/secrets-plain/   gitignored plaintext (Kind B + Kind A sources)
hosts/host.<h>/secrets/         committed *.age ciphertext
secrets-plain/                  gitignored plaintext, host-agnostic
secrets/                        committed *.age ciphertext + secrets.nix (agenix manifest)
```

Plaintext (`secrets-plain/`) is what the flake evaluates by default:
nix cannot decrypt age at eval time, so `builtins.readFile` and
imported `*.nix` files (Kind B) must point there. For `myconfig.secrets`
sources (Kind A) there are two equivalent forms:

- `source = <secrets-plain/...>` — plaintext; `myconfig.secrets`
  encrypts it to the host key at build time (the classic path), or
- `source = <secrets/...age>` — the committed ciphertext itself,
  encrypted to the target host's key by `secrets-encrypt.sh`;
  `myconfig.secrets` detects the `.age` suffix and passes it to agenix
  unchanged (no build-time re-encryption, no plaintext needed on the
  eval machine).

Both decrypt on the host identically. Prefer the `.age` form once a
secret's consumers are migrated — the plaintext then only needs to
exist on the machine that edits/encrypts secrets, not on every machine
that evaluates the flake.

## One-time migration from git-crypt

```bash
scripts/mk_age_master_key.sh                  # master key, never enters git
scripts/secrets-migrate-from-git-crypt.sh     # moves, encrypts, drops git-crypt
scripts/secrets-verify.sh --install-hooks     # pre-commit guard
git status && git commit                      # review, then commit
```

Afterwards, re-point consumers that referenced the moved `secrets/`
paths to their `secrets-plain/` counterparts (the migration script
prints the moved `*.nix` files).

## Daily use

- new key material: run the repo-root `mk_*_keypair.sh` scripts; they
  write plaintext into `secrets-plain/` and call `secrets-encrypt.sh`
- after editing any plaintext: `scripts/secrets-encrypt.sh`
- fresh checkout: `scripts/secrets-decrypt.sh`
- `scripts/secrets-verify.sh` warns about plaintext without committed
  ciphertext; with `--check-index` it also rejects tracked/staged
  plaintext (installed as a pre-commit hook)

## Recipients

Each `*.age` file is encrypted to the master key
(`AGE_MASTER_KEY`, default `~/myconfig/.age-master.key`) plus,
when the target host can be derived from the path, that host's ssh host
key from `hosts/metadata.json`
(`hosts.<host>.pubkeys."/etc/ssh/ssh_host_rsa_key.pub"`) — the same key
`myconfig.secrets` encrypts to, so an `.age` source decrypts on the
host with agenix's existing `age.identityPaths`, including the
impermanence-host override.

Back up the master key offline. Losing it makes every `*.age` file
undecryptable; it is the replacement for the git-crypt "default" key.
