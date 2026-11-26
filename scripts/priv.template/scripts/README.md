# priv repo secret scripts

Per-script usage for `scripts/`. The secret layout, the standards for
where secrets may live, the consumption paths and the recipients of
every `*.age` file are documented in the repo root
[README.md](../README.md) — the canonical place; not repeated here.

## One-time migration from git-crypt

```bash
scripts/mk_age_master_key.sh                  # master key, never enters git
scripts/secrets-migrate-from-git-crypt.sh     # moves, encrypts, drops git-crypt
scripts/secrets-verify.sh --install-hooks     # pre-commit guard
git status && git commit                      # review, then commit
```

Afterwards, re-point consumers that referenced the moved `secrets/`
paths to their `secrets-plain/` counterparts (the migration script
prints every move).

`*.nix` files under `secrets/` contain no secrets (they wire up
`myconfig.secrets` entries and imports), so the migration leaves them
tracked and unencrypted where they are; `secrets-encrypt.sh` and
`secrets-verify.sh` ignore them.

## Daily use

- new key material: run the repo-root `mk_*_keypair.sh` scripts; they
  write plaintext into `secrets-plain/` and call `secrets-encrypt.sh`
- after editing any plaintext: `scripts/secrets-encrypt.sh`
- fresh checkout: `scripts/secrets-decrypt.sh`
- `scripts/secrets-verify.sh` warns about plaintext without committed
  ciphertext; with `--check-index` it also rejects tracked/staged
  plaintext (installed as a pre-commit hook)
