# priv repo secret scripts

Scripts managing the age/agenix secret layout of the priv repo.

## Layout

```
hosts/host.<h>/secrets-plain/   gitignored plaintext (Kind B + Kind A sources)
hosts/host.<h>/secrets/         committed *.age ciphertext
secrets-plain/                  gitignored plaintext, host-agnostic
secrets/                        committed *.age ciphertext + secrets.nix (agenix manifest)
```

Plaintext (`secrets-plain/`) is what the flake evaluates: nix cannot
decrypt age at eval time, so `myconfig.secrets.<name>.source`,
`builtins.readFile` and imported `*.nix` files must point there. The
committed `*.age` files are encrypted backups/provisions of the same
data; the agenix runtime (myconfig.secrets → age.secrets) re-encrypts
the plaintext per host at build time, exactly as before the migration
— only the at-rest-in-git protection changed (git-crypt → age).

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
`myconfig.secrets` encrypts to, so host-side agenix decryption keeps
working with its existing `age.identityPaths`.

Back up the master key offline. Losing it makes every `*.age` file
undecryptable; it is the replacement for the git-crypt "default" key.
