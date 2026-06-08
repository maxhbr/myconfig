# step-ca: secrets the priv repo must supply

This repo declares the internal CA's NixOS configuration but leaves all
sensitive material (root key, intermediate key, intermediate-key
passphrase) as **stubs**. The actual files live in the sibling
`../priv/` repo, which provides `myconfig.secrets.<name>.source = ./...;`
overrides on top of every host evaluation.

This file documents *exactly* what the priv repo must contribute so
that step-ca on vserver can boot and Caddy on every host can obtain
ACME certificates from the internal CA.

## Helper script

The public repo ships `scripts/priv.template/mk_step-ca_keys.sh`. Copy
it into your priv repo (or invoke it from the priv-repo root if your
checkout includes the template) and run:

```bash
# In the priv repo checkout:
./mk_step-ca_keys.sh vserver
```

This single command:

1. Generates a 10-year EC P-256 **root CA** under
   `hosts/host.vserver/secrets/step-ca/root_ca.{crt,key}` (key
   protected by `root_ca_key_password`).
2. Generates a 4-year EC P-256 **intermediate CA** signed by the root,
   at `hosts/host.vserver/secrets/step-ca/intermediate_ca.{crt,key}`
   (key protected by `password`).
3. Writes both passphrases as plaintext files under the same directory.
4. Copies `root_ca.crt` over `modules/myconfig.deployedServices/
   internalCa/root.crt` in the public repo, so every host's trust
   store picks up the real internal-CA root.
5. `git add`s the new files in the priv repo.

The script is idempotent-by-refusal: if `hosts/host.vserver/secrets/
step-ca/` already exists it bails out rather than clobbering keys. To
regenerate, delete the directory first.

## Stub secrets in the public repo

`hosts/host.vserver/services.step-ca.nix` declares four
`myconfig.secrets.*` entries with `source = null` (the default). The
priv overlay must supply matching `source = ...` paths:

| Stub name | What it stores | Default `dest` | Owner |
|---|---|---|---|
| `step-ca-intermediate-password` | passphrase for `intermediate_ca_key` | `/run/step-ca-intermediate-password` | `root:root 0400` |
| `step-ca-root-ca-crt` | PEM root cert | `/run/step-ca-root-ca-crt` | `step-ca:step-ca 0444` |
| `step-ca-intermediate-ca-crt` | PEM intermediate cert | `/run/step-ca-intermediate-ca-crt` | `step-ca:step-ca 0444` |
| `step-ca-intermediate-ca-key` | encrypted PEM intermediate key | `/run/step-ca-intermediate-ca-key` | `step-ca:step-ca 0400` |

The secrets framework (`modules/myconfig.secrets.nix`) encrypts each
plaintext at Nix eval time using vserver's
`/etc/ssh/ssh_host_rsa_key.pub` (looked up from
`hosts/metadata.json`) and produces a per-host `.age` file in vserver's
nix store. Plaintext never leaves `../priv/`.

`root_ca_key` and `root_ca_key_password` are **not** deployed — they
stay in the priv repo as cold storage for intermediate rotation /
disaster recovery only.

## Priv-overlay module wiring

In whatever priv-repo module already drives vserver's `moreModules`
(see public-repo `flake.nix:159-260`), add the `source` overrides:

```nix
# priv/.../vserver-priv.nix (or wherever your overlay lives)
{ ... }:
{
  myconfig.secrets = {
    step-ca-intermediate-password.source =
      ./hosts/host.vserver/secrets/step-ca/password;
    step-ca-root-ca-crt.source =
      ./hosts/host.vserver/secrets/step-ca/root_ca.crt;
    step-ca-intermediate-ca-crt.source =
      ./hosts/host.vserver/secrets/step-ca/intermediate_ca.crt;
    step-ca-intermediate-ca-key.source =
      ./hosts/host.vserver/secrets/step-ca/intermediate_ca_key;
  };
}
```

Adjust the relative paths to your priv-repo layout. After this is in
place:

```bash
nix eval --raw .#nixosConfigurations.vserver.config.age.secrets.step-ca-intermediate-password.file
# → /nix/store/<hash>-step-ca-intermediate-password.age (not an error)
nix flake check
# → no "myconfig.secrets: source is missing for: step-ca-*" warnings
```

## Bootstrap sequence (combined: public + priv repos)

The full one-time bootstrap, end to end:

1. **Deploy the current public-repo state to vserver.** `step-ca.service`
   activates but will fail to start because the four secrets it needs
   don't exist yet. That is expected.

2. **In the priv repo, generate keys:**
   ```bash
   ./mk_step-ca_keys.sh vserver
   ```
   This populates `hosts/host.vserver/secrets/step-ca/` and overwrites
   the placeholder `root.crt` in the public repo.

3. **Commit the new `root.crt` in the public repo** (it changed from
   the placeholder to your real root). Push.

4. **Add the priv-overlay `source = ...` wiring** as shown above and
   commit + push the priv repo.

5. **Re-deploy vserver.** Now all four
   `step-ca-*-key.service` units write decrypted material to
   `/run/step-ca-*`, step-ca's `LoadCredential=` picks up the
   passphrase, step-ca's own `settings.{root,crt,key}` point at the
   decrypted certs/key, and the daemon starts serving the ACME
   directory at `https://ca.vserver.wg0.maxhbr.local:8443/acme/acme/directory`.

6. **Re-deploy every Caddy host** (thing, nuc, p14, vserver). Caddy
   replaces its `tls internal` certs with ACME-issued certs from the
   internal CA. Verify from any host:

   ```bash
   curl --verbose https://forgejo.thing.wg0.maxhbr.local/
   # Should succeed with no TLS warning; the cert chain ends at
   # "MyConfig Internal CA Root" which is in the system trust store.
   ```

## Rotation

* **Intermediate key**: rotate every few years. Easiest path: generate
  a new intermediate locally signed by the same root using the same
  procedure as step 5 of `mk_step-ca_keys.sh` (e.g. `step certificate
  create "MyConfig Internal CA Intermediate" intermediate_ca.crt
  intermediate_ca_key --profile intermediate-ca --ca root_ca.crt
  --ca-key root_ca_key --ca-password-file root_ca_key_password
  --password-file password`). Replace `intermediate_ca.{crt,key}` in
  the priv repo and re-deploy vserver. Leaf certs already issued
  remain valid until expiry.

* **Root key**: should outlive the intermediate by ~10×. Root rotation
  is a forklift event — every host's trust store has to pick up the new
  `root.crt`, and every Caddy leaf has to be re-issued under the new
  chain. Plan accordingly.

* **Passphrase only**: if you only want to change the intermediate
  passphrase without rotating keys, use `step crypto change-pass` on
  `intermediate_ca_key`, then update `password` in the priv repo.

## Disaster recovery

* `hosts/host.vserver/secrets/step-ca/{root_ca_key,root_ca_key_password}`
  in the priv repo are **the only way** to mint new intermediates if
  the current one is compromised or expires. Treat them like crown
  jewels: the priv repo is already replicated across thing + p14, but
  consider an additional offline backup (USB stick in a safe etc.).

* `intermediate_ca_key` is also in the priv repo; losing it means
  re-deploying a new intermediate (same procedure as rotation).
  Existing leaf certs keep working until expiry.

* If the priv repo is completely lost, you have to:
  1. Run `mk_step-ca_keys.sh vserver` to generate a fresh CA.
  2. Commit the new `root.crt` in the public repo.
  3. Re-deploy every host to pick up the new trust anchor.
  4. Re-deploy every Caddy host to re-issue leaf certs.

## Emergency switch-off

If step-ca is unreachable (vserver down, key corruption, intermediate
expired, …), set on the affected host or globally:

```nix
myconfig.deployedServices.internalCa.useAcmeForCaddy = false;
```

Caddy on that host falls back to `tls internal` (per-host self-signed),
clients see TLS warnings again, but the services keep serving. Revert
when step-ca is healthy again.
