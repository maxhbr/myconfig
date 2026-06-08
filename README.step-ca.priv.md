# step-ca: secrets the priv repo must supply

This repo declares the internal CA's NixOS configuration but leaves the
sensitive material (the intermediate-CA key passphrase) as a stub. The
actual plaintext lives in the sibling `../priv/` repo, which provides
`myconfig.secrets.<name>.source = ./...;` overrides on top of every
host evaluation.

This file documents *exactly* what the priv repo must contribute so
that step-ca on vserver can boot and Caddy on every host can obtain
ACME certificates from the internal CA.

## Background

* Public-repo module that declares the stub:
  `hosts/host.vserver/services.step-ca.nix`
* Stub declaration:
  ```nix
  myconfig.secrets.step-ca-intermediate-password = {
    dest = "/run/step-ca-intermediate-password";
    # owner/group/mode default to root:root 0400, which is what
    # systemd LoadCredential= expects.
  };
  ```
* Secret framework: `modules/myconfig.secrets.nix`. When `source = null`
  (the public-repo default) the agenix derivation is skipped, a warning
  is emitted, and `step-ca.service` fails to start because its
  `LoadCredential=intermediate_password:/run/step-ca-intermediate-password`
  source file does not exist.

## What the priv repo must contain

### 1. Plaintext passphrase file

Path inside the priv checkout:

```
priv/secrets/step-ca-intermediate-password
```

Contents: the passphrase you chose when you ran `step ca init` on
vserver — written as a single line, **no trailing newline**.

```bash
# In the priv repo checkout, after running `step ca init` on vserver:
printf '%s' '<the-passphrase-from-step-ca-init>' \
  > secrets/step-ca-intermediate-password
chmod 0400 secrets/step-ca-intermediate-password
git add secrets/step-ca-intermediate-password
```

The file is encrypted at Nix eval time using vserver's
`/etc/ssh/ssh_host_rsa_key.pub` (looked up from
`hosts/metadata.json` by `modules/myconfig.secrets.nix:89-110`) and
ends up in vserver's nix store as a per-host `.age` file. The plaintext
never leaves `../priv/`.

### 2. Module wiring in the priv overlay

Whatever mechanism your priv repo uses to feed `moreModules` into
`nixosConfigurationsGen.host-vserver` (see public-repo
`flake.nix:159-260`) needs to set the `source` for this secret on
vserver. The minimal stanza:

```nix
# Somewhere in priv/.../vserver-priv.nix (or similar)
{ ... }:
{
  myconfig.secrets.step-ca-intermediate-password.source =
    ./secrets/step-ca-intermediate-password;
}
```

The path `./secrets/step-ca-intermediate-password` is relative to that
priv module file; adjust to your actual layout.

Once this override is in place:

* `nix eval --raw .#nixosConfigurations.vserver.config.age.secrets.step-ca-intermediate-password.file`
  should return a `/nix/store/<hash>-step-ca-intermediate-password.age`
  path (not throw "missing source").
* `nix flake check` should no longer print the warning
  `myconfig.secrets: source is missing for: step-ca-intermediate-password`
  for vserver.

## Bootstrap sequence (combined: public + priv repos)

The full one-time bootstrap, end to end:

1. **Public repo deploy without the secret.** Run `nixos-rebuild
   switch` on vserver from the public repo (or with the priv overlay
   but no `source =` for this secret yet). `step-ca.service` is
   activated but will fail to start — that is expected.

2. **Initialise the CA on vserver.** Run as the step-ca user
   (`DynamicUser`, so use `machinectl shell` or `runuser`):

   ```bash
   sudo -u step-ca STEPPATH=/var/lib/step-ca \
     step ca init \
       --deployment-type standalone \
       --name "MyConfig Internal CA" \
       --dns "ca.vserver.wg0.maxhbr.local" \
       --address "10.199.199.1:8443" \
       --provisioner acme \
       --acme
   ```

   When prompted, enter a strong passphrase. **Save it** — you'll
   need it in step 4.

3. **Copy the root certificate into the public repo.** The root cert
   is non-secret:

   ```bash
   sudo cat /var/lib/step-ca/certs/root_ca.crt \
     > ~/myconfig/myconfig/modules/myconfig.deployedServices/internalCa/root.crt
   ```

   Commit and push the new `root.crt` in the public repo. Every host
   will pick it up via
   `myconfig.deployedServices.internalCa.enable = true` →
   `security.pki.certificateFiles`.

4. **Add the passphrase to the priv repo.** In the priv repo:

   ```bash
   printf '%s' '<the-passphrase>' \
     > secrets/step-ca-intermediate-password
   chmod 0400 secrets/step-ca-intermediate-password
   ```

   Plus the `source = ./...` override in the priv overlay for vserver,
   as described above. Commit + push.

5. **Re-deploy vserver with both repos.** Now
   `step-ca-intermediate-password-key.service` writes the decrypted
   passphrase to `/run/step-ca-intermediate-password`, step-ca's
   `LoadCredential=` picks it up, and step-ca starts serving the ACME
   directory at `https://ca.vserver.wg0.maxhbr.local:8443/acme/acme/directory`.

6. **Re-deploy every Caddy host.** thing, nuc, p14, vserver. Caddy
   replaces its `tls internal` certs with ACME-issued certs from the
   internal CA. Verify from any host:

   ```bash
   curl --verbose https://forgejo.thing.wg0.maxhbr.local/
   # Should succeed with no TLS warning; the cert chain ends at
   # "MyConfig Internal CA Root" which is in the system trust store.
   ```

## Rotation

* **Intermediate key**: rotate every few years per smallstep's
  recommendations. Procedure:
  https://smallstep.com/docs/step-ca/renewal/. After rotating, update
  the passphrase plaintext in `../priv/secrets/` if the new
  intermediate uses a different passphrase, and re-deploy vserver.

* **Root key**: should outlive the intermediate by a factor of ~10
  (e.g. 10y intermediate, 100y root, or whatever your policy says).
  Root rotation is a forklift event — every host's trust store has to
  pick up the new root.crt. Plan accordingly.

* **Passphrase only**: if you only want to change the passphrase
  without rotating keys, use `step crypto change-pass` against
  `/var/lib/step-ca/secrets/intermediate_ca_key`, then update
  `secrets/step-ca-intermediate-password` in the priv repo.

## Disaster recovery

* The root and intermediate key files (`/var/lib/step-ca/secrets/`)
  must be backed up off-host. **Without them you cannot re-issue
  certificates**, so all clients would have to switch back to
  `useAcmeForCaddy = false` (per-host `tls internal`) and accept new
  warnings until you stand up a fresh CA.

* Recommended backup target: an encrypted volume on a separate host
  (e.g. nas). Either include `/var/lib/step-ca/` in your existing
  backup job or copy `root_ca.crt`, `root_ca_key`, `intermediate_ca.crt`,
  `intermediate_ca_key` out to a safe location.

* The `secrets/step-ca-intermediate-password` plaintext lives in the
  priv repo, which is already replicated across thing + p14 — so the
  passphrase is implicitly backed up there.

## Emergency switch-off

If step-ca is unreachable (vserver down, key corruption, intermediate
expired, …), set on the affected host or globally:

```nix
myconfig.deployedServices.internalCa.useAcmeForCaddy = false;
```

Caddy on that host falls back to `tls internal` (per-host self-signed),
clients see TLS warnings again, but the services keep serving. Revert
when step-ca is healthy again.
