# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# step-ca — the internal Certificate Authority for the wg0 mesh.
#
# step-ca is the smallstep CA daemon. It exposes an ACME directory at
# `https://${caFqdn}:${caPort}/acme/acme/directory` over the WireGuard
# interface only, so Caddy on every peer can request short-lived
# certificates via ACME instead of using `tls internal` (Caddy's own
# per-host self-signed CA).
#
# CA key material is generated offline in the priv repo via
# `scripts/priv.template/mk_step-ca_keys.sh` (run from the priv repo
# root) and deployed to vserver as four agenix-managed secrets:
#
#   * step-ca-intermediate-password
#       Passphrase for the intermediate key, consumed by the upstream
#       step-ca module via systemd LoadCredential=.
#   * step-ca-root-ca-crt
#       PEM-encoded root certificate. Public material, but kept under
#       agenix anyway so step-ca finds it at a stable in-repo path.
#   * step-ca-intermediate-ca-crt
#       PEM-encoded intermediate certificate. Same rationale.
#   * step-ca-intermediate-ca-key
#       Encrypted PEM-encoded intermediate private key.
#
# In the public repo each of these is declared as a *stub* — `source`
# is null, so the agenix derivation is skipped and a warning is
# emitted. The priv overlay supplies the actual `source = ...` paths.
# Until that happens, step-ca will fail to start (expected) but the
# rest of the system still evaluates and boots.
#
# State (BadgerDB, db tx log, etc.) lives in `/var/lib/step-ca`. The
# upstream NixOS module uses `DynamicUser = true` with
# `StateDirectory = "step-ca"`, so the state is owned by the step-ca
# user (which is also declared statically by the upstream module, so
# the DynamicUser flag is effectively a no-op here). vserver does not
# use impermanence, so this state persists across reboots
# automatically.
#
# Bootstrap (one-time, manual):
#   1. In the priv repo checkout, run:
#        ./mk_step-ca_keys.sh vserver
#      This generates root/intermediate keys, certs, and passphrases
#      under hosts/host.vserver/secrets/step-ca/, and copies root_ca.crt
#      into the public repo's `modules/myconfig.deployedServices/
#      internalCa/root.crt`.
#   2. In the priv overlay, set the `source = ./...` paths for all four
#      `myconfig.secrets.step-ca-*` entries (see
#      scripts/priv.template/mk_step-ca_keys.sh.README.md in the
#      public repo).
#   3. Commit both repos.
#   4. `nixos-rebuild switch` on vserver — step-ca starts, serves
#      `https://${caFqdn}:${caPort}/acme/acme/directory`.
#   5. `nixos-rebuild switch` on every Caddy host — Caddy switches from
#      `tls internal` to ACME against the internal CA.
#
# Operations:
#   * Verify the ACME endpoint from any wg0-connected host:
#       curl https://ca.vserver.wg0.maxhbr.local:8443/acme/acme/directory
#     (the root cert is already in the system trust store)
#   * Rotate the intermediate (recommended every few years):
#       https://smallstep.com/docs/step-ca/renewal/
#   * Inspect the database:
#       /var/lib/step-ca/db/  (BadgerDB)
{
  config,
  lib,
  pkgs,
  myconfig,
  ...
}:
let
  internalCa = config.myconfig.deployedServices.internalCa;

  # step-ca binds to vserver's wg0 IP so only peers on the WireGuard
  # mesh can reach the CA. Public interfaces stay untouched.
  caListenIp = myconfig.metadatalib.getWgIp config.networking.hostName;
  caPort = internalCa.caPort;
  caFqdn = internalCa.caFqdn;

  # State directory used for BadgerDB + any runtime artefacts. step-ca
  # writes here, so the path must be writable. ca.crt / intermediate
  # are *not* placed here — they come from agenix (see below).
  stateDir = "/var/lib/step-ca";

  secrets = config.myconfig.secrets;
in
{
  config = {
    services.step-ca = {
      enable = true;

      address = caListenIp;
      port = caPort;

      # Password file for the intermediate CA's private key. The
      # upstream module loads this via systemd LoadCredential, so the
      # file only needs to be readable by root at activation time —
      # which is exactly what `myconfig.secrets` (agenix) guarantees.
      intermediatePasswordFile = secrets.step-ca-intermediate-password.dest;

      settings = {
        # Root / intermediate / key are all supplied via agenix. The
        # decrypted paths are owned by the step-ca user/group (set
        # below in `myconfig.secrets.*`), and step-ca reads them on
        # every start to assemble the issuance chain.
        root = secrets.step-ca-root-ca-crt.dest;
        crt = secrets.step-ca-intermediate-ca-crt.dest;
        key = secrets.step-ca-intermediate-ca-key.dest;

        # `address` here is overridden by services.step-ca.{address,port}
        # per the upstream module (see nixos/modules/services/security/
        # step-ca.nix). Set anyway for clarity.
        address = "${caListenIp}:${toString caPort}";

        # SANs the CA's own server certificate (issued from the
        # intermediate, served on the ACME directory endpoint) is
        # valid for. Must include the FQDN that Caddy peers connect to.
        dnsNames = [
          caFqdn
          "${config.networking.hostName}.wg0.maxhbr.local"
          caListenIp
        ];

        logger = {
          format = "text";
        };

        db = {
          type = "badgerv2";
          dataSource = "${stateDir}/db";
        };

        authority = {
          claims = {
            # Default leaf lifetime — long enough that a brief step-ca
            # outage doesn't immediately break clients, short enough
            # that revocation via expiry is meaningful. Caddy's auto-
            # renewal aims for ~1/3 of the lifetime remaining, so with
            # 30d certs Caddy renews around the 20d-remaining mark.
            minTLSCertDuration = "5m";
            maxTLSCertDuration = "720h"; # 30 days
            defaultTLSCertDuration = "720h"; # 30 days
            disableRenewal = false;
            allowRenewalAfterExpiry = false;
          };

          # Single ACME provisioner used by all Caddy peers. No external
          # account binding (EAB) — every wg0 peer is implicitly trusted
          # by virtue of being on the WireGuard mesh.
          provisioners = [
            {
              type = "ACME";
              name = "acme";
            }
          ];
        };

        tls = {
          # Reasonable modern defaults; step-ca's hard-coded fallback
          # is broadly equivalent. Explicit to make rotation reviews
          # easier.
          minVersion = 1.2;
          maxVersion = 1.3;
          renegotiation = false;
        };
      };
    };

    # Stub secrets. Public repo declares destination + ownership; the
    # priv overlay supplies `source = ./secrets/step-ca/<name>;` for
    # each. While any `source` is null, the corresponding agenix
    # derivation is skipped (see `validSecrets` in
    # modules/myconfig.secrets.nix:112), a warning is emitted, and
    # step-ca fails to start because settings.{root,crt,key} or
    # LoadCredential= source files are missing — that is the expected
    # state until the priv overlay supplies the material.
    #
    # All four files are owned step-ca:step-ca so the step-ca service
    # can read them directly. (The upstream NixOS module declares
    # users.users.step-ca and users.groups.step-ca statically, which
    # makes the static user the effective service user even with
    # DynamicUser=true on modern systemd.)
    myconfig.secrets = {
      step-ca-intermediate-password = {
        dest = "/run/step-ca-intermediate-password";
        # The upstream module reads this via systemd LoadCredential=
        # (which copies to a credentials dir before step-ca starts),
        # so root:root 0400 (the default) is fine.
      };
      step-ca-root-ca-crt = {
        dest = "/run/step-ca-root-ca-crt";
        owner = "step-ca";
        group = "step-ca";
        permissions = "0444";
      };
      step-ca-intermediate-ca-crt = {
        dest = "/run/step-ca-intermediate-ca-crt";
        owner = "step-ca";
        group = "step-ca";
        permissions = "0444";
      };
      step-ca-intermediate-ca-key = {
        dest = "/run/step-ca-intermediate-ca-key";
        owner = "step-ca";
        group = "step-ca";
        permissions = "0400";
      };
    };

    # Open the CA's port on the WireGuard interface only.
    networking.firewall.interfaces."wg0".allowedTCPPorts = [ caPort ];

    # Publishing `${caFqdn}` → wg-ip is handled centrally in
    # `modules/myconfig.deployedServices/internalCa/default.nix`
    # so every host (including vserver itself) gets the same mapping.

    # Order step-ca after every secret it needs has materialised. The
    # agenix-generated unit name follows the convention
    # `<secret-name>-key.service` (mirrors hosts/host.thing/
    # services.forgejo.nix:107).
    systemd.services.step-ca.serviceConfig.After = [
      "step-ca-intermediate-password-key.service"
      "step-ca-root-ca-crt-key.service"
      "step-ca-intermediate-ca-crt-key.service"
      "step-ca-intermediate-ca-key-key.service"
    ];

    # `step` CLI for day-to-day CA operations on vserver (inspect
    # certs, manage provisioners, ad-hoc signing, …).
    environment.systemPackages = with pkgs; [
      step-cli
      step-ca
    ];
  };
}
