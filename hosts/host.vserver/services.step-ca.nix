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
# State lives in `/var/lib/step-ca`. step-ca's NixOS module uses
# `DynamicUser = true` with `StateDirectory = "step-ca"`, so the state
# is owned by a transient UID but mapped onto a stable on-disk path.
# vserver does not use impermanence, so this state persists across
# reboots automatically.
#
# Bootstrap (one-time, manual):
#   1. Deploy this configuration once. step-ca will not start yet
#      because /var/lib/step-ca/certs/intermediate_ca.crt does not
#      exist; that is expected.
#   2. On vserver, run `step ca init` as the step-ca user (or as root
#      and chown afterwards):
#        sudo -u step-ca STEPPATH=/var/lib/step-ca step ca init \
#          --deployment-type standalone \
#          --name "MyConfig Internal CA" \
#          --dns "ca.vserver.wg0.maxhbr.local" \
#          --address "10.199.199.1:8443" \
#          --provisioner acme \
#          --acme
#      Choose a strong passphrase when prompted. Save it.
#   3. Copy the generated root certificate out:
#        sudo cat /var/lib/step-ca/certs/root_ca.crt
#      and replace `modules/myconfig.deployedServices/internalCa/root.crt`
#      in this repo with it. Commit.
#   4. In the priv repo, save the passphrase as plaintext:
#        echo -n '<the-passphrase>' > ../priv/secrets/step-ca-intermediate-password
#      and add the corresponding
#        myconfig.secrets.step-ca-intermediate-password.source =
#          ./secrets/step-ca-intermediate-password;
#      override in the priv overlay's modules for the vserver host.
#   5. `nixos-rebuild switch` on vserver — step-ca starts, exposes the
#      ACME directory, and Caddy on every host re-issues its certs.
#
# Operations:
#   * Trigger ACME provisioner inspection from any wg0-connected host:
#       curl --cacert /etc/ssl/internal-ca-root.crt \
#         https://ca.vserver.wg0.maxhbr.local:8443/acme/acme/directory
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

  # Paths inside /var/lib/step-ca, populated by `step ca init` on
  # bootstrap. The upstream NixOS module symlinks /etc/smallstep/ca.json
  # to our generated config, and step-ca reads root/crt/key relative to
  # those absolute paths.
  stateDir = "/var/lib/step-ca";
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
      #
      # NOTE: the secret is declared as a stub below (source = null).
      # The actual plaintext lives in ../priv/secrets/ and is supplied
      # by the priv overlay; without it, step-ca will fail to start
      # but the rest of the system still evaluates and boots.
      intermediatePasswordFile = config.myconfig.secrets.step-ca-intermediate-password.dest;

      settings = {
        # Root / intermediate / key are populated by `step ca init` and
        # live under the state directory. step-ca reads these on every
        # start to assemble the issuance chain.
        root = "${stateDir}/certs/root_ca.crt";
        crt = "${stateDir}/certs/intermediate_ca.crt";
        key = "${stateDir}/secrets/intermediate_ca_key";

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
              # Force the HTTP-01 challenge: Caddy and step-ca exchange
              # the challenge over the same wg0-only port (80) that
              # `myconfig.deployedServices` already opens. TLS-ALPN-01
              # would also work but HTTP-01 is simpler to debug.
              # (Left at default to allow either; comment retained.)
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

    # Stub secret. Public repo declares the destination + ownership;
    # the priv overlay supplies the `source = ./...` plaintext path.
    # While `source` is null the agenix derivation is skipped (see
    # `validSecrets` in modules/myconfig.secrets.nix:112) and a warning
    # is emitted. step-ca will then fail to start because its
    # `LoadCredential=intermediate_password:...` source file is
    # missing — that is the expected state until the priv overlay
    # supplies the passphrase.
    myconfig.secrets.step-ca-intermediate-password = {
      dest = "/run/step-ca-intermediate-password";
      # step-ca uses DynamicUser + systemd LoadCredential, so the file
      # only needs to be readable by root at unit-start time. Default
      # owner = root, mode = 0400.
    };

    # Open the CA's port on the WireGuard interface only.
    networking.firewall.interfaces."wg0".allowedTCPPorts = [ caPort ];

    # Publishing `${caFqdn}` → wg-ip is handled centrally in
    # `modules/myconfig.deployedServices/internalCa/default.nix`
    # so every host (including vserver itself) gets the same mapping.

    # Order step-ca after the passphrase secret unit has materialised.
    # The agenix-generated unit name follows the convention
    # `<secret-name>-key.service` (mirrors hosts/host.thing/
    # services.forgejo.nix:107).
    systemd.services.step-ca.serviceConfig.After = [
      "step-ca-intermediate-password-key.service"
    ];

    # `step` CLI for bootstrap + day-to-day CA operations on vserver.
    environment.systemPackages = with pkgs; [
      step-cli
      step-ca
    ];
  };
}
