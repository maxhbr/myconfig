# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Internal Certificate Authority for the wg0 / *.wg0.maxhbr.local mesh.
#
# Architecture:
#   * `step-ca` runs on `vserver` (see hosts/host.vserver/services.step-ca.nix),
#     reachable at https://ca.vserver.wg0.maxhbr.local:8443/acme/acme/directory.
#   * The CA's root certificate is committed in-repo as `./root.crt`
#     (public material, no secrecy needed). Every NixOS host trusts it via
#     `security.pki.certificateFiles` once
#     `myconfig.deployedServices.internalCa.enable = true`.
#   * Caddy instances driven by `modules/myconfig.deployedServices`
#     replace `tls internal` with an ACME issuer pointing at step-ca.
#
# Rollout:
#   * `myconfig.deployedServices.internalCa.enable` is true by default —
#     every host trusts the internal root unconditionally. The root.crt
#     is committed; the intermediate key + passphrase live in `../priv/`
#     (see hosts/host.vserver/services.step-ca.nix and
#     myconfig.secrets.step-ca-* below).
#   * `myconfig.deployedServices.internalCa.useAcmeForCaddy` defaults to
#     true and is consumed by `modules/myconfig.deployedServices` to
#     flip the generated Caddy vhosts from `tls internal` to
#     ACME-against-step-ca. Set to false on a host that wants to keep
#     using Caddy's per-host internal CA (e.g. for emergency fallback
#     if step-ca is down).
#
# Initial setup procedure (manual, one-time):
#   1. On vserver, after first boot with services.step-ca enabled, run
#        sudo -u step-ca step ca init --deployment-type standalone \
#          --name "MyConfig Internal CA" \
#          --dns "ca.vserver.wg0.maxhbr.local" \
#          --address "10.199.199.1:8443" \
#          --provisioner acme \
#          --acme
#      then add an ACME provisioner if `step ca init --acme` is missing in
#      your step-cli version:
#        sudo -u step-ca step ca provisioner add acme --type ACME
#   2. Copy out the generated root:
#        sudo cat /var/lib/step-ca/certs/root_ca.crt
#      and replace ./root.crt in this directory with it. Commit the new
#      root.crt.
#   3. Put the intermediate password (used by `step ca init`) into
#      `../priv/secrets/step-ca-intermediate-password` (plaintext). The
#      `myconfig.secrets.step-ca-intermediate-password` stub declared in
#      hosts/host.vserver/services.step-ca.nix will then encrypt it for
#      vserver's host key.
{
  config,
  lib,
  pkgs,
  myconfig,
  ...
}:
let
  cfg = config.myconfig.deployedServices.internalCa;
in
{
  options.myconfig.deployedServices.internalCa = with lib; {
    enable = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Install the internal-CA root certificate at
        `./root.crt` into the system trust store. Defaults to true so
        every NixOS host in the fleet automatically trusts certificates
        issued by the step-ca running on vserver.
      '';
    };

    rootCert = mkOption {
      type = types.path;
      default = ./root.crt;
      defaultText = lib.literalExpression "./root.crt";
      description = ''
        Path to the internal CA's root certificate (PEM, public). The
        committed default is a short-lived placeholder generated before
        step-ca was bootstrapped; replace it with the real
        `/var/lib/step-ca/certs/root_ca.crt` from vserver after first
        running `step ca init`.
      '';
    };

    caHost = mkOption {
      type = types.str;
      default = "vserver";
      description = ''
        Hostname (as in `hosts/metadata.json`) that runs step-ca. Used
        to derive the ACME directory URL and the CA's FQDN.
      '';
    };

    caPort = mkOption {
      type = types.port;
      default = 8443;
      description = ''
        TCP port on which step-ca listens on its wg0 IP.
      '';
    };

    caFqdn = mkOption {
      type = types.str;
      default = "ca.${cfg.caHost}.wg0.maxhbr.local";
      defaultText = lib.literalExpression ''"ca.\${caHost}.wg0.maxhbr.local"'';
      description = ''
        FQDN at which step-ca serves its ACME directory. Must be covered
        by step-ca's leaf certificate (configured in
        `hosts/host.vserver/services.step-ca.nix`).
      '';
    };

    acmeDirectoryUrl = mkOption {
      type = types.str;
      default = "https://${cfg.caFqdn}:${toString cfg.caPort}/acme/acme/directory";
      defaultText = lib.literalExpression ''"https://\${caFqdn}:\${caPort}/acme/acme/directory"'';
      description = ''
        Full URL to the ACME directory endpoint of the internal CA, as
        consumed by Caddy's `tls { issuer acme { dir ... } }` block.
      '';
    };

    useAcmeForCaddy = mkOption {
      type = types.bool;
      default = true;
      description = ''
        When true, `modules/myconfig.deployedServices` emits Caddy
        vhost configs that obtain certificates from the internal step-ca
        via ACME instead of using `tls internal` (Caddy's own per-host
        self-signed CA).

        When false, Caddy falls back to `tls internal`. Useful as a
        local emergency switch if step-ca is unreachable: each Caddy
        host will generate its own self-signed certs again and clients
        will see TLS warnings, but the services keep serving.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    # The root certificate is public material — distributing it via the
    # system trust store is the whole point. `security.pki.certificateFiles`
    # appends to the system CA bundle so that every TLS client on the
    # host (curl, browsers' nss store inheriting it on most distros,
    # Go binaries, etc.) trusts certificates chaining up to this root.
    security.pki.certificateFiles = [ cfg.rootCert ];

    # Publish `${caFqdn}` → wg-ip-of-caHost in /etc/hosts on every
    # host so peers can reach the ACME directory without depending on
    # vserver's dnsmasq being reachable first (chicken-and-egg: Caddy
    # needs DNS to resolve the CA before it can serve anything, so
    # bypass DNS via /etc/hosts).
    networking.extraHosts = ''
      ${myconfig.metadatalib.getWgIp cfg.caHost} ${cfg.caFqdn}
    '';

    # Firefox has its own NSS-based trust store and ignores the system
    # CA bundle by default — so installing the root via
    # `security.pki.certificateFiles` above is not enough on hosts
    # where Firefox is the daily-driver browser. The Firefox Enterprise
    # Policy `Certificates.ImportEnterpriseRoots = true` makes Firefox
    # additionally consult the OS trust store on every startup,
    # picking up our internal CA root without per-profile certificate
    # import. Policies apply globally to all profiles (no per-profile
    # wiring needed) and survive profile resets.
    home-manager.sharedModules = [
      (
        { config, lib, ... }:
        lib.mkIf (config.programs.firefox.enable or false) {
          programs.firefox.policies.Certificates.ImportEnterpriseRoots = true;
        }
      )
    ];
  };
}
