# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Prometheus exporter for the TFA Dostmann AirCO2NTROL Mini
# (USB VID:PID 04d9:a052), based on
# https://github.com/huhamhire/air-co2-exporter — a small Go binary
# that uses libusb (via gousb) to read CO2 ppm + ambient temperature
# from the device and serves them at /metrics.
#
# Originally requested as the Node.js project
# https://github.com/huhamhire/co2-monitor-exporter ; we use the
# same author's Go port instead, because it ships a proper go.mod /
# go.sum (so buildGoModule packages it cleanly) and produces a single
# static-ish binary with no JS/npm machinery to maintain. The metric
# names (`air_co2`, `air_temp`) are unchanged from the Node.js
# original, so the Grafana dashboard and any external queries remain
# the same.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.observability;
  clientCfg = cfg.client;
  co2Cfg = clientCfg.co2Exporter;

  # USB IDs of the supported device. Hard-coded in the upstream Go
  # source as well; exposed as Nix options only so the udev rule can
  # be kept in sync without grepping. See:
  # https://github.com/huhamhire/air-co2-exporter/blob/master/monitor/device.go
  vendorId = "04d9";
  productId = "a052";

  air-co2-exporter = pkgs.buildGoModule rec {
    pname = "air-co2-exporter";
    version = "0.3.0-unstable-2020-10-26";

    src = pkgs.fetchFromGitHub {
      owner = "huhamhire";
      repo = "air-co2-exporter";
      rev = "a72d98ed3f82d3b246a3b4c529b0f4ee70e65132";
      hash = "sha256-3pGESMN1k+wnXqC7NN9XK1eA5xbaaW6lucxz8knPFmA=";
    };

    # The repo's go.mod declares `module co2-exporter` (no domain
    # prefix), so buildGoModule resolves the main package as
    # "co2-exporter" rather than the GitHub path.
    subPackages = [ "." ];

    # Pinned to the hash reported by Nix when this derivation is first
    # evaluated against the upstream go.sum. Refresh whenever `rev` /
    # `hash` change above (run a build with `vendorHash = lib.fakeHash`
    # and copy the "got:" line from the mismatch error).
    vendorHash = "sha256-zhCoxTiWEShderg0vjRsxlu/Fxh+A2q4zHnhKFSFcNo=";

    # gousb -> cgo -> libusb-1.0
    nativeBuildInputs = [ pkgs.pkg-config ];
    buildInputs = [ pkgs.libusb1 ];

    # Tests poll real hardware over USB; cannot run in the sandbox.
    doCheck = false;

    # The binary is built as `co2-exporter` (from the module name) but
    # the upstream tooling / README all reference `air_co2_exporter`.
    # Keep both names available for clarity.
    postInstall = ''
      ln -s $out/bin/co2-exporter $out/bin/air_co2_exporter
    '';

    meta = with lib; {
      description = "Prometheus exporter for the TFA Dostmann AirCO2NTROL CO2 monitor";
      homepage = "https://github.com/huhamhire/air-co2-exporter";
      license = licenses.mit;
      platforms = platforms.linux;
      mainProgram = "co2-exporter";
    };
  };
  # Stable /dev symlink created by the udev rule below. Checking a
  # fixed path (instead of the dynamic /dev/bus/usb/<bus>/<dev> node)
  # lets the service's ExecCondition cheaply detect whether the sensor
  # is plugged in.
  deviceLink = "/dev/air-co2ntrol";

  # Runs before ExecStart. If the sensor is not connected we log a
  # warning and exit 1, which systemd treats as a *skipped* (not
  # failed) start — so a crash loop is avoided and `nixos-rebuild
  # switch` does not block/fail on a missing device. Exit 0 proceeds
  # to ExecStart. See systemd.service(5) ExecCondition semantics
  # (exit 1-254 = clean skip, 255+ = failure).
  deviceCheck = pkgs.writeShellScript "air-co2-exporter-device-check" ''
    if [ -e "${deviceLink}" ]; then
      exit 0
    fi
    echo "air-co2-exporter: AirCO2NTROL USB device (${vendorId}:${productId}) not connected at ${deviceLink}; skipping start, will retry when the device appears." >&2
    exit 1
  '';
in
{
  options.myconfig.observability.client.co2Exporter = with lib; {
    enable = mkEnableOption ''
      Prometheus exporter for a TFA Dostmann AirCO2NTROL Mini (USB
      ${vendorId}:${productId}) connected to this host.  Reads CO2
      concentration (ppm) and ambient temperature (°C) via libusb and
      exposes them on a loopback HTTP endpoint, scraped by the local
      vmagent.
    '';

    port = mkOption {
      type = types.port;
      default = 9101;
      description = ''
        Port the co2 exporter listens on (loopback only). Matches the
        Node.js predecessor's default; not in the prometheus.io
        exporter port allocation list, but loopback-binding means no
        conflicts on the LAN.
      '';
    };

    tag = mkOption {
      type = types.str;
      default = "default";
      description = ''
        Value of the `tag` label attached to every `air_co2{,_temp}`
        sample. Useful if you ever connect more than one CO2 meter to
        the same host (e.g. "office", "bedroom").
      '';
    };

    room = mkOption {
      type = types.str;
      default = "";
      example = "Schlafzimmer";
      description = ''
        Human-readable name of the room this host's CO2 sensor sits
        in (e.g. "Schlafzimmer"). When non-empty it is attached as a
        `room` label to every sample of the `co2` scrape job — i.e.
        to `air_co2`, `air_temp` *and* `up{job="co2"}` — so the
        Grafana dashboards can label panels by room instead of by
        hostname.

        The label is set as a static target label in the host-local
        vmagent scrape config rather than via the exporter's own
        `--label.*` flags, because upstream only supports the single
        hard-coded `tag` label.

        Empty (the default) means no `room` label is emitted at all;
        the dashboards then fall back to the `host` label.
      '';
    };

    user = mkOption {
      type = types.str;
      default = "co2-exporter";
      description = "System user the exporter runs as.";
    };

    group = mkOption {
      type = types.str;
      default = "co2-exporter";
      description = ''
        System group the exporter runs as. The same group is granted
        read/write access to the AirCO2NTROL USB device via udev.
      '';
    };
  };

  config = lib.mkIf (clientCfg.enable && co2Cfg.enable) {
    users.users.${co2Cfg.user} = {
      isSystemUser = true;
      group = co2Cfg.group;
      description = "Prometheus CO2 exporter (TFA Dostmann AirCO2NTROL)";
    };
    users.groups.${co2Cfg.group} = { };

    # Grant the exporter's group access to the AirCO2NTROL USB device.
    # The device is HID-class; gousb opens it via libusb, which needs
    # an unprivileged-accessible /dev/bus/usb node. gousb itself
    # detaches the kernel `usbhid` driver before claiming the
    # interface (DetachKernelDriver — see monitor/device.go upstream),
    # so we do NOT need to match the hidraw subsystem.
    #
    # KERNEL=="*" + ENV{DEVTYPE}=="usb_device" restricts the rule to
    # the per-device node (not the per-interface nodes), which is
    # what /dev/bus/usb/<bus>/<dev> actually corresponds to.
    #
    # SYMLINK creates the stable ${deviceLink} node used by the
    # service's ExecCondition. TAG+="systemd" + SYSTEMD_WANTS make
    # systemd (re)start the exporter the moment the device is plugged
    # in, so no polling is needed for the happy path.
    services.udev.extraRules = ''
      SUBSYSTEM=="usb", ENV{DEVTYPE}=="usb_device", ATTR{idVendor}=="${vendorId}", ATTR{idProduct}=="${productId}", MODE="0660", GROUP="${co2Cfg.group}", TAG+="uaccess", SYMLINK+="${lib.removePrefix "/dev/" deviceLink}", TAG+="systemd", ENV{SYSTEMD_WANTS}+="prometheus-air-co2-exporter.service"
    '';

    # Periodic retry safety net: if the device is present but the
    # udev-triggered start was missed (e.g. rule reload races), this
    # re-attempts the unit every few minutes. When the device is
    # absent the ExecCondition simply skips the start again — cheap
    # and never fails.
    systemd.timers.prometheus-air-co2-exporter = {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnBootSec = "2min";
        OnUnitInactiveSec = "2min";
      };
    };

    systemd.services.prometheus-air-co2-exporter = {
      description = "Prometheus air_co2_exporter (TFA Dostmann AirCO2NTROL)";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      # Disable systemd's start rate limiting: a missing device is a
      # clean ExecCondition skip (not a failure), but repeated
      # restarts while the device is flapping must never push the unit
      # into a permanent `failed` state that would break activation.
      startLimitIntervalSec = 0;
      # The exporter exits with non-zero if the device is unplugged;
      # rather than crash-looping forever we let it restart on a 10s
      # backoff and rely on prometheus `up{job="co2"}` to flag the
      # outage. If the device is unplugged entirely, the ExecCondition
      # below turns each (re)start into a clean skip instead of a
      # failure, so the unit never blocks `nixos-rebuild switch`.
      serviceConfig = {
        # Warn + cleanly skip the start when the sensor is absent.
        ExecCondition = "${deviceCheck}";

        # On `nixos-rebuild switch`, udev reloads its rules but does
        # NOT reapply them to already-plugged devices. Without this
        # ExecStartPre the first start of the service after a rebuild
        # would fail with `libusb: bad access [code -3]` because the
        # device node is still owned `root:root 0664` from before the
        # rule existed. The `+` prefix runs the command as root even
        # though the service itself runs as `co2-exporter`. `--settle`
        # blocks until udev has finished applying the change so the
        # main ExecStart sees the new ownership.
        ExecStartPre = [
          "+${pkgs.systemd}/bin/udevadm trigger --action=change --subsystem-match=usb --attr-match=idVendor=${vendorId} --attr-match=idProduct=${productId}"
          "+${pkgs.systemd}/bin/udevadm settle --timeout=5"
        ];
        ExecStart = lib.concatStringsSep " " [
          (lib.getExe air-co2-exporter)
          "--web.listen-address=127.0.0.1:${toString co2Cfg.port}"
          "--label.tag=${co2Cfg.tag}"
          "--log.level=info"
        ];
        Restart = "on-failure";
        RestartSec = "10s";

        User = co2Cfg.user;
        Group = co2Cfg.group;

        # Hardening — the exporter only needs USB + a TCP listener.
        # We deliberately do NOT use DynamicUser, because the udev
        # rule above grants device access to a stable group name.
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        PrivateNetwork = false;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        RestrictAddressFamilies = [
          "AF_INET"
          "AF_INET6"
          "AF_NETLINK"
          "AF_UNIX"
        ];
        RestrictNamespaces = true;
        LockPersonality = true;
        MemoryDenyWriteExecute = true;
        SystemCallArchitectures = "native";
      };
    };

    services.vmagent = {
      prometheusConfig = {
        scrape_configs = [
          {
            job_name = "co2";
            static_configs = [
              (
                {
                  targets = [ "127.0.0.1:${toString co2Cfg.port}" ];
                }
                # Only emit the label when a room was configured, so
                # unconfigured hosts keep their previous (label-free)
                # time series instead of gaining an empty `room=""`.
                // lib.optionalAttrs (co2Cfg.room != "") { labels.room = co2Cfg.room; }
              )
            ];
          }
        ];
      };
    };
  };
}
