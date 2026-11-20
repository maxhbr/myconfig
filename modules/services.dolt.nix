# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Dolt SQL server — MySQL-compatible, version-controlled database.
#
# Used as the server-mode backend for bd (beads): bd clients point at this
# server via `BEADS_DOLT_SERVER_HOST` / `BEADS_DOLT_SERVER_PORT` (or
# `.beads/config.yaml` `dolt.host`/`dolt.port`), which also tells bd the
# server is externally managed, so bd never tries to spawn its own. For
# existing embedded projects, migrate with `bd backup init` + `bd backup
# sync`, then `bd init --server` + `bd backup restore --force` against a
# database served here (bd names databases after the project prefix,
# e.g. `myconfig`).
#
# There is no `services.dolt` upstream module; this one is shaped after
# services.qdrant on host.thing: bind to the wg0 address so peers reach
# the server through the WireGuard tunnel, and open the port only on the
# wg0 firewall interface.
#
# dolt sql-server auto-creates a root super user with no password on
# first start (per-database `CREATE USER`/`GRANT` statements can restrict
# access afterwards). Exposure is limited to the wg0 interface by the
# bind address and the firewall rule, and the credentials file
# (`~/.config/beads/credentials`) can still store a password per
# `host:port` if one is set.
{
  config,
  lib,
  pkgs,
  myconfig,
  ...
}:
let
  cfg = config.services.dolt;
in
{
  options.services.dolt = with lib; {
    enable = mkEnableOption "Dolt SQL server (MySQL-compatible, versioned)";

    package = mkPackageOption pkgs "dolt" { };

    host = mkOption {
      type = types.str;
      default = "127.0.0.1";
      example = literalExpression ''myconfig.metadatalib.getWgIp "thing"'';
      description = ''
        Address the server listens on. Use the wg0 address to expose the
        server on the WireGuard tunnel (paired with
        `networking.firewall.interfaces."wg0"`).
      '';
    };

    port = mkOption {
      type = types.port;
      default = 3307;
      description = "TCP port of the MySQL-compatible listener (bd's server-mode default).";
    };

    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/dolt";
      description = ''
        Data directory; each subdirectory becomes one database named after
        its directory (e.g. `<dataDir>/myconfig` for bd's `myconfig`
        project). The directory is created automatically.
      '';
    };

    remotesapiPort = mkOption {
      type = types.types.either types.port (types.enum [ (-1) ]);
      default = -1;
      description = ''
        Port for the remotesapi listener (Dolt's own clone/pull/push
        protocol). -1 disables it; bd does not need it — it talks plain
        MySQL and pushes via git remotes.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.dolt = {
      description = "Dolt SQL server (MySQL-compatible, version-controlled)";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        ExecStart = lib.concatStringsSep " " [
          (lib.getExe' cfg.package "dolt")
          "sql-server"
          "--host=${cfg.host}"
          "--port=${toString cfg.port}"
          "--data-dir=${cfg.dataDir}"
          "--remotesapi-port=${toString cfg.remotesapiPort}"
        ];
        # static user, not DynamicUser: a rotating uid would lose write
        # access to data files copied in during a bd backup restore
        User = "dolt";
        Group = "dolt";
        StateDirectory = "dolt";
        Restart = "on-failure";
      };
    };

    users.users.dolt = {
      isSystemUser = true;
      group = "dolt";
    };
    users.groups.dolt = { };
  };
}
