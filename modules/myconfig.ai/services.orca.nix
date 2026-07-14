# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Orca runtime server — headless Linux service.
#
# Packages the Orca AppImage and optionally runs `orca serve` as a
# systemd service under a dedicated system user.  The service is granted
# FUSE device access (DeviceAllow=char-fuse) so the AppImage can mount
# its squashfs payload at runtime.  Orca bundles its own Xvfb auto-start
# (display :99) when no $DISPLAY is set, so no separate Xvfb service is
# needed.  Xvfb is still installed as a runtime dependency because Orca
# looks it up on the $PATH.
{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.myconfig.ai.orca;

  orcaVersion = "1.4.137";

  # Keep the AppImage as-is and grant the systemd service FUSE device
  # access at runtime (DeviceAllow=char-fuse).  This is simpler and
  # more reliable than trying to extract the squashfs payload in the
  # Nix build sandbox (no loop/FUSE devices available there).
  orcaAppImage =
    pkgs.runCommand "orca-${orcaVersion}.AppImage"
      {
        nativeBuildInputs = [ pkgs.makeBinaryWrapper ];
        src = pkgs.fetchurl {
          url = "https://github.com/stablyai/orca/releases/download/v${orcaVersion}/orca-linux.AppImage";
          sha256 = "16693wgcs3sm2wi4id8s1jjnjxn9y6qly2hbcr27cjlklqpi4x4c";
        };
      }
      ''
        cp "$src" "$out"
        chmod +x "$out"

        # Wrap so that Xvfb is on $PATH (Orca auto-starts it on :99 when
        # $DISPLAY is unset) and libfuse2 is on $LD_LIBRARY_PATH
        # (AppImage runtime dependency).
        wrapProgram "$out" \
          --prefix PATH : "${lib.makeBinPath [ pkgs.xorg.xvfb ]}" \
          --prefix LD_LIBRARY_PATH : "${pkgs.fuse}/lib"
      '';
in
{
  options.myconfig.ai.orca = with lib; {
    enable = mkEnableOption "Orca runtime server package (AppImage + CLI wrappers)";

    service = with lib; {
      enable = mkEnableOption "Orca runtime server systemd service";
    };

    package = mkOption {
      type = types.package;
      default = orcaAppImage;
      defaultText = literalExpression "orcaAppImage";
      description = "Orca AppImage package.";
    };

    port = mkOption {
      type = types.port;
      default = 6768;
      description = "Port for the Orca runtime server.";
    };

    pairingAddress = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = ''
        Address that remote clients should use to reach this Orca
        server.  Passed as `--pairing-address` to `orca serve`.
        Use a LAN, Tailscale, or public hostname for remote access.
      '';
    };

    user = mkOption {
      type = types.str;
      default = "orca";
      description = "System user that runs the Orca service.";
    };

    group = mkOption {
      type = types.str;
      default = "orca";
      description = "System group for the Orca service.";
    };

    dataDir = mkOption {
      type = types.path;
      default = "/var/lib/orca";
      description = "State directory for Orca (runtime data, config).";
    };
  };

  config = lib.mkMerge [
    # --- Package (AppImage + CLI wrappers) ---
    (lib.mkIf (config.myconfig.ai.enable && (cfg.enable || cfg.service.enable)) {
      home-manager.sharedModules = [
        {
          home.packages = with pkgs; [
            (writeShellApplication {
              name = "orca-logs";
              text = ''
                set -euo pipefail
                journalctl --follow --pager-end --unit orca-serve.service
              '';
            })
            (writeShellApplication {
              name = "orca-restart";
              text = ''
                set -euo pipefail
                echo "Restarting Orca..."
                sudo systemctl restart orca-serve.service
                echo "Orca restarted. Check status with: systemctl status orca-serve.service"
              '';
            })
            (writeShellApplication {
              name = "orca-status";
              text = ''
                set -euo pipefail
                systemctl status orca-serve.service --no-pager
              '';
            })
          ];
        }
      ];
    })

    # --- Systemd service ---
    (lib.mkIf (config.myconfig.ai.enable && cfg.service.enable) {
      # --- System user / group ---
      users.groups.${cfg.group} = { };
      users.users.${cfg.user} = {
        isSystemUser = true;
        group = cfg.group;
        createHome = true;
        home = toString cfg.dataDir;
        description = "Orca runtime server";
      };

      # --- State directory ---
      systemd.tmpfiles.rules = [
        "d ${cfg.dataDir} 0750 ${cfg.user} ${cfg.group} - -"
      ];

      # --- Systemd service ---
      systemd.services.orca-serve = {
        description = "Orca runtime server";
        wantedBy = [ "multi-user.target" ];
        after = [ "network-online.target" ];
        wants = [ "network-online.target" ];

        serviceConfig = {
          Type = "simple";
          User = cfg.user;
          Group = cfg.group;
          WorkingDirectory = toString cfg.dataDir;

          # Software rendering — safe on any host (VPS, workstation, etc).
          Environment = [
            "LIBGL_ALWAYS_SOFTWARE=1"
            # Do NOT set DISPLAY; Orca auto-starts Xvfb on :99 when unset.
          ];

          ExecStart = "${cfg.package} serve --port ${toString cfg.port} --pairing-address ${cfg.pairingAddress}";

          Restart = "on-failure";
          RestartSec = 5;

          # Hardening
          ProtectSystem = "full";
          ProtectHome = "read-only";
          PrivateTmp = true;
          StateDirectory = "orca";
          # AppImage needs FUSE to mount its squashfs payload at runtime.
          # Grant char-fuse device access (the fuse device node is created
          # on-demand by the kernel when a process opens /dev/fuse).
          DevicePolicy = "closed";
          DeviceAllow = [ "char-fuse rw" ];
          # Chromium sandbox and Electron runtime need write access to
          # the data directory (e.g. for lock files, GPU data, etc.).
          ReadWritePaths = [ (toString cfg.dataDir) ];
        };
      };
    })
  ];
}
