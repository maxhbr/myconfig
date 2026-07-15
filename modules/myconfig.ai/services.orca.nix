# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Orca runtime server — headless Linux service and desktop application.
#
# Packages the Orca AppImage and optionally runs `orca serve` as a
# systemd service under a dedicated system user, and/or exposes the Orca
# Electron GUI as a desktop application (bin/orca + .desktop entry).
#
# The AppImage is kept bit-for-bit (NOT wrapped with makeBinaryWrapper):
# `wrapProgram` on a bare-file `$out` moves the original to a hidden
# `.*-wrapped` sibling of `$out` that is not part of the derivation
# closure, producing a dangling exec path (silent exit 255).  Instead the
# AppImage is launched through `appimage-run`, which extracts the squashfs
# payload (no FUSE / libfuse2 / /dev/fuse needed) and provides the FHS
# runtime the AppImage expects.  Orca bundles its own Xvfb auto-start
# (display :99) when no $DISPLAY is set, so Xvfb is kept on $PATH.
{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.myconfig.ai.orca;

  orcaVersion = "1.4.137";

  # The raw Orca AppImage, with the executable bit set.  This is the
  # canonical package (cfg.package) and is always launched through
  # `appimage-run` (see the desktop launcher and the service ExecStart).
  orcaAppImage =
    pkgs.runCommand "orca-${orcaVersion}.AppImage"
      {
        src = pkgs.fetchurl {
          url = "https://github.com/stablyai/orca/releases/download/v${orcaVersion}/orca-linux.AppImage";
          sha256 = "16693wgcs3sm2wi4id8s1jjnjxn9y6qly2hbcr27cjlklqpi4x4c";
        };
      }
      ''
        cp "$src" "$out"
        chmod +x "$out"
      '';

  # `orca` command: launches the AppImage through appimage-run, with Xvfb
  # on $PATH (Orca auto-starts it on :99 when no $DISPLAY is set, so this
  # also works headless).  Passing `serve ...` through here runs the
  # runtime server instead of the desktop GUI.
  orcaLauncher =
    with pkgs;
    writeShellScriptBin "orca" ''
      set -euo pipefail
      export PATH="${lib.makeBinPath [ xorg.xvfb ]}:$PATH"
      exec ${appimage-run}/bin/appimage-run ${orcaAppImage} "$@"
    '';

  # Desktop application package: the `orca` launcher (for $PATH) plus a
  # .desktop entry so the Orca Electron GUI shows up in application
  # launchers (niri/fuzzel/wofi, etc.).  Exec points at the launcher so
  # the AppImage is always run through appimage-run.
  orcaDesktopPkg =
    let
      desktopEntry = pkgs.writeText "orca.desktop" ''
        [Desktop Entry]
        Type=Application
        Name=Orca
        Comment=Orca AI desktop application
        Exec=${orcaLauncher}/bin/orca
        Terminal=false
        Categories=Development;Utility;
      '';
    in
    pkgs.runCommand "orca-${orcaVersion}-desktop"
      {
        # Reference the AppImage so this derivation rebuilds when it does.
        passthru = { inherit orcaAppImage; };
      }
      ''
        mkdir -p $out/bin $out/share/applications
        ln -s ${orcaLauncher}/bin/orca $out/bin/orca
        cp ${desktopEntry} $out/share/applications/orca.desktop
      '';
in
{
  options.myconfig.ai.orca = with lib; {
    enable = mkEnableOption "Orca package (AppImage + CLI wrappers)";

    service = with lib; {
      enable = mkEnableOption "Orca runtime server systemd service";
    };

    package = mkOption {
      type = types.package;
      default = orcaAppImage;
      defaultText = literalExpression "orcaAppImage";
      description = "Orca AppImage package. Launched via appimage-run.";
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
    # --- Desktop application (bin/orca + .desktop entry) ---
    # Only where a desktop session exists. Provides the Orca Electron
    # GUI as a launchable application (via the .desktop entry) plus an
    # `orca` command on $PATH. The service (`orca serve`) is independent
    # and controlled by `service.enable` below.
    (lib.mkIf (config.myconfig.ai.enable && config.myconfig.desktop.enable && cfg.enable) {
      home-manager.sharedModules = [
        { home.packages = [ orcaDesktopPkg ]; }
      ];
    })

    # --- CLI wrappers (service helpers) ---
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
          # Xvfb is on $PATH because Orca auto-starts it on :99 when
          # $DISPLAY is unset (the service never sets DISPLAY).
          Environment = [
            "LIBGL_ALWAYS_SOFTWARE=1"
            "PATH=${lib.makeBinPath [ pkgs.xorg.xvfb ]}"
          ];

          # Run the AppImage through appimage-run, which extracts the
          # squashfs payload (no FUSE device needed) and provides the
          # FHS runtime the AppImage expects.
          ExecStart = "${pkgs.appimage-run}/bin/appimage-run ${cfg.package} serve --port ${toString cfg.port} --pairing-address ${cfg.pairingAddress}";

          Restart = "on-failure";
          RestartSec = 5;

          # Hardening
          ProtectSystem = "full";
          ProtectHome = "read-only";
          PrivateTmp = true;
          StateDirectory = "orca";
          # No FUSE device needed: appimage-run extracts the payload
          # instead of mounting it.
          DevicePolicy = "closed";
          # Chromium sandbox and Electron runtime need write access to
          # the data directory (e.g. for lock files, GPU data, and the
          # appimage-run extraction cache under $HOME/.cache).
          ReadWritePaths = [ (toString cfg.dataDir) ];
        };
      };
    })
  ];
}
