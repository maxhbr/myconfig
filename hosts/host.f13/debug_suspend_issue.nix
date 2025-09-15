{ pkgs, ... }:
{
  config = {
    systemd.services."pre-suspend-dump" = {
      wantedBy = [ "systemd-suspend.service" ];
      before = [ "systemd-suspend.service" ];
      serviceConfig.Type = "oneshot";
      script = ''
        mkdir -p /var/log/suspdump
        cat <<EOF > /var/log/suspdump/pre.txt
        date: $(date)
        /sys/power/mem_sleep: $(cat /sys/power/mem_sleep)
        EOF
        journalctl -b -0 -k > /var/log/suspdump/journal-kernel-pre.txt || true
      '';
    };

    systemd.services."post-resume-dump" = {
      wantedBy = [ "multi-user.target" ];
      after = [ "systemd-suspend.service" ];
      serviceConfig.Type = "oneshot";
      script = ''
        mkdir -p /var/log/suspdump
        cat <<EOF > /var/log/suspdump/post.txt
        date: $(date)
        EOF
        journalctl -b -0 -k > /var/log/suspdump/journal-kernel-post.txt || true
      '';
    };
    environment.sessionVariables = {
      ELECTRON_DISABLE_GPU = "1";
      ELECTRON_ENABLE_GPU = "false";
    };

    # Verbose systemd sleep logs
    systemd.services.systemd-suspend.environment."SYSTEMD_LOG_LEVEL" = "debug";
    systemd.services.systemd-hibernate.environment."SYSTEMD_LOG_LEVEL" = "debug";
    systemd.services.systemd-suspend-then-hibernate.environment."SYSTEMD_LOG_LEVEL" = "debug";

    # Persist journal between boots so we can read last-boot logs after a crash.
    services.journald.extraConfig = "Storage=persistent";

    nixpkgs.overlays = [
      (
        self: super:
        let
          lib = super.lib;

          wrapElectronNoGpu =
            pkg: bins:
            pkg.overrideAttrs (old: {
              nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ super.makeWrapper ];
              # Some packages already have a postFixup; append to it.
              postFixup = (old.postFixup or "") + ''
                for b in ${lib.concatStringsSep " " (map (b: "\"${b}\"") bins)}; do
                  if [ -x "$out/bin/$b" ]; then
                    wrapProgram "$out/bin/$b" --add-flags "--disable-gpu"
                  fi
                done
              '';
            });

        in
        {
          vscode = wrapElectronNoGpu super.vscode [ "code" ];
          cursor = wrapElectronNoGpu super.cursor [ "cursor" ];
          slack = wrapElectronNoGpu super.slack [ "slack" ];
          signal-desktop = wrapElectronNoGpu super.signal-desktop [ "signal-desktop" ];
        }
      )
    ];
  };
}
