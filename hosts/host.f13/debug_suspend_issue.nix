{ pkgs, ... }:
{
  config = {
    systemd.services."pre-suspend-dump" = {
      wantedBy = [ "systemd-suspend.service" ];
      before = [ "systemd-suspend.service" ];
      serviceConfig.Type = "oneshot";
      script = ''
        mkdir -p /var/log/suspdump
        date > /var/log/suspdump/pre.txt
        cat /sys/power/mem_sleep >> /var/log/suspdump/pre.txt
        dmesg -c > /var/log/suspdump/dmesg-pre.txt || true
      '';
    };

    systemd.services."post-resume-dump" = {
      wantedBy = [ "multi-user.target" ];
      after = [ "systemd-suspend.service" ];
      serviceConfig.Type = "oneshot";
      script = ''
        mkdir -p /var/log/suspdump
        date > /var/log/suspdump/post.txt
        dmesg > /var/log/suspdump/dmesg-post.txt || true
        journalctl -b -0 -k > /var/log/suspdump/journal-kernel.txt || true
      '';
    };
    environment.sessionVariables = {
      ELECTRON_DISABLE_GPU = "1";
      ELECTRON_ENABLE_GPU = "false";
    };
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
        }
      )
    ];
  };
}
