{ pkgs, config, lib, ... }:
let
  streamcam = with pkgs;
    writeShellScriptBin "streamcam" ''
      set -euo pipefail
      set -x
      ${pkgs.mjpg-streamer}/bin/mjpg_streamer \
        -i "input_uvc.so -d ''${1:-/dev/video0} -f 15 -r 1920x1080" \
        -o "output_http.so -w /www -p ''${2:-3215} -l 0.0.0.0"
    '';
in {
  home-manager.sharedModules = [{
    home.packages = [ streamcam pkgs.mjpg-streamer pkgs.motion ];
    home.file = {
      ".motion/motion.conf".text = ''
        stream_port 3215
        stream_localhost off

        webcontrol_port 3216
        webcontrol_localhost off
      '';
      ".motion/camera0.conf".text = ''
        video_device /dev/video0
        stream_port 3217
      '';
    };
  }];

  networking.firewall.allowedTCPPorts = [ 3215 3216 3217 ];
  networking.firewall.allowedUDPPorts = [ 3215 3216 3217 ];

  # systemd.services.cam-stream-server = {
  #   description = "CamStreamServer";
  #   enable = true;
  #   serviceConfig = {
  #     User = "mhuber";
  #     Type = "simple";
  #     ExecStart = "${streamcam}/bin/streamcam";
  #     ExecStop = "${pkgs.procps}/bin/pkill mjpg_streamer";
  #     Restart = "always";
  #   };
  #   after = [ "multi-user.target" ];
  #   wantedBy = [ "multi-user.target" ];
  # };
}
