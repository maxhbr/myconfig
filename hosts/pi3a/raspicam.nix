{ pkgs, ... }:
let
  streamcam = with pkgs; writeShellScriptBin "streamcam" ''
${pkgs.mjpg-streamer}/bin/mjpg_streamer \
  -i "input_uvc.so \
  -d /dev/video0 \
  -f 15 \
  -r 1280x720" \
  -o "output_http.so -w /www -p 32145"
'';
in
{ home-manager.users.mhuber =
    { home.packages = [streamcam];
    };

  networking.firewall.allowedTCPPorts = [ 32145 ];
  networking.firewall.allowedUDPPorts = [ 32145 ];

  systemd.user.services.cam-stream-server = {
    description = "CamStreamServer";
    enable = true;
    serviceConfig = {
      User = "mhuber";
      Type = "simple";
      ExecStart = "${streamcam}/bin/streamcam";
      ExecStop = "pkill mjpg_streamer";
      Restart = "always";
    };
    after = [ "multi-user.target" ];
    wantedBy = [ "multi-user.target" ];
  };

  # These two parameters are the important ones to get the
  # camera working. These will be appended to /boot/config.txt.
  boot.loader.raspberryPi.firmwareConfig = ''
    start_x=1
  '';
  boot.kernelModules = [ "bcm2835-v4l2" ];
}
