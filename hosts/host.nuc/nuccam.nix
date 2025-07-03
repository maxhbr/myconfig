{ pkgs, ... }:
let
  streamcam =
    with pkgs;
    writeShellScriptBin "streamcam" ''
      ${pkgs.mjpg-streamer}/bin/mjpg_streamer \
        -i "input_uvc.so \
          -d /dev/video0 \
          -f 30 \
          -r 1920x1080" \
        -o "output_http.so -w /www -p 32145"
    '';
in
{
  home-manager.users.mhuber = {
    home.packages = [ streamcam ];
  };

  networking.firewall.allowedTCPPorts = [ 32145 ];
  networking.firewall.allowedUDPPorts = [ 32145 ];

  systemd.services.cam-stream-server = {
    description = "CamStreamServer";
    enable = true;
    serviceConfig = {
      User = "mhuber";
      Type = "simple";
      ExecStart = "${streamcam}/bin/streamcam";
      ExecStop = "${pkgs.procps}/bin/pkill mjpg_streamer";
      Restart = "always";
    };
    after = [ "multi-user.target" ];
    wantedBy = [ "multi-user.target" ];
  };
}
