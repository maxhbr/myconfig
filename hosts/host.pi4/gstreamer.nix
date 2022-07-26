{ pkgs, config, ... }:
let
  streamcam = with pkgs;
    writeShellScriptBin "streamcam" ''
      ${pkgs.mjpg-streamer}/bin/mjpg_streamer \
        -i "input_uvc.so \
          -d ''${1:-/dev/video0} \
          -rot 0 \
          -f 15 \
          -r ''${2:-1280x720}" \
        -o "output_http.so -w /www -p 32145"
    '';
in {
  home-manager.sharedModules = [{
    home.packages = with pkgs; [
      gst_all_1.gstreamer
      gst_all_1.gst-plugins-bad
      gst_all_1.gst-plugins-base
      gst_all_1.gst-plugins-good
      gst_all_1.gst-plugins-ugly
      gst_all_1.gst-rtsp-server
      mjpg-streamer
      streamcam
    ];
  }];

  networking.firewall.allowedTCPPorts = [ 32145 ];
  networking.firewall.allowedUDPPorts = [ 32145 ];
  boot.loader.raspberryPi.firmwareConfig = ''
    start_x=1
    gpu_mem=128
  '';
  boot.kernelModules = [ "bcm2835-v4l2" ];
}
