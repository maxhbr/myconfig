{ config, pkgs, ... }:
let
  sink-name = "alsa_output.pci-0000_c1_00.6.analog-stereo";
  sink-card = "1";
  sink-hw = "hw:${sink-card},0";

  sound-file = "${pkgs.sound-theme-freedesktop}/share/sounds/freedesktop/stereo/service-logout.oga";
  pactl-suspend-sink = pkgs.writeShellScriptBin "pactl-suspend-sink" ''
    set +e
    set -x
    ${pkgs.pulseaudio}/bin/pactl suspend-sink ${sink-name} 1
    true
  '';
  pactl-resume-sink = pkgs.writeShellScriptBin "pactl-resume-sink" ''
    set +e
    set -x
    ${pkgs.pulseaudio}/bin/pactl suspend-sink ${sink-name} 0
    true
  '';
  play-suspend-sound = pkgs.writeShellScriptBin "play-suspend-sound" ''
    set +e
    set -x
    ${pkgs.alsa-utils}/bin/amixer -q -c${sink-card} sset Speaker 80%% unmute
    ${pkgs.sox}/bin/sox ${sound-file} -t wav - | ${pkgs.alsa-utils}/bin/aplay -D ${sink-hw} -
    true
  '';
in
{
  environment.systemPackages = [ play-suspend-sound ];
  systemd.services.play-suspend-sound = {
    description = "Play a short sound just before suspend/hibernate";
    before = [ "sleep.target" ];
    wantedBy = [ "sleep.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStartPre = "${pactl-suspend-sink}/bin/pactl-suspend-sink";
      ExecStart = "${play-suspend-sound}/bin/play-suspend-sound";
      ExecStopPost = "${pactl-resume-sink}/bin/pactl-resume-sink";
    };
  };
}
