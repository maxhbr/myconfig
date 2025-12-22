# see:
# - https://github.com/NixOS/nixpkgs/pull/85690
# - https://github.com/colemickens/nixcfg
{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig;
in
{
  options.myconfig = with lib; {
    desktop.obs.enable = mkEnableOption "obs";
  };

  imports = [
    (
      {
        pkgs,
        config,
        lib,
        ...
      }:
      lib.mkIf (cfg.desktop.enable && cfg.desktop.obs.enable) {
        programs.obs-studio = {
          enable = true;
          plugins =
            with pkgs.obs-studio-plugins;
            (
              [
                wlrobs
                obs-backgroundremoval
                obs-pipewire-audio-capture
                obs-gstreamer
                obs-vkcapture
                distroav
              ]
              ++ (lib.optionals
                (
                  config.myconfig.hardware.gpu.variant == "amd"
                  || config.myconfig.hardware.gpu.variant == "amd-no-rocm"
                )
                [
                  obs-vaapi # optional AMD hardware acceleration
                ]
              )
            );
        };
        home-manager.sharedModules = [
          {
            home.packages = with pkgs; [ ndi ];
            myconfig.persistence.directories = [ ".config/obs-studio" ];
          }
        ];
        myconfig.desktop.wayland.launcherCommands = [
          "obs"
        ];
      }
    )
    (
      {
        pkgs,
        config,
        lib,
        ...
      }:
      lib.mkIf (cfg.desktop.enable && cfg.desktop.obs.enable && cfg.v4l2.enable) {
        programs.obs-studio.enableVirtualCamera = true;
        boot.extraModprobeConfig = ''
          options v4l2loopback \
            devices=1 \
            video_nr=10 \
            card_label="OBS Virtual Camera" \
            exclusive_caps=1
        '';
      }
    )
  ];
}
