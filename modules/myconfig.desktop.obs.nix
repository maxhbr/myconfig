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

  config = (
    lib.mkIf (cfg.desktop.enable && cfg.desktop.obs.enable) {
      home-manager.sharedModules = [
        {
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
                  obs-ndi
                  # droidcam-obs
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
            enableVirtualCamera = true;
          };
          home.packages = with pkgs; [ ndi ];
          myconfig.persistence.directories = [ ".config/obs-studio" ];
        }
      ];
      myconfig.v4l2.enable = true;
    }
  );
}
