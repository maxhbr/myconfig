{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
{
  imports = [
    (
      {
        lib,
        config,
        pkgs,
        ...
      }:
      {
        config = lib.mkIf (config.specialisation != { }) {
          # Config that should only apply to the default system, not the specialised ones
        };
      }
    )
  ];
  specialisation = {
    wlp-supplicant = {
      inheritParentConfig = true;
      configuration = {
        myconfig.wifi.backend = "wpa_supplicant";
      };
    };
    iwd = {
      inheritParentConfig = true;
      configuration = {
        myconfig.wifi.backend = "iwd";
      };
    };
    no-psr-workaround = {
      inheritParentConfig = true;
      configuration = {
        # Disable the PSR (panel self-refresh) workaround amdgpu.dcdebugmask=0x10
        # to test whether it interferes with s2idle/S0ix suspend.
        # Boot this specialisation from systemd-boot to test suspend without it.
        boot.kernelParams = lib.mkAfter [ "amdgpu.dcdebugmask=0x0" ];
      };
    };
    # gaming = {
    #   inheritParentConfig = true;
    #   configuration = {
    #     programs.steam = {
    #       enable = true;
    #       remotePlay.openFirewall = true;
    #       dedicatedServer.openFirewall = true;
    #     };
    #     programs.appimage.enable = true;
    #     programs.appimage.binfmt = true;
    #     hardware = {
    #         graphics = {
    #             enable = true;
    #             enable32Bit = true;
    #         };

    #         amdgpu.amdvlk = {
    #             enable = true;
    #             support32Bit.enable = true;
    #         };
    #     };
    #   };
    # };
  };
}
