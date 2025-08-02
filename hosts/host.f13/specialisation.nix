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
    ({ lib, config, pkgs, ... }: {
      config = lib.mkIf (config.specialisation != {}) {
        # Config that should only apply to the default system, not the specialised ones
      };
    })
  ];
  specialisation = {
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