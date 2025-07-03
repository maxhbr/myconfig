{
  pkgs,
  config,
  inputs,
  ...
}:
{
  imports = [ inputs.nixos-hardware.nixosModules.common-gpu-amd ];
  config = {
    hardware.graphics = {
      enable = true;
    };
    services.xserver.videoDrivers = [ "amdgpu" ];
    nixpkgs.config.rocmSupport = true;
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          nvtopPackages.amd
          rocmPackages.rocminfo
          rocmPackages.rocm-smi
        ];
      }
    ];
  };
}
