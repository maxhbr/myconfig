{ pkgs, config, inputs, ... }: {
  imports = [ inputs.nixos-hardware.nixosModules.common-gpu-amd ];
  config = {
    hardware.graphics = { enable = true; };
    services.xserver.videoDrivers = [ "amdgpu" ];
  };
}
