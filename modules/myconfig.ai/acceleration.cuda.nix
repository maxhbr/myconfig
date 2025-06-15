{ config, lib, pkgs, ... }:
{
  config = lib.mkIf config.nixpkgs.config.cudaSupport {
    home-manager.sharedModules = [{
      home.packages = with pkgs; [
        nvtopPackages.nvidia
        nvidia-smi
        nvidia-cuda-toolkit
      ];
    }];
  };
}