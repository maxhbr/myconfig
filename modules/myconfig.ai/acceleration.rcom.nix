{ config, lib, pkgs, ... }:
{
  config = lib.mkIf config.nixpkgs.config.rocmSupport {
    home-manager.sharedModules = [{
    home.packages = with pkgs; [
      nvtopPackages.amd
        rocmPackages.rocminfo
        rocmPackages.rocm-smi
      ];
    }];
  };
}