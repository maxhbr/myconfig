# TODO: this is WIP
{ config, lib, pkgs, ...}:

let
  nixpkgs66601Src = builtins.fetchTarball {
    # latest commit of https://github.com/eadwu/nixpkgs/tree/nvidia/prime-render-offload
    url = "https://github.com/eadwu/nixpkgs/archive/ad9ca07f646a573c67b31d4638c8be8f3e5b2cde.tar.gz";
    sha256 = "1i06z9vm9wq0wjay4hrrnq827arblqhdpbvnlb9yy3ibh9h8cknq";
  };

  nixpkgs66601 = import nixpkgs66601Src {
    config = config.nixpkgs.config;
  };
in

{
  disabledModules = [
    "hardware/video/bumblebee.nix"
    "hardware/video/nvidia.nix"
    "services/x11/xserver.nix"
    "services/x11/desktop-managers/default.nix"
    "services/x11/desktop-managers/gnome3.nix"
    "services/x11/desktop-managers/pantheon.nix"
    "services/x11/desktop-managers/plasma5.nix"
    "services/x11/desktop-managers/xfce.nix"
  ];

  imports = [
    "${nixpkgs66601Src}/nixos/modules/hardware/video/bumblebee.nix"
    "${nixpkgs66601Src}/nixos/modules/hardware/video/nvidia.nix"
    "${nixpkgs66601Src}/nixos/modules/services/x11/xserver.nix"
    # "${nixpkgs66601Src}/services/x11/desktop-managers/default.nix"
  ];

  config = {
    services.xserver.videoDrivers = [ "nvidia" ];
    hardware.nvidia.prime.offload.enable = true;
    hardware.nvidia.prime.nvidiaBusId = "PCI:1:0:0";
    hardware.nvidia.prime.intelBusId = "PCI:0:2:0";
  };
}
