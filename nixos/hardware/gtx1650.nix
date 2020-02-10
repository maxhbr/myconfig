# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
let
  ##############################################################################
  # Graphics:
  # See also:
  # - https://nixos.wiki/wiki/Nvidia
  # - https://nixos.org/nixos/manual/#sec-x11-graphics-cards-nvidia
  ##############################################################################
  blacklistNouveau = {
    # Blacklist nouveau
    boot.extraModprobeConfig = "install nouveau /run/current-system/sw/bin/false";
    boot.blacklistedKernelModules = ["nouveau"];
  };
  ##############################################################################
  rawNvidiaConf = {...}: blacklistNouveau // {
    services.xserver = {
      videoDrivers = [ "nvidia" ];
      # # screenSection = ''
      # #   Identifier     "Screen1"
      # #   Device         "Device1"
      # #   Monitor        "Monitor1"
      # #   DefaultDepth    24
      # #   Option         "Stereo" "0"
      # #   Option         "nvidiaXineramaInfoOrder" "DFP-2"
      # #   Option         "metamodes" "HDMI-0: nvidia-auto-select +0+0, DP-0: nvidia-auto-select +2560+0"
      # #   Option         "SLI" "Off"
      # #   Option         "MultiGPU" "Off"
      # #   Option         "BaseMosaic" "off"
      # # '';
      # # extraDisplaySettings = ''
      # #   Depth       24
      # # '';
      # # # monitorSection = ''
      # # # '';
      deviceSection = ''
        Option   "Backlight"      "gmux_backlight"
        Option   "RegistryDwords" "EnableBrightnessControl=1"
      '';
      # Identifier     "Device1"
      # Driver         "nvidia"
      # VendorName     "NVIDIA Corporation"
      # BoardName      "GeForce GTX 1650"
      # Option   "NoLogo"         "TRUE"
      # # Option   "DPI"            "96 x 96"
    };
     boot.kernelParams = [
      # "acpi_backlight=vendor"
      "acpi_backlight=native"
      # "acpi_backlight=video"
      # "acpi_backlight=none"
      "nomodeset"
      "video.use_native_backlight=1"
    ];
  };
  ##############################################################################
  rawNouveauConf = {...}: {
    services.xserver.videoDrivers = [ "nouveau" ];
  };
  ##############################################################################
  rawIntelConf = {...}: blacklistNouveau // {
    services.xserver.videoDrivers = [ "intel" ];
  };
  ##############################################################################
  bumblebeeConf = {pkgs, ...}: blacklistNouveau // {
    hardware.bumblebee = {
      enable = true;
      connectDisplay = true;
    };
    environment.systemPackages = with pkgs; [ bumblebee xorg.xf86videointel ];
  };
  ##############################################################################
  bumblebeeNouveauConf = {pkgs, ...}: {
    hardware.bumblebee = {
      enable = true;
      connectDisplay = true;
      driver = "nouveau";
    };
    environment.systemPackages = with pkgs; [ bumblebee xorg.xf86videointel ];
  };
  ##############################################################################
  optimusPrimeConf = {...}: blacklistNouveau // {
    services.xserver.videoDrivers = [ "intel" "nvidia" ];
    hardware.nvidia = {
      optimus_prime = {
        enable = true;
        # sync.enable = true;
        # offload.enable = true; # see: https://github.com/NixOS/nixpkgs/pull/66601

        # Bus ID of the NVIDIA GPU. You can find it using lspci, either under 3D or VGA
        nvidiaBusId = "PCI:1:0:0";
        # Bus ID of the Intel GPU. You can find it using lspci, either under 3D or VGA
        intelBusId = "PCI:0:2:0";
      };
      modesetting.enable = true;
    };
  };
  ##############################################################################

in {
  inherit
    rawIntelConf
    rawNvidiaConf
    rawNouveauConf
    bumblebeeConf
    bumblebeeNouveauConf
    optimusPrimeConf;
  # issue66601PrimeRenderOffload = ./prime-render-offload.nix
}
