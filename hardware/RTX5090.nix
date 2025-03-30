{ config, pkgs, lib, myconfig, inputs, ... }: {
  imports = [
    # inputs.nixos-hardware.nixosModules.common-gpu-nvidia
  ];
  config = {
    users.extraUsers."${myconfig.user}".extraGroups = ["nvidia"];
    virtualisation.podman.enableNvidia = true;
    hardware.graphics = { enable = true; };
    services.xserver.videoDrivers = [ "nvidia" ];
    hardware.nvidia = {
      modesetting.enable = true;

      # Nvidia power management. Experimental, and can cause sleep/suspend to fail.
      # Enable this if you have graphical corruption issues or application crashes after waking
      # up from sleep. This fixes it by saving the entire VRAM memory to /tmp/ instead 
      # of just the bare essentials.
      powerManagement.enable = false;

      # Fine-grained power management. Turns off GPU when not in use.
      # Experimental and only works on modern Nvidia GPUs (Turing or newer).
      powerManagement.finegrained = false;

      open = true;
      nvidiaSettings = true;
      package = config.boot.kernelPackages.nvidiaPackages.beta;
    };
  };
}
