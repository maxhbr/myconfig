{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
{
  config = {
    myconfig.hardware.gpu.variant = "nvidia";
    hardware.nvidia = {
      package = config.boot.kernelPackages.nvidiaPackages.beta; # latest or stable
      open = true;
      nvidiaSettings = true;
      modesetting.enable = true;
      powerManagement = {
        # Nvidia power management. Experimental, and can cause sleep/suspend to fail.
        # Enable this if you have graphical corruption issues or application crashes after waking
        # up from sleep. This fixes it by saving the entire VRAM memory to /tmp/ instead
        # of just the bare essentials.
        enable = false;
        # Fine-grained power management. Turns off GPU when not in use.
        # Experimental and only works on modern Nvidia GPUs (Turing or newer).
        finegrained = false;
      };
    };
  };
}
