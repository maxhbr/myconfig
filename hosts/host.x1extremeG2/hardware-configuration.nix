{ config, lib, pkgs, flake, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.initrd.availableKernelModules =
    [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "sdhci_pci" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/f6a4c12c-ce7b-4ee1-9ab2-a825a5a594dc";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/A58D-212A";
    fsType = "vfat";
  };

  fileSystems."/home/mhuber/MINE" = {
    device = "/dev/disk/by-uuid/5b43fba0-0f18-4dbc-88cf-aa0f226b7b3f";
    fsType = "ext4";
  };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/1975c0a8-c22e-4428-8336-d17baf300409"; }];

  nix.settings.max-jobs = lib.mkDefault 12;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
