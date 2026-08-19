{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "nvme"
    "usb_storage"
    "sd_mod"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  boot.initrd.luks.devices."enc-pv" = {
    device = "/dev/disk/by-uuid/def88204-2784-4ee8-8fa5-82e94105366f";
    allowDiscards = true;
    crypttabExtraOpts = [ "tpm2-device=auto" ];
  };

  fileSystems."/" = {
    device = "/dev/mapper/enc-pv";
    fsType = "btrfs";
    options = [ "subvol=@" ];
  };

  fileSystems."/.snapshots" = {
    device = "/dev/mapper/enc-pv";
    fsType = "btrfs";
    options = [ "subvol=@snapshots" ];
  };

  fileSystems."/nix" = {
    device = "/dev/mapper/enc-pv";
    fsType = "btrfs";
    options = [ "subvol=@nix" ];
  };

  fileSystems."/var/log" = {
    device = "/dev/mapper/enc-pv";
    fsType = "btrfs";
    options = [ "subvol=@log" ];
  };

  fileSystems."/home" = {
    device = "/dev/mapper/enc-pv";
    fsType = "btrfs";
    options = [ "subvol=@home" ];
  };

  fileSystems."/.swapfile" = {
    device = "/dev/mapper/enc-pv";
    fsType = "btrfs";
    options = [ "subvol=@swapfile" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/CA0E-78AC";
    fsType = "vfat";
    options = [
      "fmask=0022"
      "dmask=0022"
    ];
  };

  swapDevices = [ ];

  networking.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
