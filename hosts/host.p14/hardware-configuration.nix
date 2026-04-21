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
    "thunderbolt"
    "nvme"
    "usb_storage"
    "sd_mod"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/mapper/enc-pv";
    fsType = "btrfs";
    options = [ "subvol=@" ];
  };

  # Give USB time to enumerate before the key is read.
  boot.initrd.systemd.enable = true;
  boot.initrd.systemd.services.wait-for-usb-key = {
    description = "Give USB time to enumerate before the LUKS key is read";
    wantedBy = [ "systemd-cryptsetup@enc\\x2dpv.service" ];
    before = [ "systemd-cryptsetup@enc\\x2dpv.service" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${pkgs.coreutils}/bin/sleep 2";
    };
  };

  boot.initrd.luks.devices."enc-pv" = {
    device = "/dev/disk/by-uuid/50d4205b-70d1-4836-99e3-6cb568e832bb";
    preLVM = true;
    allowDiscards = true;
    keyFileSize = 4096;
    keyFile = "/dev/disk/by-id/usb-SanDisk_Ultra_Fit_4C530001100720106492-0:0";
  };

  fileSystems."/home" = {
    device = "/dev/mapper/enc-pv";
    fsType = "btrfs";
    options = [ "subvol=@home" ];
  };

  fileSystems."/.snapshots" = {
    device = "/dev/mapper/enc-pv";
    fsType = "btrfs";
    options = [ "subvol=@snapshots" ];
  };

  fileSystems."/.swapfile" = {
    device = "/dev/mapper/enc-pv";
    fsType = "btrfs";
    options = [ "subvol=@swapfile" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/113B-166C";
    fsType = "vfat";
  };

  swapDevices = [
    {
      device = "/.swapfile/swapfile";
      size = 20480;
    }
  ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp0s31f6.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlp0s20f3.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
