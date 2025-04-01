# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.initrd.availableKernelModules =
    [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/9f17fd7f-85bd-4c6b-8fdb-deb0c93b9a72";
    fsType = "btrfs";
    options = [ "subvol=@" ];
  };

  boot.initrd.luks.devices."enc-pv" = {
    device = "/dev/disk/by-uuid/bb6616ae-c6b2-42d9-ac32-9955be35d3b5";
    keyFile = "/dev/disk/by-id/mmc-SDC_0x14022ef6";
    keyFileSize = 4096;
    preLVM = true;
    allowDiscards = true;
    fallbackToPassword = true;
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/9f17fd7f-85bd-4c6b-8fdb-deb0c93b9a72";
    fsType = "btrfs";
    options = [ "subvol=@home" ];
  };

  fileSystems."/.snapshots" = {
    device = "/dev/disk/by-uuid/9f17fd7f-85bd-4c6b-8fdb-deb0c93b9a72";
    fsType = "btrfs";
    options = [ "subvol=@snapshots" ];
  };

  fileSystems."/.swapfile" = {
    device = "/dev/disk/by-uuid/9f17fd7f-85bd-4c6b-8fdb-deb0c93b9a72";
    fsType = "btrfs";
    options = [ "subvol=@swapfile" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/12CE-A600";
    fsType = "vfat";
    options = [ "fmask=0022" "dmask=0022" ];
  };

  swapDevices = [ ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp0s13f0u3u2.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlp0s20f3.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;
}
