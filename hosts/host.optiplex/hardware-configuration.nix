# TODO: This is a PLACEHOLDER hardware configuration for the
# Dell OptiPlex 3000 TC (Intel N6005 / 16 GB DDR4 / 256 GB NVMe, built-in
# Wi-Fi, micro / tiny case form factor).
#
# It must be replaced by the output of `nixos-generate-config --root /mnt`
# run on the actual machine. In particular the following need real,
# verified values before this host can boot:
#   - boot.initrd.availableKernelModules (detected storage/Wi-Fi modules)
#   - fileSystems."/" and "/boot" device labels/UUIDs
#
# Do NOT invent disk UUIDs here. The dummy filesystem entries below only
# exist so the configuration evaluates; they will not produce a bootable
# system.
{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  # TODO: replace with modules detected by nixos-generate-config.
  boot.initrd.availableKernelModules = [
    "nvme"
    "usb_storage"
    "sd_mod"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  # WiFi: see ./wifi.nix (chip TODO tracked there).
  #
  # TODO: replace the placeholder labels/UUIDs below with the real values
  # from the target disk (format it with "EFI" / "nixos" labels).
  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/EFI";
    fsType = "vfat";
  };

  swapDevices = [ ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted
  # networking (the default) this is the recommended approach. When using
  # systemd-networkd it's still possible to use this option, but it's
  # recommended to use it in conjunction with explicit per-interface
  # declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
