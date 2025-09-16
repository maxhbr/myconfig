{ config, pkgs, ... }:

#
# This file contains configuration specific to running OS as a qemu guest.
#

{
  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/vda";

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.permitRootLogin = "yes";
  users.extraUsers.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEA6NF8iallvQVp22WDkTkyrtvp9eWW6A8YVr+kz4TjGYe7gHzIw+niNltGEFHzD8+v1I2YJ6oXevct1YeS0o9HZyN1Q9qgCgzUFtdOKLv6IedplqoPkcmF0aYet2PkEDo3MlTBckFXPITAMzF8dJSIFo9D8HfdOV0IAdx4O7PtixWKn5y2hMNG0zQPyUecp4pzC6kivAIhyfHilFR61RGL+GPXQ2MWZWFYbAGjyiYJnAmCP3NOTd0jMZEnDkbUvxhMmBYSdETk1rRgm+R4LOzFUGaHqHDLKLX+FIPKcF96hrucXzcWyLbIbEgE98OHlnVYCzRdK8jlqm8tehUc9c9WhQ== vagrant insecure public key"
  ];

  # Vagrant cannot yet handle udev's new predictable interface names.
  # Use the old ethX naming system for the moment.
  # http://www.freedesktop.org/wiki/Software/systemd/PredictableNetworkInterfaceNames/
  networking.usePredictableInterfaceNames = false;

  users.extraUsers.qemu = {
    isNormalUser = true;
    group = "qemu";
    uid = 1001;
    extraGroups = [
      "myconfig"
      "wheel"
    ];
    home = "/home/qemu";
    createHome = true;
    shell = "/run/current-system/sw/bin/zsh";
    password = "dummy";
  };
}
