# see: https://github.com/NixOS/nixpkgs/issues/180175
# NetworkManager-wait-online.service is failing after rebuild
{...}: {
  # udev 250 doesn't reliably reinitialize devices after restart
  systemd.services.systemd-udevd.restartIfChanged = false;
  # or
  # systemd.services.NetworkManager-wait-online.enable = false;
}
