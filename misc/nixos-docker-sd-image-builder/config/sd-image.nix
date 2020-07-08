{ lib, ... }: {
  imports = [
    ./rpi4
    ../../../modules/service.openssh.nix
    ../../../roles/core.nix
  ];

  networking.hostName = "rpi4";
  networking.hostId = "78acddde";
  networking.networkmanager.enable = lib.mkForce false;

  # bzip2 compression takes loads of time with emulation, skip it. Enable this if you're low
  # on space.
  sdImage.compressImage = false;

  # OpenSSH is forced to have an empty `wantedBy` on the installer system[1], this won't allow it
  # to be automatically started. Override it with the normal value.
  # [1] https://github.com/NixOS/nixpkgs/blob/9e5aa25/nixos/modules/profiles/installation-device.nix#L76
  systemd.services.sshd.wantedBy = lib.mkOverride 40 [ "multi-user.target" ];

  # Enable OpenSSH out of the box.
  services.sshd.enabled = true;
}
