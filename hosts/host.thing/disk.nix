{
  config,
  myconfig,
  ...
}:
let
  user = myconfig.user;
in
{
  fileSystems."/mnt/disk" = {
    device = "/dev/disk/by-uuid/29baa1b7-3832-4c45-a079-1729f219582d";
    fsType = "ext4";
    options = [
      "auto"
      "nofail"
      "x-systemd.device-timeout=1"
      "users"
      "rw"
      "discard"
      "noatime"
    ];
  };
}
