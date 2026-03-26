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
    device = "/dev/disk/by-uuid/c0fd2f7c-5a5c-4fb5-a567-070e68cfbef7";
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
