{ ... }:
{
  config = {
    boot.initrd.supportedFilesystems = [
      "btrfs"
      "bcachefs"
      "luks"
    ];
  };
}
