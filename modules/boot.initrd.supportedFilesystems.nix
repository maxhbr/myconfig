{ ... }:
{
  config = {
    boot.initrd.supportedFilesystems = [
      "btrfs"
      "luks"
    ];
  };
}
