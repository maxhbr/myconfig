{ config, myconfig, lib, pkgs, ... }:  let
  user = myconfig.user;
in {
  config = {
    myconfig.persistence.impermanence = {
      enable = true;
      btrfs_device = "/dev/disk/by-uuid/78c33ad0-409f-4ea5-9fe0-3050b9561788";
    };
  };
}
