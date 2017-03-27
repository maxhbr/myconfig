{ config, hostName, hostId,
  otherImports ? [],
  ... }:

{
  networking.hostId = "${hostId}";
  networking.hostName = "${hostName}";
  nixpkgs.config = import ../../nix/nixpkgs-config.nix;
  boot.kernel.sysctl = {
  # "fs.inotify.max_user_watches" = 524288;
  "vm.swappiness" = 1;
  };

  imports = otherImports ++ [
    ../profiles/core
    (../machines + "/${hostName}.nix")
  ];
}
