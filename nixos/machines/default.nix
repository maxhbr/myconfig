{ config, hostName, hostId,
  otherImports ? [],
  ... }:
{
  networking.hostId = "${hostId}";
  networking.hostName = "${hostName}";
  nixpkgs.config = import ../../nix/nixpkgs-config.nix;
  nix.nixPath = [ "nixpkgs=/etc/nix/nixpkgs.nix" "nixos-config=/etc/nixos/configuration.nix" ];
  # nixpkgs.overlays = nixpkgs.config.overlays;
  boot.kernel.sysctl = {
  # "fs.inotify.max_user_watches" = 524288;
  "vm.swappiness" = 1;
  };

  imports = otherImports ++ [
    ../profiles/core
    (../machines + "/${hostName}.nix")
  ];
}
