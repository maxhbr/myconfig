{ config, hostName, hostId,
  otherImports ? [],
  ... }:
{
  networking.hostId = "${hostId}";
  networking.hostName = "${hostName}";
  nixpkgs.config = import ../../nix/nixpkgs-config.nix;
  nix.nixPath = [
    "nixpkgs=http://nixos.org/channels/nixos-17.03/nixexprs.tar.xz"
    "unstable=http://nixos.org/channels/nixos-unstable/nixexprs.tar.xz"
    "nixpkgs-overlays=/etc/nix/overlays"
    "nixos-config=/etc/nixos/configuration.nix"
  ];
  # nixpkgs.overlays = nixpkgs.config.overlays;
  boot.kernel.sysctl = {
  # "fs.inotify.max_user_watches" = 524288;
  "vm.swappiness" = 1;
  };

  imports = otherImports ++ [
    ../core
    ../roles
    (../machines + "/${hostName}.nix")
  ];
}
