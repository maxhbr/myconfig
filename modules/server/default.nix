{...}: {
  imports = [
    ./service.openssh.nix
    ./service.vsftp.nix
    ./nixos.auto-upgrade.nix
  ];
}
