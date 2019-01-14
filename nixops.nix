{
  my-machine = { ... }: {
    deployment.targetHost = "localhost";
    imports = [/etc/nixos/hardware-configuration.nix ./nixos];
  };
}
