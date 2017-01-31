{ hostName ? "mobile" }:
let
  baseConfig = {
    networking = {
      # hostId = "123456";
      hostName = hostName;
    };

    nixpkgs.config = import ../nix/nixpkgs-config.nix;

    imports = [
      /etc/nixos/hardware-configuration.nix
    ];
  };

  machineConfig = import ./machines { baseConfig = baseConfig; };

  configuration = { ... }: {
    _module.args.buildVM = true;
    virtualisation.diskSize = 2048;
    virtualisation.memorySize = 1024;
  } // machineConfig;

  build = import <nixpkgs/nixos> { inherit configuration; };
in build.vm

# let
# nixos = import <nixpkgs/nixos> {
#   system = "x86_64-linux";
#   configuration = import ./configuration.nix;
# };
# in { system = nixos.config.system.build.toplevel; }
