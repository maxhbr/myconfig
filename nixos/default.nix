let
  configuration = { ... }: {

    imports = [ ./configuration.nix ];

    _module.args.buildVM = true;

     # Set VM disk size (in MB)
     virtualisation.diskSize = 2048;

     # Set VM ram amount (in MB)
     virtualisation.memorySize = 1024;

  };
  build = import <nixpkgs/nixos> { inherit configuration; };
in build.vm

# let
# nixos = import <nixpkgs/nixos> {
#   system = "x86_64-linux";
#   configuration = import ./configuration.nix;
# };
# in { system = nixos.config.system.build.toplevel; }
