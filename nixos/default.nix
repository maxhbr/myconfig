let
  hostId = "123456";
  hostName = hostName;

  # machineConfig = import ./machines {
  #   inherit hostId hostName;
  # };
  machine = import <nixpkgs/nixos> {
    system = "x86_64-linux";
    configuration = import ./machine;
  };
   # config = (import <nixpkgs/nixos/lib/eval-config.nix> {

  configuration = { ... }: machine.config // {
    _module.args.buildVM = true;
    virtualisation.diskSize = 2048;
    virtualisation.memorySize = 1024;
  };

  build = import <nixpkgs/nixos> { configuration = machine.config; };
in build.vm

# let
# nixos = import <nixpkgs/nixos> {
#   system = "x86_64-linux";
#   configuration = import ./configuration.nix;
# };
# in { system = nixos.config.system.build.toplevel; }
