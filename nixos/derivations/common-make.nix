{ machine, pkgs, lib, ... }:
let
  nixConfig = {
    nixpkgs.config = import ../../nix/nixpkgs-config.nix;
  };

  machineConfig = import ../core {
    config = nixConfig;
    hostId = "12ABCDEF";
    hostName = machine;
  };

  authorizedKeys = [ "${builtins.readFile ../../misc/unsafe-keypair.pub}" ];

in {
  services.openssh.permitRootLogin = "yes";
  users.extraUsers.root.initialPassword = lib.mkForce "dummy";
  users.extraUsers.root.openssh.authorizedKeys.keys = authorizedKeys;
  users.extraUsers.mhuber.openssh.authorizedKeys.keys = authorizedKeys;

  environment.etc = {
    myconfig.source = ../../.;
  };
} // machineConfig
