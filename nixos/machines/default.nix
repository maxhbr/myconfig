{ config ? { networking.hostName = "vbox"; networking.hostId = "123456"; },
  ... }:

config // {
  imports = [
    ../profiles/core
    (../machines + "/${config.networking.hostName}.nix")
  ];
}
