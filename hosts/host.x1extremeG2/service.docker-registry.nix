{
  pkgs,
  config,
  myconfig,
  ...
}:
let
  serverIP = myconfig.metadatalib.get.hosts."${config.networking.hostName}".wireguard.wg0.ip4;
in
{
  services.dockerRegistry = {
    enable = true;
    enableGarbageCollect = true;
    # listenAddress = serverIp;
  };
}
