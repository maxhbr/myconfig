{ lib, metadataOverride ? {} }:
let
  json = builtins.fromJSON (builtins.readFile (./. + "/metadata.json"));
  metadata = lib.recursiveUpdate json metadataOverride;
in
{
  fixIp = hostName: deviceName: {
    networking = rec {
      interfaces."${deviceName}".ipv4.addresses = [{
        address = metadata.hosts."${hostName}".ip4;
        prefixLength = 24;
      }];
      defaultGateway = metadata.networks."${metadata.hosts."${hostName}".network}".defaultGateway;
      nameservers = [ defaultGateway "8.8.8.8" "8.8.4.4" ];
    };
  };
}
