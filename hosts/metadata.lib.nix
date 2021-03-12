{ lib, metadataOverride ? {} }:
let
  json = builtins.fromJSON (builtins.readFile (./. + "/metadata.json"));
  metadata = lib.recursiveUpdate json metadataOverride;
in
{
  fixIp = deviceName: ({config, ...}: let
    hostName = config.networking.hostName;
  in {
    networking = rec {
      interfaces."${deviceName}".ipv4.addresses = [{
        address = metadata.hosts."${hostName}".ip4;
        prefixLength = 24;
      }];
      defaultGateway = metadata.networks."${metadata.hosts."${hostName}".network}".defaultGateway;
      nameservers = [ defaultGateway "8.8.8.8" "8.8.4.4" ];
    };
  });
  setupAsWireguardClient = wgInterface:
    ( let
      wgNetwork = metadata.networks."${wgInterface}";
      wgPeerMetadata = metadata.hosts."${wgNetwork.peer}";
    in { config, lib, pkgs, ... }@args:
      {
        environment.systemPackages = [ pkgs.wireguard ];
        networking.wireguard.interfaces = {
          "${wgInterface}" = {
            ips = [
              (metadata.hosts."${config.networking.hostName}".wireguard."${wgInterface}".ip4 + "/24")
            ]; # Determines the IP address and subnet of the server's end of the tunnel interface.
            privateKeyFile = "/etc/wireguard/wg-private";
            peers = [{
              publicKey = wgPeerMetadata.wireguard."${wgInterface}".pubkey;
              # allowedIPs = [ "0.0.0.0/0" ];
              # Or forward only particular subnets
              allowedIPs = wgNetwork.allowedIPs;
              endpoint = (wgPeerMetadata.ip4 + ":51820");
              persistentKeepalive =
                25; # Send keepalives every 25 seconds. Important to keep NAT tables alive.
            }];
          };
        };
      });
}
