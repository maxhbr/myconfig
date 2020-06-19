# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# see: https://nixos.wiki/wiki/Wireguard

# Creating a keypair is simple:
# $ umask 077
# $ mkdir ~/wireguard-keys
# $ wg genkey > ~/wireguard-keys/private
# $ wg pubkey < ~/wireguard-keys/private > ~/wireguard-keys/public

{ pkgs, ... }:
{
  config = {
    environment.systemPackages = with pkgs; [
      wireguard
    ];
    # enable NAT
    networking.nat.enable = true;
    networking.nat.externalInterface = "ens3";
    networking.nat.internalInterfaces = [ "wg0" ];
    networking.firewall = {
      allowedUDPPorts = [ 51820 51821 ];

     # This allows the wireguard server to route your traffic to the internet and hence be like a VPN
     # For this to work you have to set the dnsserver IP of your router (or dnsserver of choice) in your clients
     extraCommands = ''
      iptables -t nat -A POSTROUTING -s 10.199.199.0/24 -o eth0 -j MASQUERADE
      '';
    };

    networking.wireguard.interfaces = {
      wg0 = {
        ips = [ "10.199.199.1/24" ]; # Determines the IP address and subnet of the server's end of the tunnel interface.
        listenPort = 51820;
        privateKeyFile = "/etc/wireguard/wg-private";
        peers = let
            path = ./peers;
            content = builtins.readDir path;
          in map (n: import (path + ("/" + n)))
                 (builtins.filter (n: builtins.match ".*\\.nix" n != null)
                                  (builtins.attrNames content));
      };
      wg1 = {
        ips = [ "10.199.203.1/24" ];
        listenPort = 51821;
        privateKeyFile = "/etc/wireguard/wg-private";
        peers =
          [ { # T470s
              publicKey = "tkc8XZWOxLKsgG82R17J2DikWXMq5qqCKOjQdWOjuX0=";
              allowedIPs = [ "10.199.203.2/32" ];
            }
            { # NC
              publicKey = "drtF/JL+NJu+EJDMv+j2GAXNA2tbB5gBE6NFBeOeMFA=";
              allowedIPs = [ "10.199.203.3/32" ];
            }
            { # ST
              publicKey = "nl1o+bOTXpr1xNrbUlNeR+ExRDOGNVeHe1Nz7aiU6BI=";
              allowedIPs = [ "10.199.203.4/32" ];
            }
            { # JS
              publicKey = "huIAnrTZwu7++3HsTRNkCSu77lieZQbMWPwURYiTYRY=";
              allowedIPs = [ "10.199.203.5/32" ];
            }
          ];
      };
    };
  };
}
