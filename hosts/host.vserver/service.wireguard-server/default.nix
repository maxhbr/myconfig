# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# see: https://nixos.wiki/wiki/Wireguard

# Creating a keypair is simple:
# $ umask 077
# $ mkdir ~/wireguard-keys
# $ wg genkey > ~/wireguard-keys/private
# $ wg pubkey < ~/wireguard-keys/private > ~/wireguard-keys/public

{ pkgs, ... }: {
  config = {
    environment.systemPackages = with pkgs; [ wireguard-tools ];
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
        ips = [
          "10.199.199.1/24"
        ]; # Determines the IP address and subnet of the server's end of the tunnel interface.
        listenPort = 51820;
        privateKeyFile = "/etc/wireguard/wg-private";
        peers = let
          path = ./peers;
          content = builtins.readDir path;
        in map (n: import (path + ("/" + n)))
        (builtins.filter (n: builtins.match ".*\\.nix" n != null)
          (builtins.attrNames content));
      };
    };
  };
}
