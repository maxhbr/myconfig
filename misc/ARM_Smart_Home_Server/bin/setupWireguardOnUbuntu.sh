#!/usr/bin/env bash

set -ex

read -p 'PublicKeyOfServer: ' pubkey
read -p 'PrivateKey: ' privkey
read -p 'Endpoint: ' endpoint
read -p 'Ip last nr: ' nr

sudo mkdir -p /etc/network/interfaces.d
cat <<EOF | sudo tee /etc/network/interfaces.d/wg0.conf
auto wg0
iface wg0 inet static
        address 10.199.199.$nr
        netmask 255.255.255.0
        pre-up ip link add wg0 type wireguard
        pre-up wg setconf wg0 /etc/wireguard/wg0.conf
        up ip link set wg0 up
        post-up ip route add 10.199.199.0/24 via 10.199.199.1 dev wg0
        down ip link delete wg0
EOF

sudo mkdir -p /etc/wireguard
cat <<EOF | sudo tee /etc/wireguard/wg0.conf
[Interface]
PrivateKey = $privkey
ListenPort = 51820

[Peer]
PublicKey = $pubkey
Endpoint = $endpoint:51820
AllowedIPs = 10.199.199.0/24
EOF
sudo chmod 0600 /etc/wireguard/wg0.conf

