#!/usr/bin/env bash

set -ex

read -p 'PublicKeyOfServer: ' pubkey
read -p 'PrivateKey: ' privkey
read -p 'Endpoint: ' endpoint
read -p 'Ip last nr: ' nr

ipspace="10.199.199"
port=51820
wg0conf="/etc/wireguard/wg0.conf"

sudo mkdir -p "$(dirname "$wg0conf")"
cat <<EOF | sudo tee "$wg0conf"
[Interface]
Address = $ipspace.$nr/24
ListenPort = 42223
PrivateKey = $privkey
#PostUp = iptables -A FORWARD -i wg0 -j ACCEPT; iptables -t nat -A POSTROUTING -o eno1 -j MASQUERADE; ip6tables -A FORWARD -i wg0 -j ACCEPT; ip6tables -t nat -A POSTROUTING -o eno1 -j MASQUERADE
#PostDown = iptables -D FORWARD -i wg0 -j ACCEPT; iptables -t nat -D POSTROUTING -o eno1 -j MASQUERADE; ip6tables -D FORWARD -i wg0 -j ACCEPT; ip6tables -t nat -D POSTROUTING -o eno1 -j MASQUERADE

[Peer]
PublicKey = $pubkey
Endpoint = $endpoint:$port
#PresharedKey = [... PRESHARED Key ...]
AllowedIPs = $ipspace.0/24
PersistentKeepalive = 25
EOF
sudo chmod 0600 /etc/wireguard/wg0.conf

