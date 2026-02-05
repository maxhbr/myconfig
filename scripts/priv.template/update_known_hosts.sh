#!/usr/bin/env bash

set -ex
hostname="$1"
ip_address=$(cat "$hostname/ip")

ssh-keygen -R ${hostname}
ssh-keygen -R ${ip_address}
ssh-keygen -R ${hostname},${ip_address}
ssh-keyscan -H ${hostname},${ip_address} >> ~/.ssh/known_hosts
ssh-keyscan -H ${ip_address} >> ~/.ssh/known_hosts
ssh-keyscan -H ${hostname} >> ~/.ssh/known_hosts
