#!/bin/sh

# Partly from: https://github.com/oblique/create_ap/blob/master/create_ap

if [[ $(id -u) -ne 0 ]]; then
  echo "You must run it as root." >&2
  exit 1
fi

#INTERFACE="wlan1"
# initialize WiFi interface
#ip link set dev ${INTERFACE} address ${NEW_MACADDR}
#ip link set down dev ${INTERFACE}
#ip addr flush ${INTERFACE}

# disable iptables rules for bridged interfaces
echo 0 > /proc/sys/net/bridge/bridge-nf-call-iptables
#ip link set dev br0 up

# boost low-entropy
if [[ $(cat /proc/sys/kernel/random/entropy_avail) -lt 1000 ]]; then
    which haveged > /dev/null 2>&1 && {
        haveged -w 1024 -p $CONFDIR/haveged.pid
    }
fi

#not needed for bridged version:
#dnsmasq -C /etc/dnsmasq.conf

echo "start hostapd"
hostapd -d /etc/hostapd/hostapd.conf #1> /dev/null
