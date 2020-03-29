{ pkgs, ... }: let

  lxdMyInit = with pkgs; writeScriptBin "lxdMyInit" ''
#!${stdenv.shell}
cat <<EOF | sudo ${pkgs.lxd}/bin/lxd init --preseed
config: {}
networks:
- config:
    ipv4.address: auto
    ipv6.address: auto
  description: ""
  managed: false
  name: lxdbr0
  type: ""
storage_pools:
- config:
    size: 100GB
  description: ""
  name: default
  driver: btrfs
profiles:
- config: {}
  description: ""
  devices:
    eth0:
      name: eth0
      nictype: bridged
      parent: lxdbr0
      type: nic
    root:
      path: /
      pool: default
      type: disk
  name: default
cluster: null
EOF
'';
  lxcList = with pkgs; writeScriptBin "lxcList" ''
#!${stdenv.shell}
set -ex
sudo ${lxd}/bin/lxc list
sudo ${lxd}/bin/lxc image list
sudo ${lxd}/bin/lxc network list
sudo ${lxd}/bin/lxc storage volume list default
'';

in {
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        lxc lxcList
        lxd lxdMyInit
      ];
    };
    virtualisation = {
      lxc.enable = true;
      lxd.enable = true;
    };
  };
}
