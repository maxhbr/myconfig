{
  pkgs,
  config,
  lib,
  ...
}:
let
  lxdMyInit =
    with pkgs;
    writeScriptBin "lxdMyInit" ''
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
  lxdMyTeardown =
    with pkgs;
    writeScriptBin "lxdMyTeardown" ''
      #!${stdenv.shell}
      echo "##########################################################################"
      sudo ${pkgs.lxd}/bin/lxc list
      echo "lxc delete <whatever came from list>"

      echo "##########################################################################"
      sudo ${pkgs.lxd}/bin/lxc image list
      echo "lxc image delete <whatever came from list>"

      echo "##########################################################################"
      sudo ${pkgs.lxd}/bin/lxc network list
      echo "lxc network delete <whatever came from list>"
      sudo ${pkgs.lxd}/bin/lxc network delete lxdbr0 || true

      echo "##########################################################################"
      echo '{"config": {}}' | sudo lxc profile edit default

      echo "##########################################################################"
      sudo ${pkgs.lxd}/bin/lxc storage volume list default
      echo "lxc storage volume delete default <whatever came from list>"

      echo "##########################################################################"
      sudo ${pkgs.lxd}/bin/lxc storage delete default
    '';
  lxcList =
    with pkgs;
    writeScriptBin "lxcList" ''
      #!${stdenv.shell}
      set -ex
      sudo ${lxd}/bin/lxc list
      sudo ${lxd}/bin/lxc image list
      sudo ${lxd}/bin/lxc network list
      sudo ${lxd}/bin/lxc storage volume list default
    '';

in
{
  config = (
    lib.mkIf config.virtualisation.lxc.enable {
      home-manager.users.mhuber = {
        home.packages = with pkgs; [
          lxc
          lxcList
          lxd
          lxdMyInit
          lxdMyTeardown
        ];
      };
      virtualisation = {
        # lxc.enable = true;
        lxd.enable = true;
      };
    }
  );
}
