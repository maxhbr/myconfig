{ pkgs, ... }:
{
  config = {
    fileSystems."/export/imgwork" = {
      device = "/dev/disk/by-uuid/0bdd6e1e-036d-4f98-b4bd-0c82c2b90aa2";
      fsType = "ext4";
    };
    services.nfs.server = {
      enable = true;
      exports = ''
        /export       192.168.1.0/24(rw,fsid=0,insecure,no_subtree_check,crossmnt,fsid=0)
        /export/imgwork 192.168.1.0/24(rw,nohide,insecure,no_subtree_check)
      '';
        # /export/data  192.168.1.0/24(rw,nohide,insecure,no_subtree_check)
        # /export/guest 192.168.1.0/24(rw,nohide,insecure,no_subtree_check)
      statdPort = 4000;
      lockdPort = 4001;
    };
    networking.firewall.allowedTCPPorts = [ 2049 111 4000 4001 ];
    networking.firewall.allowedUDPPorts = [ 2049 111 4000 4001 ];
  };
}
