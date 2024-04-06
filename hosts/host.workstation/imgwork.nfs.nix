{ pkgs, ... }:
{
  imports = [{
    # Many clients only support NFSv3, which requires the server to have fixed ports:
    services.nfs.server = {
      # fixed rpc.statd port; for firewall
      lockdPort = 4001;
      mountdPort = 4002;
      statdPort = 4000;
      extraNfsdConfig = '''';
    };
    networking.firewall = {
      enable = true;
        # for NFSv3; view with `rpcinfo -p`
      allowedTCPPorts = [ 111  2049 4000 4001 4002 20048 ];
      allowedUDPPorts = [ 111 2049 4000 4001  4002 20048 ];
    };
  }];
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
    };
  };
}


