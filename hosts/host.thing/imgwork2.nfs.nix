# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ myconfig, ... }:
{
  services.nfs.server = {
    enable = true;
    exports = ''
      /home/${myconfig.user}/imgwork2 192.168.1.0/24(rw,nohide,insecure,no_subtree_check)
    '';
      # /home/${myconfig.user}/imgwork2 10.199.199.0/24(rw,nohide,insecure,no_subtree_check)
  };
  networking.firewall.interfaces."enp191s0" = {
    allowedTCPPorts = [ 111 2049 ];
    allowedUDPPorts = [ 111 2049 ];
  };
  # networking.firewall.interfaces."wg0" = {
  #   allowedTCPPorts = [ 111 2049 ];
  #   allowedUDPPorts = [ 111 2049 ];
  # };
}
