# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }: {
  imports = [ ./desktop.X.common ];

  config = {
    environment.systemPackages = with pkgs; [ x11vnc ];
    ## Setup via ssh tunnel:
    # $ ssh -t -L 5900:localhost:5900 $IP 'x11vnc -ncache 10 -unixpw -localhost -display :0'
    ## in other terminal:
    # $ vncviewer -encodings 'copyrect tight zrle hextile' localhost:0

    ## or open ports
    # networking.firewall.allowedUDPPorts = [ 5900 ];
    # networking.firewall.allowedTCPPorts = [ 5900 ];
  };
}
