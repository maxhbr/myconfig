# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  config,
  lib,
  myconfig,
  ...
}:
let
  user = myconfig.user;
  syncthingTunnel =
    with pkgs;
    pkgs.writeShellScriptBin "syncthingTunnel" ''
      set -x
      exec \
          ssh -N \
              -R ''${2:-9384}:localhost:8384 \
              -L ''${2:-9384}:localhost:8384 \
              "''${1}"
    '';

  # Restrict syncthing to the wg0 mesh on hosts that participate in it:
  # bind the sync listener to the wg address only, disable public
  # discovery/relaying, and open the firewall solely on wg0. Hosts
  # without the mesh keep the previous LAN-reachable behavior.
  wg0Enabled =
    lib.attrsets.hasAttrByPath [ "networking" "wireguard" "interfaces" "wg0" "ips" ] config
    && config.networking.wireguard.interfaces.wg0.ips != [ ];
  wgOnly = wg0Enabled && myconfig.metadatalib.hasWg config.networking.hostName;
  wgIp = myconfig.metadatalib.getWgIp config.networking.hostName;
  syncListenPort = 22000;
in
{
  config = lib.mkIf config.services.syncthing.enable {
    services.syncthing = {
      overrideDevices = true;
      overrideFolders = true;
      user = "${user}";
      group = "${user}";
      dataDir = "/home/${user}/syncthing";
      configDir = "/home/${user}/syncthing/.config/syncthing";
      openDefaultPorts = !wgOnly;
      settings = lib.mkIf wgOnly {
        options = {
          listenAddresses = [ "tcp://${wgIp}:${toString syncListenPort}" ];
          globalAnnounceEnabled = false;
          localAnnounceEnabled = false;
          relaysEnabled = false;
          natEnabled = false;
        };
      };
    };
    networking.firewall.interfaces.wg0 = lib.mkIf wgOnly {
      allowedTCPPorts = [ syncListenPort ];
    };
    # The wg address must exist before syncthing binds to it.
    systemd.services.syncthing = {
      after = lib.mkIf wgOnly [ "wireguard-wg0.service" ];
      wants = lib.mkIf wgOnly [ "wireguard-wg0.service" ];
      environment.STNODEFAULTFOLDER = "true"; # Don't create default ~/Sync folder
    };
    home-manager.users."${user}" = {
      home.packages = [ syncthingTunnel ];
    };
    myconfig.persistence.directories =
      let
        folders = lib.mapAttrsToList (
          name: folder:
          if lib.hasPrefix "/home/${user}/" name then lib.removePrefix "/home/${user}/" name else folder.path
        ) config.services.syncthing.settings.folders;
      in
      folders ++ [ config.services.syncthing.configDir ];
  };
}
