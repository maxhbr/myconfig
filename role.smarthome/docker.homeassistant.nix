{ pkgs, config, ... }: {
  config = {
    networking.firewall.allowedTCPPorts = [ 8123 ];
    networking.firewall.allowedUDPPorts = [ 8123 ];

    users.users.hass = {
      home = "/var/lib/config";
      createHome = true;
      group = "hass";
      extraGroups = [ "dialout" ];
      uid = config.ids.uids.hass;
    };

    docker-containers = {
      homeassistant = {
        image = "homeassistant/home-assistant:stable";
        # args = ;
        # entrypoint = ;
        volumes = [
          "/var/lib/config:/config"
          "/etc/localtime:/etc/localtime:ro"
          # "/etc/timezone:/etc/timezone:ro"
        ];
        extraDockerOptions = [
          "--net=host"
          "-e"
          "USER_ID=${toString config.ids.uids.hass}"
          "-e"
          "GROUP_ID=${toString config.ids.uids.hass}"
        ];
      };
    };
  };
}
